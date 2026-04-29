// --- Dispose old WebGL resources safely ---
window.disposeWebGL = (canvasId) => {
    const canvas = document.getElementById(canvasId);
    if (!canvas || !canvas._glContext) return;

    const gl = canvas._glContext;

    // Cancel draw loop
    if (canvas._drawLoopId) {
        cancelAnimationFrame(canvas._drawLoopId);
        canvas._drawLoopId = null;
    }

    // Delete shaders, programs, buffers
    if (canvas._glState) {
        const state = canvas._glState;

        if (state.prog) {
            const attachedShaders = gl.getAttachedShaders(state.prog) || [];
            attachedShaders.forEach(sh => gl.deleteShader(sh));
            gl.deleteProgram(state.prog);
        }

        if (state.faceBuf) gl.deleteBuffer(state.faceBuf);
        if (state.colorBuf) gl.deleteBuffer(state.colorBuf);

        canvas._glState = null;
    }

};

// --- Compile shader helper ---
function compileShader(gl, type, src) {
    const sh = gl.createShader(type);
    if (!sh) return null;
    gl.shaderSource(sh, src);
    gl.compileShader(sh);
    if (!gl.getShaderParameter(sh, gl.COMPILE_STATUS)) {
        console.error("[WebGL] Shader compile error:", gl.getShaderInfoLog(sh));
        return null;
    }
    return sh;
}

// --- Initialize WebGL extruded polygons ---
window.initWebGLExtrudedPolygons = (canvasId, meshes, colors, heights, baseHeights, edgePolygons, centroids, vsSource, fsSource, externalProj, viewLocked) => {
    window.disposeWebGL(canvasId);

    const canvas = document.getElementById(canvasId);
    if (!canvas) return console.error("Canvas not found:", canvasId);
    
    canvas._viewLocked = !!viewLocked;
    canvas._geoData = { meshes, colors, heights, baseHeights, edgePolygons, centroids, externalProj };
    
    // Set explicit size
    canvas.width = canvas.clientWidth || 600;
    canvas.height = canvas.clientHeight || 400;

    let gl = canvas._glContext;
    if (!gl) {
        gl = canvas.getContext("webgl", { alpha: false, antialias: true });
        if (!gl) {
            console.warn("WebGL context not available, retrying in 50ms...");
            setTimeout(() => window.initWebGLExtrudedPolygons(canvasId, meshes, colors, heights, baseHeights, edgePolygons, centroids, vsSource, fsSource, externalProj, viewLocked), 50);
            return;
        }
        canvas._glContext = gl;
    }

    if (canvas._drawLoopId) cancelAnimationFrame(canvas._drawLoopId);

    if (!vsSource || !fsSource) {
        console.error("[WebGL] Missing shader source!");
        return;
    }

    const vs = compileShader(gl, gl.VERTEX_SHADER, vsSource);
    const fs = compileShader(gl, gl.FRAGMENT_SHADER, fsSource);
    if (!vs || !fs) return;

    const prog = gl.createProgram();
    gl.attachShader(prog, vs);
    gl.attachShader(prog, fs);
    gl.linkProgram(prog);
    if (!gl.getProgramParameter(prog, gl.LINK_STATUS)) {
        console.error("Program link failed:", gl.getProgramInfoLog(prog));
        return;
    }
    gl.useProgram(prog);

    const aPos = gl.getAttribLocation(prog, "a_position");
    const aCol = gl.getAttribLocation(prog, "a_color");
    const uProj = gl.getUniformLocation(prog, "u_projection");
    const uView = gl.getUniformLocation(prog, "u_view");

    // --- Compute normalization ---
    let minX = Infinity, maxX = -Infinity;
    let minY = Infinity, maxY = -Infinity;

    meshes.forEach(mesh => mesh.forEach(tri =>
        tri.forEach(([x, y]) => {
            minX = Math.min(minX, x); maxX = Math.max(maxX, x);
            minY = Math.min(minY, y); maxY = Math.max(maxY, y);
        })
    ));

    const cx = (minX === Infinity) ? 0 : (minX + maxX) / 2;
    const cy = (minY === Infinity) ? 0 : (minY + maxY) / 2;
    const maxDim = Math.max(maxX - minX, maxY - minY);
    const scaleXY = (maxDim > 0) ? 2 / maxDim : 1;
    const scaleZ = scaleXY;

    console.log("[WebGL] Normalized center:", cx, cy, "Scale:", scaleXY);

    // --- Prepare face buffers (top + bottom + ground) ---
    const faceVertices = [], faceColors = [];
    
    // Ground Shadow (Fits bounding box with margin)
    const margin = 1.2;
    const shadowColor = [0.95, 0.95, 0.95, 0.8]; // Very light gray
    faceVertices.push(-margin, -margin, -0.01,  margin, -margin, -0.01,  margin, margin, -0.01);
    faceVertices.push(-margin, -margin, -0.01,  margin, margin, -0.01, -margin, margin, -0.01);
    for(let k=0; k<6; k++) faceColors.push(...shadowColor);

    meshes.forEach((tris, i) => {
        const baseColor = (colors && colors[i]) ? colors[i] : [0.8, 0.8, 0.8];
        const height = (heights?.[i] ?? 1.0) * scaleZ;
        const baseH = (baseHeights?.[i] ?? 0.0) * scaleZ;

        tris.forEach(tri => {
            // Top face (Upward normal)
            tri.forEach(([x, y]) => {
                const nx = (x - cx) * scaleXY;
                const ny = (y - cy) * scaleXY;
                const nz = baseH + height;
                const shade = 0.9; // Bright top
                const col = baseColor.map(c => c * shade);
                faceVertices.push(nx, ny, nz);
                faceColors.push(...col, 1.0);
            });

            // Bottom face (Downward normal)
            for (let j = tri.length - 1; j >= 0; j--) {
                const [x, y] = tri[j];
                const nx = (x - cx) * scaleXY;
                const ny = (y - cy) * scaleXY;
                const nz = baseH;
                const shade = 0.4; // Very dark bottom
                const col = baseColor.map(c => c * shade);
                faceVertices.push(nx, ny, nz);
                faceColors.push(...col, 1.0);
            }
        });
    });

    if (edgePolygons?.length) {
        // Light direction for wall shading
        const lightDir = [0.5, 0.3, 0.8];
        const len = Math.sqrt(lightDir[0]*lightDir[0] + lightDir[1]*lightDir[1] + lightDir[2]*lightDir[2]);
        const lx = lightDir[0]/len, ly = lightDir[1]/len, lz = lightDir[2]/len;

        edgePolygons.forEach((poly, i) => {
            const baseColor = (colors && colors[i]) ? colors[i] : [0.5, 0.5, 0.5];
            const height = (heights?.[i] ?? 1.0) * scaleZ;
            const baseH = (baseHeights?.[i] ?? 0.0) * scaleZ;

            for (let j = 0; j < poly.length; j++) {
                const [x1, y1] = poly[j];
                const [x2, y2] = poly[(j + 1) % poly.length];

                const nx1 = (x1 - cx) * scaleXY;
                const ny1 = (y1 - cy) * scaleXY;
                const nx2 = (x2 - cx) * scaleXY;
                const ny2 = (y2 - cy) * scaleXY;

                // Wall normal calculation
                const dx = nx2 - nx1;
                const dy = ny2 - ny1;
                const wallLen = Math.sqrt(dx*dx + dy*dy);
                const wnx = dy / wallLen;  // Outward normal X
                const wny = -dx / wallLen; // Outward normal Y
                
                // Lambertian shading: Ambient (0.6) + Directional (0.4)
                const dot = Math.max(0, wnx * lx + wny * ly);
                const wallShade = 0.5 + 0.4 * dot;
                const col = baseColor.map(c => c * wallShade);

                const wallVerts = [
                    [nx1, ny1, baseH], [nx2, ny2, baseH + height], [nx1, ny1, baseH + height],
                    [nx1, ny1, baseH], [nx2, ny2, baseH], [nx2, ny2, baseH + height]
                ];
                wallVerts.forEach(([vx, vy, vz]) => {
                    faceVertices.push(vx, vy, vz);
                    faceColors.push(...col, 1.0);
                });
            }
        });
    }

    if (faceVertices.length === 0) {
        console.warn("[WebGL] No vertices to draw.");
        return;
    }

    const faceBuf = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, faceBuf);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(faceVertices), gl.STATIC_DRAW);

    const colorBuf = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, colorBuf);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(faceColors), gl.STATIC_DRAW);

    // --- Prepare edge lines ---
    const edgeVertices = [], edgeColors = [];
    if (edgePolygons?.length) {
        edgePolygons.forEach((poly, i) => {
            const edgeColor = [0.7, 0.7, 0.7, 1.0]; // Light grayish edge
            const height = (heights?.[i] ?? 1.0) * scaleZ;
            const baseH = (baseHeights?.[i] ?? 0.0) * scaleZ;

            for (let j = 0; j < poly.length; j++) {
                const [x1, y1] = poly[j];
                const [x2, y2] = poly[(j + 1) % poly.length];

                const nx1 = (x1 - cx) * scaleXY;
                const ny1 = (y1 - cy) * scaleXY;
                const nx2 = (x2 - cx) * scaleXY;
                const ny2 = (y2 - cy) * scaleXY;

                edgeVertices.push(nx1, ny1, baseH + height, nx2, ny2, baseH + height);
                edgeColors.push(...edgeColor, ...edgeColor);
                edgeVertices.push(nx1, ny1, baseH, nx2, ny2, baseH);
                edgeColors.push(...edgeColor, ...edgeColor);
                edgeVertices.push(nx1, ny1, baseH, nx1, ny1, baseH + height);
                edgeVertices.push(nx2, ny2, baseH, nx2, ny2, baseH + height);
                edgeColors.push(...edgeColor, ...edgeColor, ...edgeColor, ...edgeColor);
            }
        });
    }

    const edgeBuf = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, edgeBuf);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(edgeVertices), gl.STATIC_DRAW);

    const edgeColorBuf = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, edgeColorBuf);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(edgeColors), gl.STATIC_DRAW);

    // --- Interaction & camera ---
    if (canvas._cam_rx === undefined) {
        canvas._cam_ry = 0;
        canvas._cam_rx = Math.PI / 6;
        canvas._cam_zoom = 3;
    }
    let dragging = false, lx = 0, ly = 0;
    const init = { rx: Math.PI / 6, ry: 0, zoom: 3 };

    if (!canvas._listenersAdded) {
        canvas.addEventListener("mousedown", e => { if (canvas._viewLocked) return; dragging = true; lx = e.clientX; ly = e.clientY; });
        document.addEventListener("mouseup", () => dragging = false);
        document.addEventListener("mousemove", e => {
            if (!dragging || canvas._viewLocked) return;
            const dx = e.clientX - lx, dy = e.clientY - ly;
            canvas._cam_ry -= dx * 0.01;
            canvas._cam_rx = Math.min(Math.max(canvas._cam_rx + dy * 0.01, 0.01), Math.PI / 2 - 0.01);
            lx = e.clientX; ly = e.clientY;
        });
        canvas.addEventListener("dblclick", () => { 
            if (canvas._viewLocked) return; 
            canvas._cam_rx = init.rx; canvas._cam_ry = init.ry; canvas._cam_zoom = init.zoom; 
        });
        canvas.addEventListener("wheel", e => {
            if (canvas._viewLocked) return;
            e.preventDefault();
            canvas._cam_zoom = Math.min(Math.max(canvas._cam_zoom + e.deltaY * 0.005, 1.5), 10);
        }, { passive: false });

        let prevDist = null;
        canvas.addEventListener("touchstart", e => {
            if (canvas._viewLocked) return;
            if (e.cancelable) e.preventDefault();
            if (e.touches.length === 1) { lx = e.touches[0].clientX; ly = e.touches[0].clientY; }
            else if (e.touches.length === 2) {
                const dx = e.touches[0].clientX - e.touches[1].clientX;
                const dy = e.touches[0].clientY - e.touches[1].clientY;
                prevDist = Math.hypot(dx, dy);
            }
        }, { passive: false });
        canvas.addEventListener("touchmove", e => {
            if (canvas._viewLocked) return;
            if (e.cancelable) e.preventDefault();
            if (e.touches.length === 1) {
                const dx = e.touches[0].clientX - lx;
                const dy = e.touches[0].clientY - ly;
                canvas._cam_ry -= dx * 0.01;
                canvas._cam_rx = Math.min(Math.max(canvas._cam_rx + dy * 0.01, 0.01), Math.PI / 2 - 0.01);
                lx = e.touches[0].clientX; ly = e.touches[0].clientY;
            } else if (e.touches.length === 2) {
                const dx = e.touches[0].clientX - e.touches[1].clientX;
                const dy = e.touches[0].clientY - e.touches[1].clientY;
                const dist = Math.hypot(dx, dy);
                if (prevDist) { 
                    canvas._cam_zoom = Math.min(Math.max(canvas._cam_zoom * (prevDist / dist), 1.5), 10);
                }
                prevDist = dist;
            }
        }, { passive: false });
        canvas.addEventListener("touchend", (e) => {
            if (e.cancelable) e.preventDefault();
            prevDist = null;
        }, { passive: false });

        canvas.addEventListener("webglcontextlost", e => { e.preventDefault(); console.warn("WebGL context lost."); });
        canvas.addEventListener("webglcontextrestored", () => {
            window.initWebGLExtrudedPolygons(canvasId, meshes, colors, heights, baseHeights, edgePolygons, centroids, vsSource, fsSource, externalProj, viewLocked);
        });
        canvas._listenersAdded = true;
    }

    gl.enable(gl.DEPTH_TEST);
    gl.enable(gl.BLEND);
    gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

    // --- Matrix operations ---
    const mat4 = {
        create: () => new Float32Array([1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1]),
        lookAt: (out, eye, target, up) => {
            let [ex, ey, ez] = eye, [tx, ty, tz] = target, [ux, uy, uz] = up;
            let zx = ex - tx, zy = ey - ty, zz = ez - tz;
            let len = Math.hypot(zx, zy, zz); zx /= len; zy /= len; zz /= len;
            let xx = uy * zz - uz * zy, xy = uz * zx - ux * zz, xz = ux * zy - uy * zx;
            len = Math.hypot(xx, xy, xz);
            if (len === 0) { xx = 1; xy = 0; xz = 0; } else { xx /= len; xy /= len; xz /= len; }
            let yx = zy * xz - zz * xy, yy = zz * xx - zx * xz, yz = zx * xy - zy * xx;
            out[0] = xx; out[1] = yx; out[2] = zx; out[3] = 0;
            out[4] = xy; out[5] = yy; out[6] = zy; out[7] = 0;
            out[8] = xz; out[9] = yz; out[10] = zz; out[11] = 0;
            out[12] = -(xx * ex + xy * ey + xz * ez);
            out[13] = -(yx * ex + yy * ey + yz * ez);
            out[14] = -(zx * ex + zy * ey + zz * ez);
            out[15] = 1;
            return out;
        }
    };

    // --- Draw loop ---
    function draw() {
        const w = Math.max(1, canvas.clientWidth || canvas.width),
            h = Math.max(1, canvas.clientHeight || canvas.height);
        if (canvas.width !== w || canvas.height !== h) { canvas.width = w; canvas.height = h; }

        gl.viewport(0, 0, w, h);
        gl.clearColor(1, 1, 1, 1);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        gl.useProgram(prog);

        const proj = externalProj ? new Float32Array(externalProj) : new Float32Array([1.8, 0, 0, 0, 0, 2.4, 0, 0, 0, 0, -1, -1, 0, 0, -0.2, 0]);
        const camDist = canvas._cam_zoom;
        const camX = camDist * Math.cos(canvas._cam_ry) * Math.cos(canvas._cam_rx);
        const camY = camDist * Math.sin(canvas._cam_ry) * Math.cos(canvas._cam_rx);
        const camZ = camDist * Math.sin(canvas._cam_rx);
        const view = mat4.create();
        mat4.lookAt(view, [camX, camY, camZ], [0, 0, 0], [0, 0, 1]);

        if (uProj) gl.uniformMatrix4fv(uProj, false, proj);
        if (uView) gl.uniformMatrix4fv(uView, false, view);

        canvas._camState = { 
            rx: canvas._cam_rx, 
            ry: canvas._cam_ry, 
            zoom: canvas._cam_zoom, 
            view, proj, cx, cy, scaleXY, scaleZ 
        };

        // Draw faces
        if (aPos !== -1) { gl.bindBuffer(gl.ARRAY_BUFFER, faceBuf); gl.vertexAttribPointer(aPos, 3, gl.FLOAT, false, 0, 0); gl.enableVertexAttribArray(aPos); }
        if (aCol !== -1) { gl.bindBuffer(gl.ARRAY_BUFFER, colorBuf); gl.vertexAttribPointer(aCol, 4, gl.FLOAT, false, 0, 0); gl.enableVertexAttribArray(aCol); }
        gl.drawArrays(gl.TRIANGLES, 0, faceVertices.length / 3);

        // Draw edges
        if (edgeVertices.length) {
            if (aPos !== -1) { gl.bindBuffer(gl.ARRAY_BUFFER, edgeBuf); gl.vertexAttribPointer(aPos, 3, gl.FLOAT, false, 0, 0); gl.enableVertexAttribArray(aPos); }
            if (aCol !== -1) { gl.bindBuffer(gl.ARRAY_BUFFER, edgeColorBuf); gl.vertexAttribPointer(aCol, 4, gl.FLOAT, false, 0, 0); gl.enableVertexAttribArray(aCol); }
            gl.drawArrays(gl.LINES, 0, edgeVertices.length / 3);
        }

        canvas._drawLoopId = requestAnimationFrame(draw);
    }

    requestAnimationFrame(draw);
};

// --- Export 3D View to SVG ---
window.export3DToSVG = (canvasId, filename) => {
    const canvas = document.getElementById(canvasId);
    if (!canvas || !canvas._camState || !canvas._geoData) return;

    const { view, proj, cx, cy, scaleXY, scaleZ } = canvas._camState;
    const { meshes, colors, heights, baseHeights, edgePolygons } = canvas._geoData;
    const w = canvas.width;
    const h = canvas.height;

    function project(p) {
        // column-major multiplication
        let x = p[0] * view[0] + p[1] * view[4] + p[2] * view[8] + view[12];
        let y = p[0] * view[1] + p[1] * view[5] + p[2] * view[9] + view[13];
        let z = p[0] * view[2] + p[1] * view[6] + p[2] * view[10] + view[14];
        let mw = p[0] * view[3] + p[1] * view[7] + p[2] * view[11] + view[15];

        let px = x * proj[0] + y * proj[4] + z * proj[8] + mw * proj[12];
        let py = x * proj[1] + y * proj[5] + z * proj[9] + mw * proj[13];
        let pz = x * proj[2] + y * proj[6] + z * proj[10] + mw * proj[14];
        let pw = x * proj[3] + y * proj[7] + z * proj[11] + mw * proj[15];

        return [(px / pw + 1) * w / 2, (1 - py / pw) * h / 2, pz / pw];
    }

    let elements = [];

    // Faces
    meshes.forEach((tris, i) => {
        const baseColor = colors[i] || [0.8, 0.8, 0.8];
        const height = (heights[i] ?? 1.0) * scaleZ;
        const baseH = (baseHeights[i] ?? 0.0) * scaleZ;

        tris.forEach(tri => {
            // Top face
            let topPts = tri.map(([vx, vy]) => project([(vx - cx) * scaleXY, (vy - cy) * scaleXY, baseH + height]));
            let topZ = topPts.reduce((a, b) => a + b[2], 0) / 3;
            let topCol = baseColor.map(c => Math.floor(c * 0.9 * 255));
            elements.push({ z: topZ, type: 'poly', pts: topPts, fill: `rgb(${topCol.join(',')})` });

            // Bottom face
            let botPts = tri.map(([vx, vy]) => project([(vx - cx) * scaleXY, (vy - cy) * scaleXY, baseH]));
            let botZ = botPts.reduce((a, b) => a + b[2], 0) / 3;
            let botCol = baseColor.map(c => Math.floor(c * 0.4 * 255));
            elements.push({ z: botZ, type: 'poly', pts: botPts, fill: `rgb(${botCol.join(',')})` });
        });
    });

    // Walls
    if (edgePolygons) {
        const lightDir = [0.5, 0.3, 0.8];
        const len = Math.hypot(...lightDir);
        const lx = lightDir[0] / len, ly = lightDir[1] / len;

        edgePolygons.forEach((poly, i) => {
            const baseColor = colors[i] || [0.5, 0.5, 0.5];
            const height = (heights[i] ?? 1.0) * scaleZ;
            const baseH = (baseHeights[i] ?? 0.0) * scaleZ;

            for (let j = 0; j < poly.length; j++) {
                const [x1, y1] = poly[j];
                const [x2, y2] = poly[(j + 1) % poly.length];
                const nx1 = (x1 - cx) * scaleXY, ny1 = (y1 - cy) * scaleXY;
                const nx2 = (x2 - cx) * scaleXY, ny2 = (y2 - cy) * scaleXY;

                const dx = nx2 - nx1, dy = ny2 - ny1;
                const wlen = Math.hypot(dx, dy);
                const wnx = dy / wlen, wny = -dx / wlen;
                const dot = Math.max(0, wnx * lx + wny * ly);
                const shade = 0.5 + 0.4 * dot;
                const col = baseColor.map(c => Math.floor(c * shade * 255));

                let wallPts = [
                    project([nx1, ny1, baseH]),
                    project([nx2, ny2, baseH]),
                    project([nx2, ny2, baseH + height]),
                    project([nx1, ny1, baseH + height])
                ];
                let wallZ = wallPts.reduce((a, b) => a + b[2], 0) / 4;
                elements.push({ z: wallZ, type: 'poly', pts: wallPts, fill: `rgb(${col.join(',')})` });
            }
        });
    }

    // Sort by depth (painter's algorithm)
    elements.sort((a, b) => b.z - a.z);

    let svg = `<svg xmlns="http://www.w3.org/2000/svg" width="${w}" height="${h}" viewBox="0 0 ${w} ${h}">`;
    svg += `<rect width="100%" height="100%" fill="#f9f9f9" />`;
    elements.forEach(el => {
        if (el.type === 'poly') {
            const d = el.pts.map(p => `${p[0]},${p[1]}`).join(' ');
            svg += `<polygon points="${d}" fill="${el.fill}" stroke="${el.fill}" stroke-width="0.5" />`;
        }
    });
    svg += `</svg>`;

    const blob = new Blob([svg], { type: 'image/svg+xml' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = filename;
    a.click();
    URL.revokeObjectURL(url);
};
