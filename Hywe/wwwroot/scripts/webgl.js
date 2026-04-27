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

    canvas._listenersAdded = false;
};

// --- Compile shader helper ---
function compileShader(gl, type, src) {
    const sh = gl.createShader(type);
    gl.shaderSource(sh, src);
    gl.compileShader(sh);
    if (!gl.getShaderParameter(sh, gl.COMPILE_STATUS)) {
        console.error(gl.getShaderInfoLog(sh));
        return null;
    }
    return sh;
}

// --- Initialize WebGL extruded polygons ---
window.initWebGLExtrudedPolygons = (canvasId, meshes, colors, heights, edgePolygons, centroids, vsSource, fsSource, externalProj) => {
    window.disposeWebGL(canvasId);

    const canvas = document.getElementById(canvasId);
    if (!canvas) return console.error("Canvas not found:", canvasId);
    canvas.width = canvas.clientWidth || 600;
    canvas.height = canvas.clientHeight || 400;

    let gl = canvas._glContext;
    if (!gl) {
        gl = canvas.getContext("webgl");
        if (!gl) {
            console.warn("WebGL context not available, retrying in 50ms...");
            setTimeout(() => window.initWebGLExtrudedPolygons(canvasId, meshes, colors, heights, edgePolygons, centroids, vsSource, fsSource, externalProj), 50);
            return;
        }
        canvas._glContext = gl;
    }

    if (canvas._drawLoopId) cancelAnimationFrame(canvas._drawLoopId);

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
    const uOverride = gl.getUniformLocation(prog, "u_overrideColor");

    // --- Compute normalization ---
    let minX = Infinity, maxX = -Infinity;
    let minY = Infinity, maxY = -Infinity;

    meshes.forEach(mesh => mesh.forEach(tri =>
        tri.forEach(([x, y]) => {
            minX = Math.min(minX, x); maxX = Math.max(maxX, x);
            minY = Math.min(minY, y); maxY = Math.max(maxY, y);
        })
    ));

    const cx = (minX + maxX) / 2;
    const cy = (minY + maxY) / 2;
    const scaleXY = 2 / Math.max(maxX - minX, maxY - minY);
    const scaleZ = scaleXY;

    // --- Normalize Centroids ---
    const modelCentroids = (centroids || []).map(([x, y, z]) => [
        (x - cx) * scaleXY,
        (y - cy) * scaleXY,
        z * scaleZ
    ]);

    // --- Precompute Wall Candidates for Labels ---
    const massWalls = [];
    if (edgePolygons?.length) {
        edgePolygons.forEach((poly, i) => {
            const height = (heights?.[i] ?? 1.0) * scaleZ;
            let walls = [];
            for (let j = 0; j < poly.length; j++) {
                const [x1, y1] = poly[j];
                const [x2, y2] = poly[(j + 1) % poly.length];

                const nx1 = (x1 - cx) * scaleXY;
                const ny1 = (y1 - cy) * scaleXY;
                const nx2 = (x2 - cx) * scaleXY;
                const ny2 = (y2 - cy) * scaleXY;

                // Midpoint of the wall, vertically centered
                let mx = (nx1 + nx2) / 2;
                let my = (ny1 + ny2) / 2;
                let mz = height / 2;
                walls.push([mx, my, mz]);
            }
            massWalls.push(walls);
        });
    }

    // --- Prepare face buffers (top + bottom) ---
    const faceVertices = [], faceColors = [];
    meshes.forEach((tris, i) => {
        const baseColor = colors[i] || [0.8, 0.8, 0.8];
        const height = (heights?.[i] ?? 1.0) * scaleZ;

        tris.forEach(tri => {
            // Top face
            tri.forEach(([x, y]) => {
                const nx = (x - cx) * scaleXY;
                const ny = (y - cy) * scaleXY;
                const nz = height;
                const shade = 0.7 + 0.3 * Math.min(1, nz + 0.5);
                const col = baseColor.map(c => c * shade);
                faceVertices.push(nx, ny, nz);
                faceColors.push(...col);
            });

            // Bottom face (reversed winding for correct outward normal)
            for (let j = tri.length - 1; j >= 0; j--) {
                const [x, y] = tri[j];
                const nx = (x - cx) * scaleXY;
                const ny = (y - cy) * scaleXY;
                const nz = 0;
                const shade = 0.7 + 0.3 * Math.min(1, nz + 0.5);
                const col = baseColor.map(c => c * shade);
                faceVertices.push(nx, ny, nz);
                faceColors.push(...col);
            }
        });
    });

    // --- Prepare wall faces from edgePolygons only ---
    if (edgePolygons?.length) {
        edgePolygons.forEach((poly, i) => {
            const baseColor = colors[i] || [0.5, 0.5, 0.5];
            const height = (heights?.[i] ?? 1.0) * scaleZ;

            for (let j = 0; j < poly.length; j++) {
                const [x1, y1] = poly[j];
                const [x2, y2] = poly[(j + 1) % poly.length];

                const nx1 = (x1 - cx) * scaleXY;
                const ny1 = (y1 - cy) * scaleXY;
                const nx2 = (x2 - cx) * scaleXY;
                const ny2 = (y2 - cy) * scaleXY;

                // Wall (two triangles per edge, outward winding)
                const wallVerts = [
                    [nx1, ny1, 0], [nx2, ny2, height], [nx1, ny1, height],
                    [nx1, ny1, 0], [nx2, ny2, 0], [nx2, ny2, height]
                ];
                wallVerts.forEach(([vx, vy, vz]) => {
                    faceVertices.push(vx, vy, vz);
                    const shade = 0.7 + 0.3 * Math.min(1, vz + 0.5);
                    const col = baseColor.map(c => c * shade);
                    faceColors.push(...col);
                });
            }
        });
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
            const baseColor = colors[i] || [0, 0, 0];
            const height = (heights?.[i] ?? 1.0) * scaleZ;

            for (let j = 0; j < poly.length; j++) {
                const [x1, y1] = poly[j];
                const [x2, y2] = poly[(j + 1) % poly.length];

                const nx1 = (x1 - cx) * scaleXY;
                const ny1 = (y1 - cy) * scaleXY;
                const nx2 = (x2 - cx) * scaleXY;
                const ny2 = (y2 - cy) * scaleXY;

                // Top edge
                edgeVertices.push(nx1, ny1, height, nx2, ny2, height);
                edgeColors.push(...baseColor, ...baseColor);
                // Bottom edge
                edgeVertices.push(nx1, ny1, 0, nx2, ny2, 0);
                edgeColors.push(...baseColor, ...baseColor);
                // Vertical edges
                edgeVertices.push(nx1, ny1, 0, nx1, ny1, height);
                edgeVertices.push(nx2, ny2, 0, nx2, ny2, height);
                edgeColors.push(...baseColor, ...baseColor, ...baseColor, ...baseColor);
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
    let ry = 0, rx = Math.PI / 6, zoom = 3;
    let dragging = false, lx = 0, ly = 0;
    const init = { rx, ry, zoom };

    if (!canvas._listenersAdded) {
        canvas.addEventListener("mousedown", e => { dragging = true; lx = e.clientX; ly = e.clientY; });
        document.addEventListener("mouseup", () => dragging = false);
        document.addEventListener("mousemove", e => {
            if (!dragging) return;
            const dx = e.clientX - lx, dy = e.clientY - ly;
            ry -= dx * 0.01;
            rx = Math.min(Math.max(rx + dy * 0.01, 0.01), Math.PI / 2 - 0.01);
            lx = e.clientX; ly = e.clientY;
        });
        canvas.addEventListener("dblclick", () => { rx = init.rx; ry = init.ry; zoom = init.zoom; });

        let prevDist = null;
        canvas.addEventListener("touchstart", e => {
            if (e.touches.length === 1) { lx = e.touches[0].clientX; ly = e.touches[0].clientY; }
            else if (e.touches.length === 2) {
                const dx = e.touches[0].clientX - e.touches[1].clientX;
                const dy = e.touches[0].clientY - e.touches[1].clientY;
                prevDist = Math.hypot(dx, dy);
            }
        }, { passive: true });
        canvas.addEventListener("touchmove", e => {
            if (e.touches.length === 1) {
                const dx = e.touches[0].clientX - lx;
                const dy = e.touches[0].clientY - ly;
                ry -= dx * 0.01;
                rx = Math.min(Math.max(rx + dy * 0.01, 0.01), Math.PI / 2 - 0.01);
                lx = e.touches[0].clientX; ly = e.touches[0].clientY;
            } else if (e.touches.length === 2) {
                const dx = e.touches[0].clientX - e.touches[1].clientX;
                const dy = e.touches[0].clientY - e.touches[1].clientY;
                const dist = Math.hypot(dx, dy);
                if (prevDist) { zoom *= prevDist / dist; zoom = Math.min(Math.max(zoom, 1.5), 10); }
                prevDist = dist;
            }
        }, { passive: true });
        canvas.addEventListener("touchend", () => prevDist = null);

        canvas.addEventListener("webglcontextlost", e => { e.preventDefault(); console.warn("WebGL context lost."); });
        canvas.addEventListener("webglcontextrestored", () => {
            window.initWebGLExtrudedPolygons(canvasId, meshes, colors, heights, edgePolygons, centroids, vsSource, fsSource, externalProj);
        });
        canvas._listenersAdded = true;
    }

    gl.enable(gl.DEPTH_TEST);
    gl.enable(gl.BLEND);
    gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

    // --- Helpers for Labels ---
    function transformPoint(m, v) {
        let x = v[0], y = v[1], z = v[2], w = 1.0;
        let resX = m[0]*x + m[4]*y + m[8]*z + m[12]*w;
        let resY = m[1]*x + m[5]*y + m[9]*z + m[13]*w;
        let resZ = m[2]*x + m[6]*y + m[10]*z + m[14]*w;
        let resW = m[3]*x + m[7]*y + m[11]*z + m[15]*w;
        return { x: resX/resW, y: resY/resW, z: resZ/resW, w: resW };
    }

    function multiplyMat4(a, b) {
        let out = new Float32Array(16);
        for(let col = 0; col < 4; col++) {
            for(let row = 0; row < 4; row++) {
                out[col*4+row] = 
                    a[0*4+row]*b[col*4+0] + 
                    a[1*4+row]*b[col*4+1] + 
                    a[2*4+row]*b[col*4+2] + 
                    a[3*4+row]*b[col*4+3];
            }
        }
        return out;
    }

    // --- Helpers for Matrix operations ---
    const mat4 = {
        create: () => new Float32Array([1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1]),
        lookAt: (out, eye, target, up) => {
            let [ex, ey, ez] = eye, [tx, ty, tz] = target, [ux, uy, uz] = up;
            let zx = ex - tx, zy = ey - ty, zz = ez - tz;
            let len = Math.hypot(zx, zy, zz); zx /= len; zy /= len; zz /= len;
            let xx = uy * zz - uz * zy, xy = uz * zx - ux * zz, xz = ux * zy - uy * xz;
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

        const proj = externalProj || new Float32Array([1.8,0,0,0,0,2.4,0,0,0,0,-1,-1,0,0,-0.2,0]);
        const camDist = zoom;
        const camX = camDist * Math.cos(ry) * Math.cos(rx);
        const camY = camDist * Math.sin(ry) * Math.cos(rx);
        const camZ = camDist * Math.sin(rx);
        const view = mat4.create();
        mat4.lookAt(view, [camX, camY, camZ], [0, 0, 0], [0, 0, 1]);

        if (uProj) gl.uniformMatrix4fv(uProj, false, proj);
        if (uView) gl.uniformMatrix4fv(uView, false, view);
        if (uOverride) gl.uniform3fv(uOverride, [-1, -1, -1]);

        // Draw faces
        if (aPos !== -1) { gl.bindBuffer(gl.ARRAY_BUFFER, faceBuf); gl.vertexAttribPointer(aPos, 3, gl.FLOAT, false, 0, 0); gl.enableVertexAttribArray(aPos); }
        if (aCol !== -1) { gl.bindBuffer(gl.ARRAY_BUFFER, colorBuf); gl.vertexAttribPointer(aCol, 3, gl.FLOAT, false, 0, 0); gl.enableVertexAttribArray(aCol); }
        gl.drawArrays(gl.TRIANGLES, 0, faceVertices.length / 3);

        // Draw edges
        if (edgeVertices.length) {
            if (aPos !== -1) { gl.bindBuffer(gl.ARRAY_BUFFER, edgeBuf); gl.vertexAttribPointer(aPos, 3, gl.FLOAT, false, 0, 0); gl.enableVertexAttribArray(aPos); }
            if (aCol !== -1) { gl.bindBuffer(gl.ARRAY_BUFFER, edgeColorBuf); gl.vertexAttribPointer(aCol, 3, gl.FLOAT, false, 0, 0); gl.enableVertexAttribArray(aCol); }
            gl.drawArrays(gl.LINES, 0, edgeVertices.length / 3);
        }

        // --- Update HTML Labels ---
        if (modelCentroids && modelCentroids.length > 0) {
            const vpMatrix = multiplyMat4(proj, view);
            
            // Dynamically select the best anchor based on camera elevation
            let dynamicAnchors = [];
            for (let i = 0; i < modelCentroids.length; i++) {
                let walls = massWalls[i];
                let height = (heights?.[i] ?? 1.0) * scaleZ;
                let topCenter = [modelCentroids[i][0], modelCentroids[i][1], height];
                
                let bestPoint = topCenter; 
                
                if (camZ <= height) {
                    if (walls && walls.length > 0) {
                        let minDist = Infinity;
                        for (let w = 0; w < walls.length; w++) {
                            let pt = walls[w];
                            let distSq = (pt[0] - camX)**2 + (pt[1] - camY)**2 + (pt[2] - camZ)**2;
                            if (distSq < minDist) {
                                minDist = distSq;
                                bestPoint = pt;
                            }
                        }
                    }
                }
                dynamicAnchors.push(bestPoint);
            }

            let screenPts = dynamicAnchors.map((c, i) => {
                let pt = transformPoint(vpMatrix, c);
                let px = (pt.x + 1) / 2 * w;
                let py = (-pt.y + 1) / 2 * h; 
                return { i: i, x: px, y: py, z: pt.z, w_clip: pt.w };
            });

            screenPts = screenPts.filter(p => 
                p.w_clip > 0 && 
                p.z >= -1.0 && p.z <= 1.0 &&
                p.x >= -40 && p.x <= w + 40 &&
                p.y >= -40 && p.y <= h + 40
            );
            
            screenPts.sort((a, b) => a.z - b.z);

            const labelRadiusX = 25; 
            const labelRadiusY = 10; 
            for (let k = 0; k < screenPts.length; k++) {
                let curr = screenPts[k];
                if (curr.hidden) continue;

                for (let l = k + 1; l < screenPts.length; l++) {
                    let next = screenPts[l];
                    if (next.hidden) continue;

                    if (Math.abs(curr.x - next.x) < labelRadiusX && Math.abs(curr.y - next.y) < labelRadiusY) {
                        next.hidden = true; 
                    }
                }
            }

            for (let j = 0; j < modelCentroids.length; j++) {
                let labelDiv = document.getElementById('mass-label-g-' + j);
                if (!labelDiv) continue;

                let pt = screenPts.find(p => p.i === j && !p.hidden);
                if (pt) {
                    labelDiv.setAttribute('display', 'block');
                    labelDiv.setAttribute('transform', 'translate(' + pt.x + ', ' + pt.y + ')');
                } else {
                    labelDiv.setAttribute('display', 'none');
                }
            }
        }

        canvas._drawLoopId = requestAnimationFrame(draw);
    }

    requestAnimationFrame(draw);
};
