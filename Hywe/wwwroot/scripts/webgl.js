// --- Global helpers ---
// Reusable mat4 helpers
window.mat4 = {
    create: () => new Float32Array([
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1
    ]),
    perspective: (out, fovy, aspect, near, far) => {
        const f = 1 / Math.tan(fovy / 2);
        out.fill(0);
        out[0] = f / aspect; out[5] = f;
        out[10] = (far + near) / (near - far); out[11] = -1;
        out[14] = (2 * far * near) / (near - far);
        return out;
    },
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

// --- Global shader sources ---
window.glShaders = {
    vsSource: `
        attribute vec3 a_position;
        attribute vec3 a_color;
        uniform mat4 u_projection;
        uniform mat4 u_view;
        varying vec3 v_color;
        void main() {
            gl_Position = u_projection * u_view * vec4(a_position, 1.0);
            v_color = a_color;
        }`,
    fsSource: `
        precision mediump float;
        varying vec3 v_color;
        uniform vec3 u_overrideColor;
        void main() {
            float alpha = 0.95;
            if (u_overrideColor.r < 0.0)
                gl_FragColor = vec4(v_color, alpha);
            else
                gl_FragColor = vec4(u_overrideColor, 1.0);
        }`
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

    // Do NOT force loseContext here
    // Just clear references
    canvas._listenersAdded = false;
};

// --- Initialize WebGL extruded polygons ---
// --- Initialize WebGL extruded polygons ---
window.initWebGLExtrudedPolygons = (canvasId, meshes, colors, heights, edgePolygons) => {
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
            setTimeout(() => window.initWebGLExtrudedPolygons(canvasId, meshes, colors, heights, edgePolygons), 50);
            return;
        }
        canvas._glContext = gl;
    }

    if (canvas._drawLoopId) cancelAnimationFrame(canvas._drawLoopId);

    const vs = compileShader(gl, gl.VERTEX_SHADER, window.glShaders.vsSource);
    const fs = compileShader(gl, gl.FRAGMENT_SHADER, window.glShaders.fsSource);
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

            // Bottom face
            tri.forEach(([x, y]) => {
                const nx = (x - cx) * scaleXY;
                const ny = (y - cy) * scaleXY;
                const nz = 0;
                const shade = 0.7 + 0.3 * Math.min(1, nz + 0.5);
                const col = baseColor.map(c => c * shade);
                faceVertices.push(nx, ny, nz);
                faceColors.push(...col);
            });
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

                // Wall (two triangles per edge)
                const wallVerts = [
                    [nx1, ny1, 0], [nx1, ny1, height], [nx2, ny2, height],
                    [nx1, ny1, 0], [nx2, ny2, height], [nx2, ny2, 0]
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
            window.initWebGLExtrudedPolygons(canvasId, meshes, colors, heights, edgePolygons);
        });
        canvas._listenersAdded = true;
    }

    gl.enable(gl.DEPTH_TEST);
    gl.enable(gl.BLEND);
    gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

    // --- Draw loop ---
    function draw() {
        const w = Math.max(1, canvas.clientWidth || canvas.width),
            h = Math.max(1, canvas.clientHeight || canvas.height);
        if (canvas.width !== w || canvas.height !== h) { canvas.width = w; canvas.height = h; }

        gl.viewport(0, 0, w, h);
        gl.clearColor(1, 1, 1, 1);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
        gl.useProgram(prog);

        const proj = mat4.create();
        mat4.perspective(proj, Math.PI / 4, w / h, 0.1, 100);
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

        canvas._drawLoopId = requestAnimationFrame(draw);
    }

    requestAnimationFrame(draw);
};

