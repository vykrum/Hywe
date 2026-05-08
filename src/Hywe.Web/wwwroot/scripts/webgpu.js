// --- WebGPU implementation ---

const wgslShaders = `
struct Uniforms {
    projection: mat4x4<f32>,
    view: mat4x4<f32>,
};

@group(0) @binding(0) var<uniform> uniforms: Uniforms;

struct VertexInput {
    @location(0) position: vec3<f32>,
    @location(1) color: vec4<f32>,
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) color: vec4<f32>,
};

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    out.position = uniforms.projection * uniforms.view * vec4<f32>(in.position, 1.0);
    out.color = in.color;
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    return in.color;
}
`;

window.disposeWebGPU = (canvasId) => {
    const canvas = document.getElementById(canvasId);
    if (!canvas || !canvas._wgpuState) return;

    if (canvas._drawLoopId) {
        cancelAnimationFrame(canvas._drawLoopId);
        canvas._drawLoopId = null;
    }

    const state = canvas._wgpuState;
    if (state.faceBuffer) state.faceBuffer.destroy();
    if (state.colorBuffer) state.colorBuffer.destroy();
    if (state.edgeBuffer) state.edgeBuffer.destroy();
    if (state.edgeColorBuffer) state.edgeColorBuffer.destroy();
    if (state.uniformBuffer) state.uniformBuffer.destroy();
    if (state.depthTexture) state.depthTexture.destroy();

    canvas._wgpuState = null;
};

window.initWebGPUExtrudedPolygons = async (canvasId, meshes, colors, heights, baseHeights, edgePolygons, centroids, externalProj, viewLocked) => {
    const canvas = document.getElementById(canvasId);
    if (!canvas) return console.error("Canvas not found:", canvasId);

    // Cancel existing loop
    if (canvas._drawLoopId) {
        cancelAnimationFrame(canvas._drawLoopId);
    }
    window.disposeWebGPU(canvasId);

    if (!navigator.gpu) {
        const ctx = canvas.getContext('2d');
        if (ctx) {
            canvas.width = canvas.clientWidth || 600;
            canvas.height = canvas.clientHeight || 400;
            ctx.fillStyle = "#f8f9fa";
            ctx.fillRect(0, 0, canvas.width, canvas.height);
            ctx.fillStyle = "#dc3545";
            ctx.font = "16px sans-serif";
            ctx.textAlign = "center";
            ctx.fillText("WebGPU is not supported by your browser.", canvas.width / 2, canvas.height / 2 - 10);
            ctx.fillStyle = "#6c757d";
            ctx.font = "14px sans-serif";
            ctx.fillText("Please upgrade to a modern browser (like Chrome or Edge) to view the 3D model.", canvas.width / 2, canvas.height / 2 + 15);
        }
        return;
    }

    const adapter = await navigator.gpu.requestAdapter();
    if (!adapter) {
        console.error("Failed to get GPU adapter.");
        return;
    }
    const device = await adapter.requestDevice();

    const context = canvas.getContext('webgpu');
    const presentationFormat = navigator.gpu.getPreferredCanvasFormat();

    canvas.width = canvas.clientWidth || 600;
    canvas.height = canvas.clientHeight || 400;

    context.configure({
        device,
        format: presentationFormat,
        alphaMode: 'premultiplied',
    });

    canvas._viewLocked = !!viewLocked;
    canvas._geoData = { meshes, colors, heights, baseHeights, edgePolygons, centroids, externalProj };

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

    // --- Prepare face buffers ---
    const faceVertices = [], faceColors = [];
    
    // Removed ground shadow plane per user request

    meshes.forEach((tris, i) => {
        const baseColor = (colors && colors[i]) ? colors[i] : [0.8, 0.8, 0.8];
        const height = (heights?.[i] ?? 1.0) * scaleZ;
        const baseH = (baseHeights?.[i] ?? 0.0) * scaleZ;

        tris.forEach(tri => {
            tri.forEach(([x, y]) => {
                const nx = (x - cx) * scaleXY;
                const ny = (y - cy) * scaleXY;
                const nz = baseH + height;
                const col = baseColor.map(c => c * 0.95);
                faceVertices.push(nx, ny, nz);
                faceColors.push(...col, 1.0);
            });

            for (let j = tri.length - 1; j >= 0; j--) {
                const [x, y] = tri[j];
                const nx = (x - cx) * scaleXY;
                const ny = (y - cy) * scaleXY;
                const nz = baseH;
                const col = baseColor.map(c => c * 0.85);
                faceVertices.push(nx, ny, nz);
                faceColors.push(...col, 1.0);
            }
        });
    });

    if (edgePolygons?.length) {
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

                const dx = nx2 - nx1, dy = ny2 - ny1;
                const wallLen = Math.sqrt(dx*dx + dy*dy);
                const wnx = dy / wallLen, wny = -dx / wallLen;
                
                const dot = Math.max(0, wnx * lx + wny * ly);
                const wallShade = 0.7 + 0.25 * dot;
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

    const faceVertexData = new Float32Array(faceVertices);
    const faceColorData = new Float32Array(faceColors);

    const faceBuffer = device.createBuffer({
        size: faceVertexData.byteLength,
        usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
    });
    device.queue.writeBuffer(faceBuffer, 0, faceVertexData);

    const colorBuffer = device.createBuffer({
        size: faceColorData.byteLength,
        usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
    });
    device.queue.writeBuffer(colorBuffer, 0, faceColorData);

    // --- Prepare edge lines ---
    const edgeVertices = [], edgeColors = [];
    if (edgePolygons?.length) {
        edgePolygons.forEach((poly, i) => {
            const edgeColor = [0.4, 0.4, 0.4, 1.0];
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

    const edgeVertexData = new Float32Array(edgeVertices);
    const edgeColorData = new Float32Array(edgeColors);

    const edgeBuffer = device.createBuffer({
        size: edgeVertexData.byteLength || 16, // Ensure non-zero size
        usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
    });
    if (edgeVertexData.byteLength > 0) {
        device.queue.writeBuffer(edgeBuffer, 0, edgeVertexData);
    }

    const edgeColorBuffer = device.createBuffer({
        size: edgeColorData.byteLength || 16,
        usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
    });
    if (edgeColorData.byteLength > 0) {
        device.queue.writeBuffer(edgeColorBuffer, 0, edgeColorData);
    }

    // --- Pipeline setup ---
    const shaderModule = device.createShaderModule({ code: wgslShaders });

    const uniformBufferSize = 4 * 16 * 2; // 2 mat4x4
    const uniformBuffer = device.createBuffer({
        size: uniformBufferSize,
        usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
    });

    const bindGroupLayout = device.createBindGroupLayout({
        entries: [{
            binding: 0,
            visibility: GPUShaderStage.VERTEX,
            buffer: { type: "uniform" }
        }]
    });

    const bindGroup = device.createBindGroup({
        layout: bindGroupLayout,
        entries: [{
            binding: 0,
            resource: { buffer: uniformBuffer }
        }]
    });

    const pipelineLayout = device.createPipelineLayout({
        bindGroupLayouts: [bindGroupLayout]
    });

    const vertexBuffers = [
        {
            arrayStride: 3 * 4, // 3 floats, 4 bytes each
            attributes: [{ shaderLocation: 0, offset: 0, format: 'float32x3' }]
        },
        {
            arrayStride: 4 * 4, // 4 floats, 4 bytes each
            attributes: [{ shaderLocation: 1, offset: 0, format: 'float32x4' }]
        }
    ];

    const facePipeline = device.createRenderPipeline({
        layout: pipelineLayout,
        vertex: {
            module: shaderModule,
            entryPoint: 'vs_main',
            buffers: vertexBuffers
        },
        fragment: {
            module: shaderModule,
            entryPoint: 'fs_main',
            targets: [{
                format: presentationFormat,
                blend: {
                    color: {
                        srcFactor: 'src-alpha',
                        dstFactor: 'one-minus-src-alpha',
                        operation: 'add',
                    },
                    alpha: {
                        srcFactor: 'one',
                        dstFactor: 'one-minus-src-alpha',
                        operation: 'add',
                    },
                }
            }]
        },
        primitive: {
            topology: 'triangle-list',
            cullMode: 'none',
        },
        depthStencil: {
            depthWriteEnabled: true,
            depthCompare: 'less',
            format: 'depth24plus',
            depthBias: 2,
            depthBiasSlopeScale: 2.0,
        },
    });

    const edgePipeline = device.createRenderPipeline({
        layout: pipelineLayout,
        vertex: {
            module: shaderModule,
            entryPoint: 'vs_main',
            buffers: vertexBuffers
        },
        fragment: {
            module: shaderModule,
            entryPoint: 'fs_main',
            targets: [{
                format: presentationFormat,
                blend: {
                    color: {
                        srcFactor: 'src-alpha',
                        dstFactor: 'one-minus-src-alpha',
                        operation: 'add',
                    },
                    alpha: {
                        srcFactor: 'one',
                        dstFactor: 'one-minus-src-alpha',
                        operation: 'add',
                    },
                }
            }]
        },
        primitive: {
            topology: 'line-list',
        },
        depthStencil: {
            depthWriteEnabled: true,
            depthCompare: 'less',
            format: 'depth24plus',
        },
    });

    canvas._wgpuState = {
        device, context, presentationFormat,
        faceBuffer, colorBuffer, edgeBuffer, edgeColorBuffer, uniformBuffer,
        facePipeline, edgePipeline, bindGroup
    };

    // --- Interaction & camera ---
    if (canvas._cam_rx === undefined) {
        canvas._cam_ry = 0;
        canvas._cam_rx = Math.PI / 6;
        canvas._cam_zoom = 3;
    }
    let dragging = false, lx = 0, ly = 0;
    const init = { rx: Math.PI / 6, ry: 0, zoom: 3 };

    if (!canvas._listenersAdded) {
        canvas.addEventListener("pointerdown", e => { if (canvas._viewLocked) return; dragging = true; lx = e.clientX; ly = e.clientY; });
        document.addEventListener("pointerup", () => dragging = false);
        document.addEventListener("pointermove", e => {
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

        canvas._listenersAdded = true;
    }

    const mat4 = {
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

    let depthTexture = null;

    // --- Draw loop ---
    function draw() {
        const w = Math.max(1, canvas.clientWidth || canvas.width);
        const h = Math.max(1, canvas.clientHeight || canvas.height);
        
        if (canvas.width !== w || canvas.height !== h || !depthTexture || depthTexture.width !== w || depthTexture.height !== h) { 
            canvas.width = w; canvas.height = h; 
            if (depthTexture) depthTexture.destroy();
            depthTexture = device.createTexture({
                size: [w, h],
                format: 'depth24plus',
                usage: GPUTextureUsage.RENDER_ATTACHMENT,
            });
            canvas._wgpuState.depthTexture = depthTexture;
        }

        const proj = externalProj ? new Float32Array(externalProj) : new Float32Array([1.8, 0, 0, 0, 0, 2.4, 0, 0, 0, 0, -1, -1, 0, 0, -0.2, 0]);
        const camDist = canvas._cam_zoom;
        const camX = camDist * Math.cos(canvas._cam_ry) * Math.cos(canvas._cam_rx);
        const camY = camDist * Math.sin(canvas._cam_ry) * Math.cos(canvas._cam_rx);
        const camZ = camDist * Math.sin(canvas._cam_rx);
        const view = new Float32Array(16);
        mat4.lookAt(view, [camX, camY, camZ], [0, 0, 0], [0, 0, 1]);

        device.queue.writeBuffer(uniformBuffer, 0, proj.buffer, proj.byteOffset, proj.byteLength);
        device.queue.writeBuffer(uniformBuffer, 64, view.buffer, view.byteOffset, view.byteLength);

        const commandEncoder = device.createCommandEncoder();
        const textureView = context.getCurrentTexture().createView();

        const renderPassDescriptor = {
            colorAttachments: [{
                view: textureView,
                clearValue: { r: 1.0, g: 1.0, b: 1.0, a: 1.0 },
                loadOp: 'clear',
                storeOp: 'store',
            }],
            depthStencilAttachment: {
                view: depthTexture.createView(),
                depthClearValue: 1.0,
                depthLoadOp: 'clear',
                depthStoreOp: 'store',
            },
        };

        const passEncoder = commandEncoder.beginRenderPass(renderPassDescriptor);
        
        passEncoder.setBindGroup(0, bindGroup);

        if (faceVertices.length > 0) {
            passEncoder.setPipeline(facePipeline);
            passEncoder.setVertexBuffer(0, faceBuffer);
            passEncoder.setVertexBuffer(1, colorBuffer);
            passEncoder.draw(faceVertices.length / 3);
        }

        if (edgeVertices.length > 0) {
            passEncoder.setPipeline(edgePipeline);
            passEncoder.setVertexBuffer(0, edgeBuffer);
            passEncoder.setVertexBuffer(1, edgeColorBuffer);
            passEncoder.draw(edgeVertices.length / 3);
        }

        passEncoder.end();

        device.queue.submit([commandEncoder.finish()]);

        canvas._drawLoopId = requestAnimationFrame(draw);
    }

    requestAnimationFrame(draw);
};

// SVG Capture functionality stub - keeping interface but WebGPU canvas needs special readback for exact SVG mapping
window.export3DToSVG = (canvasId, filename) => {
    console.warn("SVG export from WebGPU is not fully implemented in this prototype.");
};
window.captureCanvasSVG = (canvasId) => {
    console.warn("SVG capture from WebGPU is not fully implemented in this prototype.");
    return null;
};
