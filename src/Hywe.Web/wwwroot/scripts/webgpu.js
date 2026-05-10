// --- Global Shader Storage ---
window._gpuShaders = window._gpuShaders || { compute: "", render: "", post: "" };
window.registerWebGPUShaders = (c, r, p) => { 
    console.log("WebGPU: Shaders registered.");
    window._gpuShaders.compute = c; 
    window._gpuShaders.render = r; 
    window._gpuShaders.post = p; 
};

let _gpuCache = { 
    adapter: null, 
    device: null,
    lastCamera: {
        ry: 0,
        rx: Math.PI / 6,
        zoom: 3
    }
};

window.disposeWebGPU = (canvasId) => {
    const canvas = document.getElementById(canvasId);
    if (!canvas || !canvas._wgpuState) return;

    if (canvas._drawLoopId) {
        cancelAnimationFrame(canvas._drawLoopId);
        canvas._drawLoopId = null;
    }

    const state = canvas._wgpuState;
    if (state.facePosBuffer) state.facePosBuffer.destroy();
    if (state.faceColBuffer) state.faceColBuffer.destroy();
    if (state.edgePosBuffer) state.edgePosBuffer.destroy();
    if (state.edgeColBuffer) state.edgeColBuffer.destroy();
    if (state.triInputBuffer) state.triInputBuffer.destroy();
    if (state.wallInputBuffer) state.wallInputBuffer.destroy();
    if (state.computeUniformBuffer) state.computeUniformBuffer.destroy();
    if (state.uniformBuffer) state.uniformBuffer.destroy();
    
    if (state.msaaColorTexture) state.msaaColorTexture.destroy();
    if (state.resolvedColorTexture) state.resolvedColorTexture.destroy();
    if (state.msaaDepthTexture) state.msaaDepthTexture.destroy();

    canvas._wgpuState = null;
};

window.initWebGPUExtrudedPolygons = async (canvasId, meshes, colors, heights, baseHeights, edgePolygons, centroids, externalProj, viewLocked) => {
    console.log("WebGPU: Initializing for", canvasId);
    
    const computeWgsl = window._gpuShaders.compute;
    const wgslShaders = window._gpuShaders.render;
    const postProcessWgsl = window._gpuShaders.post;

    if (!computeWgsl || !wgslShaders) {
        console.error("WebGPU: Shaders not registered yet!");
        return;
    }

    const canvas = document.getElementById(canvasId);
    if (!canvas) return;

    if (canvas._isInitializing) return;
    canvas._isInitializing = true;

    try {
        if (canvas._drawLoopId) {
            cancelAnimationFrame(canvas._drawLoopId);
        }
        window.disposeWebGPU(canvasId);

        if (!navigator.gpu) {
            console.error("WebGPU: Not supported by browser.");
            const ctx = canvas.getContext('2d');
            if (ctx) {
                canvas.width = canvas.clientWidth || 600;
                canvas.height = canvas.clientHeight || 400;
                ctx.fillStyle = "#fcfcfc"; ctx.fillRect(0, 0, canvas.width, canvas.height);
                ctx.textAlign = "center";
                
                // Primary message
                ctx.fillStyle = "#333";
                ctx.font = "600 20px 'Outfit', sans-serif";
                ctx.fillText("Please use a compatible browser", canvas.width / 2, canvas.height / 2 - 15);
                
                // Secondary technical detail
                ctx.fillStyle = "#dc3545";
                ctx.font = "14px 'Outfit', sans-serif";
                ctx.fillText("WebGPU is not enabled in this browser", canvas.width / 2, canvas.height / 2 + 15);
            }
            return;
        }

        if (!_gpuCache.device) {
            _gpuCache.adapter = await navigator.gpu.requestAdapter();
            if (!_gpuCache.adapter) throw new Error("No GPU adapter found");
            _gpuCache.device = await _gpuCache.adapter.requestDevice();
            
            _gpuCache.device.lost.then((info) => {
                console.warn(`WebGPU device was lost: ${info.message}`);
                _gpuCache.device = null;
                _gpuCache.adapter = null;
            });
        }
        const device = _gpuCache.device;

    const context = canvas.getContext('webgpu');
    const presentationFormat = navigator.gpu.getPreferredCanvasFormat();

    canvas.width = canvas.clientWidth || 600;
    canvas.height = canvas.clientHeight || 400;

    context.configure({
        device,
        format: presentationFormat,
        alphaMode: 'opaque',
    });

    canvas._viewLocked = !!viewLocked;
    canvas._geoData = { meshes, colors, heights, baseHeights, edgePolygons, centroids, externalProj };

    let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;
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

    let numTris = 0;
    meshes.forEach(tris => numTris += tris.length);
    let numWalls = 0;
    if (edgePolygons) edgePolygons.forEach(poly => numWalls += poly.length);

    const triInputData = new Float32Array(numTris * 12);
    const wallInputData = new Float32Array(numWalls * 12);

    let tIdx = 0;
    meshes.forEach((tris, i) => {
        const c = (colors && colors[i]) ? colors[i] : [0.8, 0.8, 0.8];
        const h = (heights?.[i] ?? 1.0) * scaleZ;
        const bh = (baseHeights?.[i] ?? 0.0) * scaleZ;
        for (let j=0; j<tris.length; j++) {
            const tri = tris[j];
            triInputData[tIdx++] = tri[0][0]; triInputData[tIdx++] = tri[0][1];
            triInputData[tIdx++] = tri[1][0]; triInputData[tIdx++] = tri[1][1];
            triInputData[tIdx++] = tri[2][0]; triInputData[tIdx++] = tri[2][1];
            triInputData[tIdx++] = c[0]; triInputData[tIdx++] = c[1]; triInputData[tIdx++] = c[2];
            triInputData[tIdx++] = bh; triInputData[tIdx++] = h;
            tIdx++; // Padding to 12 floats
        }
    });

    let wIdx = 0;
    if (edgePolygons) {
        edgePolygons.forEach((poly, i) => {
            const c = (colors && colors[i]) ? colors[i] : [0.5, 0.5, 0.5];
            const h = (heights?.[i] ?? 1.0) * scaleZ;
            const bh = (baseHeights?.[i] ?? 0.0) * scaleZ;
            for (let j=0; j<poly.length; j++) {
                const p1 = poly[j];
                const p2 = poly[(j+1)%poly.length];
                wallInputData[wIdx++] = p1[0]; wallInputData[wIdx++] = p1[1];
                wallInputData[wIdx++] = p2[0]; wallInputData[wIdx++] = p2[1];
                wallInputData[wIdx++] = c[0]; wallInputData[wIdx++] = c[1]; wallInputData[wIdx++] = c[2];
                wallInputData[wIdx++] = bh; wallInputData[wIdx++] = h;
                wIdx += 3; // Padding to 12 floats
            }
        });
    }

    const getStorageBuffer = (data, minSize) => {
        let size = data.byteLength;
        if (size === 0) size = minSize || 16;
        size = Math.ceil(size / 4) * 4; 
        const buf = device.createBuffer({ size, usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST });
        if (data.byteLength > 0) device.queue.writeBuffer(buf, 0, data);
        return buf;
    };

    const triInputBuffer = getStorageBuffer(triInputData);
    const wallInputBuffer = getStorageBuffer(wallInputData);

    const computeUniformData = new ArrayBuffer(48);
    const cuF32 = new Float32Array(computeUniformData);
    const cuU32 = new Uint32Array(computeUniformData);
    cuF32[0] = cx; cuF32[1] = cy; cuF32[2] = scaleXY; cuF32[3] = scaleZ;
    const lightDir = [0.5, 0.3, 0.8];
    const lenL = Math.sqrt(lightDir[0]*lightDir[0] + lightDir[1]*lightDir[1] + lightDir[2]*lightDir[2]);
    cuF32[4] = lightDir[0]/lenL; cuF32[5] = lightDir[1]/lenL; cuF32[6] = lightDir[2]/lenL; 
    cuU32[7] = numTris; cuU32[8] = numWalls;

    const computeUniformBuffer = device.createBuffer({ size: 48, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST });
    device.queue.writeBuffer(computeUniformBuffer, 0, computeUniformData);

    const totalFaceVertices = numTris * 6 + numWalls * 6;
    const facePosBuffer = device.createBuffer({ size: Math.max(16, totalFaceVertices * 3 * 4), usage: GPUBufferUsage.VERTEX | GPUBufferUsage.STORAGE });
    const faceColBuffer = device.createBuffer({ size: Math.max(16, totalFaceVertices * 4 * 4), usage: GPUBufferUsage.VERTEX | GPUBufferUsage.STORAGE });

    const totalEdgeVertices = numWalls * 8;
    const edgePosBuffer = device.createBuffer({ size: Math.max(16, totalEdgeVertices * 3 * 4), usage: GPUBufferUsage.VERTEX | GPUBufferUsage.STORAGE });
    const edgeColBuffer = device.createBuffer({ size: Math.max(16, totalEdgeVertices * 4 * 4), usage: GPUBufferUsage.VERTEX | GPUBufferUsage.STORAGE });

    const computeModule = device.createShaderModule({ code: computeWgsl });
    const computeBindGroupLayout = device.createBindGroupLayout({
        entries: [
            { binding: 0, visibility: GPUShaderStage.COMPUTE, buffer: { type: "uniform" } },
            { binding: 1, visibility: GPUShaderStage.COMPUTE, buffer: { type: "read-only-storage" } },
            { binding: 2, visibility: GPUShaderStage.COMPUTE, buffer: { type: "read-only-storage" } },
            { binding: 3, visibility: GPUShaderStage.COMPUTE, buffer: { type: "storage" } },
            { binding: 4, visibility: GPUShaderStage.COMPUTE, buffer: { type: "storage" } },
            { binding: 5, visibility: GPUShaderStage.COMPUTE, buffer: { type: "storage" } },
            { binding: 6, visibility: GPUShaderStage.COMPUTE, buffer: { type: "storage" } },
        ]
    });

    const computeBindGroup = device.createBindGroup({
        layout: computeBindGroupLayout,
        entries: [
            { binding: 0, resource: { buffer: computeUniformBuffer } },
            { binding: 1, resource: { buffer: triInputBuffer } },
            { binding: 2, resource: { buffer: wallInputBuffer } },
            { binding: 3, resource: { buffer: facePosBuffer } },
            { binding: 4, resource: { buffer: faceColBuffer } },
            { binding: 5, resource: { buffer: edgePosBuffer } },
            { binding: 6, resource: { buffer: edgeColBuffer } },
        ]
    });

    const computePipelineTris = device.createComputePipeline({
        layout: device.createPipelineLayout({ bindGroupLayouts: [computeBindGroupLayout] }),
        compute: { module: computeModule, entryPoint: 'computeTris' },
    });
    const computePipelineWalls = device.createComputePipeline({
        layout: device.createPipelineLayout({ bindGroupLayouts: [computeBindGroupLayout] }),
        compute: { module: computeModule, entryPoint: 'computeWalls' },
    });

    const computeEncoder = device.createCommandEncoder();
    const computePass = computeEncoder.beginComputePass();
    computePass.setBindGroup(0, computeBindGroup);
    if (numTris > 0) {
        computePass.setPipeline(computePipelineTris);
        computePass.dispatchWorkgroups(Math.ceil(numTris / 64));
    }
    if (numWalls > 0) {
        computePass.setPipeline(computePipelineWalls);
        computePass.dispatchWorkgroups(Math.ceil(numWalls / 64));
    }
    computePass.end();
    device.queue.submit([computeEncoder.finish()]);

    const shaderModule = device.createShaderModule({ code: wgslShaders });
    const uniformBuffer = device.createBuffer({ size: 128, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST });
    const bindGroupLayout = device.createBindGroupLayout({
        entries: [{ binding: 0, visibility: GPUShaderStage.VERTEX, buffer: { type: "uniform" } }]
    });
    const bindGroup = device.createBindGroup({
        layout: bindGroupLayout,
        entries: [{ binding: 0, resource: { buffer: uniformBuffer } }]
    });

    const vertexBuffers = [
        { arrayStride: 12, attributes: [{ shaderLocation: 0, offset: 0, format: 'float32x3' }] },
        { arrayStride: 16, attributes: [{ shaderLocation: 1, offset: 0, format: 'float32x4' }] }
    ];

    const facePipeline = device.createRenderPipeline({
        layout: device.createPipelineLayout({ bindGroupLayouts: [bindGroupLayout] }),
        vertex: { module: shaderModule, entryPoint: 'vs_main', buffers: vertexBuffers },
        fragment: {
            module: shaderModule, entryPoint: 'fs_main',
            targets: [{
                format: presentationFormat,
                blend: {
                    color: { srcFactor: 'src-alpha', dstFactor: 'one-minus-src-alpha', operation: 'add' },
                    alpha: { srcFactor: 'one', dstFactor: 'one-minus-src-alpha', operation: 'add' },
                }
            }]
        },
        primitive: { topology: 'triangle-list' },
        depthStencil: { depthWriteEnabled: true, depthCompare: 'less', format: 'depth32float', depthBias: 2, depthBiasSlopeScale: 2.0 },
        multisample: { count: 4 },
    });

    const edgePipeline = device.createRenderPipeline({
        layout: device.createPipelineLayout({ bindGroupLayouts: [bindGroupLayout] }),
        vertex: { module: shaderModule, entryPoint: 'vs_main', buffers: vertexBuffers },
        fragment: {
            module: shaderModule, entryPoint: 'fs_main',
            targets: [{
                format: presentationFormat,
                blend: {
                    color: { srcFactor: 'src-alpha', dstFactor: 'one-minus-src-alpha', operation: 'add' },
                    alpha: { srcFactor: 'one', dstFactor: 'one-minus-src-alpha', operation: 'add' },
                }
            }]
        },
        primitive: { topology: 'line-list' },
        depthStencil: { depthWriteEnabled: true, depthCompare: 'less', format: 'depth32float' },
        multisample: { count: 4 },
    });

    const postModule = device.createShaderModule({ code: postProcessWgsl });
    const postBindGroupLayout = device.createBindGroupLayout({
        entries: [
            { binding: 0, visibility: GPUShaderStage.FRAGMENT, texture: { sampleType: 'float', viewDimension: '2d' } },
            { binding: 1, visibility: GPUShaderStage.FRAGMENT, texture: { sampleType: 'depth', viewDimension: '2d', multisampled: true } },
            { binding: 2, visibility: GPUShaderStage.FRAGMENT, sampler: { type: 'filtering' } },
        ]
    });
    const postPipeline = device.createRenderPipeline({
        layout: device.createPipelineLayout({ bindGroupLayouts: [postBindGroupLayout] }),
        vertex: { module: postModule, entryPoint: 'vs_post' },
        fragment: {
            module: postModule, entryPoint: 'fs_post',
            targets: [{ format: presentationFormat }]
        },
        primitive: { topology: 'triangle-list' }
    });
    const linearSampler = device.createSampler({ magFilter: 'linear', minFilter: 'linear' });

    canvas._wgpuState = {
        device, context, presentationFormat,
        facePosBuffer, faceColBuffer, edgePosBuffer, edgeColBuffer, 
        uniformBuffer, facePipeline, edgePipeline, bindGroup,
        postPipeline, postBindGroupLayout, linearSampler,
        totalFaceVertices, totalEdgeVertices, lastW: 0, lastH: 0
    };

    if (canvas._cam_rx === undefined) {
        canvas._cam_ry = _gpuCache.lastCamera.ry; 
        canvas._cam_rx = _gpuCache.lastCamera.rx; 
        canvas._cam_zoom = _gpuCache.lastCamera.zoom;
    }
    let dragging = false, lx = 0, ly = 0;
    if (!canvas._listenersAdded) {
        canvas.addEventListener("pointerdown", e => { if (canvas._viewLocked) return; dragging = true; lx = e.clientX; ly = e.clientY; });
        document.addEventListener("pointerup", () => dragging = false);
        document.addEventListener("pointermove", e => {
            if (!dragging || canvas._viewLocked) return;
            canvas._cam_ry -= (e.clientX - lx) * 0.01;
            canvas._cam_rx = Math.min(Math.max(canvas._cam_rx + (e.clientY - ly) * 0.01, 0.01), Math.PI/2 - 0.01);
            lx = e.clientX; ly = e.clientY;
            _gpuCache.lastCamera.ry = canvas._cam_ry;
            _gpuCache.lastCamera.rx = canvas._cam_rx;
        });
        canvas.addEventListener("wheel", e => { 
            if (!canvas._viewLocked) { 
                e.preventDefault(); 
                canvas._cam_zoom = Math.min(Math.max(canvas._cam_zoom + e.deltaY * 0.005, 1.5), 10); 
                _gpuCache.lastCamera.zoom = canvas._cam_zoom;
            } 
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
            out[12] = -(xx * ex + xy * ey + xz * ez); out[13] = -(yx * ex + yy * ey + yz * ez); out[14] = -(zx * ex + zy * ey + zz * ez); out[15] = 1;
            return out;
        }
    };

    function draw() {
        const state = canvas._wgpuState;
        if (!state) return;

        const w = Math.max(1, canvas.clientWidth);
        const h = Math.max(1, canvas.clientHeight);
        
        if (canvas.width !== w || canvas.height !== h || !state.msaaDepthTexture) { 
            canvas.width = w; canvas.height = h;
            if (state.msaaDepthTexture) state.msaaDepthTexture.destroy();
            state.msaaDepthTexture = device.createTexture({ size: [w, h], format: 'depth32float', sampleCount: 4, usage: GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.TEXTURE_BINDING });
            if (state.msaaColorTexture) state.msaaColorTexture.destroy();
            state.msaaColorTexture = device.createTexture({ size: [w, h], format: presentationFormat, sampleCount: 4, usage: GPUTextureUsage.RENDER_ATTACHMENT });
            if (state.resolvedColorTexture) state.resolvedColorTexture.destroy();
            state.resolvedColorTexture = device.createTexture({ size: [w, h], format: presentationFormat, sampleCount: 1, usage: GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_SRC });
            state.postBindGroup = device.createBindGroup({
                layout: state.postBindGroupLayout,
                entries: [
                    { binding: 0, resource: state.resolvedColorTexture.createView() },
                    { binding: 1, resource: state.msaaDepthTexture.createView() },
                    { binding: 2, resource: state.linearSampler },
                ]
            });
        }

        const proj = externalProj ? new Float32Array(externalProj) : new Float32Array([1.8, 0, 0, 0, 0, 2.4, 0, 0, 0, 0, -1, -1, 0, 0, -0.2, 0]);
        const camX = canvas._cam_zoom * Math.cos(canvas._cam_ry) * Math.cos(canvas._cam_rx);
        const camY = canvas._cam_zoom * Math.sin(canvas._cam_ry) * Math.cos(canvas._cam_rx);
        const camZ = canvas._cam_zoom * Math.sin(canvas._cam_rx);
        const view = new Float32Array(16);
        mat4.lookAt(view, [camX, camY, camZ], [0, 0, 0], [0, 0, 1]);

        device.queue.writeBuffer(state.uniformBuffer, 0, proj);
        device.queue.writeBuffer(state.uniformBuffer, 64, view);

        const commandEncoder = device.createCommandEncoder();
        const passEncoder = commandEncoder.beginRenderPass({
            colorAttachments: [{ view: state.msaaColorTexture.createView(), resolveTarget: state.resolvedColorTexture.createView(), clearValue: { r: 1, g: 1, b: 1, a: 1 }, loadOp: 'clear', storeOp: 'discard' }],
            depthStencilAttachment: { view: state.msaaDepthTexture.createView(), depthClearValue: 1, depthLoadOp: 'clear', depthStoreOp: 'store' },
        });
        passEncoder.setBindGroup(0, state.bindGroup);
        if (state.totalFaceVertices > 0) {
            passEncoder.setPipeline(state.facePipeline);
            passEncoder.setVertexBuffer(0, state.facePosBuffer);
            passEncoder.setVertexBuffer(1, state.faceColBuffer);
            passEncoder.draw(state.totalFaceVertices);
        }
        /*
        if (state.totalEdgeVertices > 0) {
            passEncoder.setPipeline(state.edgePipeline);
            passEncoder.setVertexBuffer(0, state.edgePosBuffer);
            passEncoder.setVertexBuffer(1, state.edgeColBuffer);
            passEncoder.draw(state.totalEdgeVertices);
        }
        */
        passEncoder.end();

        const postEncoder = commandEncoder.beginRenderPass({
            colorAttachments: [{ view: context.getCurrentTexture().createView(), clearValue: { r: 1, g: 1, b: 1, a: 1 }, loadOp: 'clear', storeOp: 'store' }]
        });
        postEncoder.setPipeline(state.postPipeline);
        postEncoder.setBindGroup(0, state.postBindGroup);
        postEncoder.draw(3);
        postEncoder.end();

        device.queue.submit([commandEncoder.finish()]);
        canvas._drawLoopId = requestAnimationFrame(draw);
    }
    draw();

    } catch (err) {
        console.error("WebGPU Init Error:", err);
    } finally {
        canvas._isInitializing = false;
    }
};

window.captureCanvasWebGPU = async (canvasId) => {
    const canvas = document.getElementById(canvasId);
    const state = canvas?._wgpuState;
    if (!state) return null;
    const { device, resolvedColorTexture } = state;
    const w = canvas.width, h = canvas.height;
    const bytesPerRow = Math.ceil((w * 4) / 256) * 256;
    const readBuffer = device.createBuffer({ size: bytesPerRow * h, usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ });
    const encoder = device.createCommandEncoder();
    encoder.copyTextureToBuffer({ texture: resolvedColorTexture }, { buffer: readBuffer, bytesPerRow }, [w, h]);
    device.queue.submit([encoder.finish()]);
    await readBuffer.mapAsync(GPUMapMode.READ);
    const data = new Uint8ClampedArray(readBuffer.getMappedRange());
    const tempCanvas = document.createElement('canvas');
    tempCanvas.width = w; tempCanvas.height = h;
    const ctx = tempCanvas.getContext('2d'), imageData = ctx.createImageData(w, h);
    for (let y = 0; y < h; y++) imageData.data.set(data.subarray(y * bytesPerRow, y * bytesPerRow + w * 4), y * w * 4);
    ctx.putImageData(imageData, 0, 0);
    const dataUrl = tempCanvas.toDataURL('image/png');
    readBuffer.unmap(); readBuffer.destroy();
    return dataUrl;
};
window.captureCanvasSVG = window.captureCanvasWebGPU;
window.export3DToSVG = (canvasId, filename) => { window.captureCanvasWebGPU(canvasId).then(d => { if (d) { const a = document.createElement("a"); a.href = d; a.download = filename.replace(".svg", ".png"); a.click(); } }); };
