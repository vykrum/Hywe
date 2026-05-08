// --- WebGPU implementation ---

const computeWgsl = `
struct Uniforms {
    cx: f32,
    cy: f32,
    scaleXY: f32,
    scaleZ: f32,
    lx: f32,
    ly: f32,
    lz: f32,
    numTris: u32,
    numWalls: u32,
    pad1: u32,
    pad2: u32,
    pad3: u32,
};

@group(0) @binding(0) var<uniform> uniforms: Uniforms;
@group(0) @binding(1) var<storage, read> triInput: array<f32>;
@group(0) @binding(2) var<storage, read> wallInput: array<f32>;
@group(0) @binding(3) var<storage, read_write> facePosOut: array<f32>;
@group(0) @binding(4) var<storage, read_write> faceColOut: array<f32>;
@group(0) @binding(5) var<storage, read_write> edgePosOut: array<f32>;
@group(0) @binding(6) var<storage, read_write> edgeColOut: array<f32>;

@compute @workgroup_size(64)
fn computeTris(@builtin(global_invocation_id) id: vec3<u32>) {
    let idx = id.x;
    if (idx >= uniforms.numTris) { return; }

    let inOffset = idx * 11u;
    let x1 = triInput[inOffset + 0u];
    let y1 = triInput[inOffset + 1u];
    let x2 = triInput[inOffset + 2u];
    let y2 = triInput[inOffset + 3u];
    let x3 = triInput[inOffset + 4u];
    let y3 = triInput[inOffset + 5u];
    let r = triInput[inOffset + 6u];
    let g = triInput[inOffset + 7u];
    let b = triInput[inOffset + 8u];
    let baseH = triInput[inOffset + 9u];
    let height = triInput[inOffset + 10u];

    let nx1 = (x1 - uniforms.cx) * uniforms.scaleXY;
    let ny1 = (y1 - uniforms.cy) * uniforms.scaleXY;
    let nx2 = (x2 - uniforms.cx) * uniforms.scaleXY;
    let ny2 = (y2 - uniforms.cy) * uniforms.scaleXY;
    let nx3 = (x3 - uniforms.cx) * uniforms.scaleXY;
    let ny3 = (y3 - uniforms.cy) * uniforms.scaleXY;
    let nzTop = baseH + height;
    let nzBot = baseH;

    let outPosOffset = idx * 18u;
    let outColOffset = idx * 24u;

    facePosOut[outPosOffset + 0u] = nx1; facePosOut[outPosOffset + 1u] = ny1; facePosOut[outPosOffset + 2u] = nzTop;
    facePosOut[outPosOffset + 3u] = nx2; facePosOut[outPosOffset + 4u] = ny2; facePosOut[outPosOffset + 5u] = nzTop;
    facePosOut[outPosOffset + 6u] = nx3; facePosOut[outPosOffset + 7u] = ny3; facePosOut[outPosOffset + 8u] = nzTop;

    facePosOut[outPosOffset + 9u]  = nx3; facePosOut[outPosOffset + 10u] = ny3; facePosOut[outPosOffset + 11u] = nzBot;
    facePosOut[outPosOffset + 12u] = nx2; facePosOut[outPosOffset + 13u] = ny2; facePosOut[outPosOffset + 14u] = nzBot;
    facePosOut[outPosOffset + 15u] = nx1; facePosOut[outPosOffset + 16u] = ny1; facePosOut[outPosOffset + 17u] = nzBot;

    let topCol = vec3<f32>(r, g, b) * 0.95;
    let botCol = vec3<f32>(r, g, b) * 0.85;

    for (var i = 0u; i < 3u; i++) {
        faceColOut[outColOffset + i*4u + 0u] = topCol.r;
        faceColOut[outColOffset + i*4u + 1u] = topCol.g;
        faceColOut[outColOffset + i*4u + 2u] = topCol.b;
        faceColOut[outColOffset + i*4u + 3u] = 1.0;
        
        faceColOut[outColOffset + 12u + i*4u + 0u] = botCol.r;
        faceColOut[outColOffset + 12u + i*4u + 1u] = botCol.g;
        faceColOut[outColOffset + 12u + i*4u + 2u] = botCol.b;
        faceColOut[outColOffset + 12u + i*4u + 3u] = 1.0;
    }
}

@compute @workgroup_size(64)
fn computeWalls(@builtin(global_invocation_id) id: vec3<u32>) {
    let idx = id.x;
    if (idx >= uniforms.numWalls) { return; }

    let inOffset = idx * 9u;
    let x1 = wallInput[inOffset + 0u];
    let y1 = wallInput[inOffset + 1u];
    let x2 = wallInput[inOffset + 2u];
    let y2 = wallInput[inOffset + 3u];
    let r = wallInput[inOffset + 4u];
    let g = wallInput[inOffset + 5u];
    let b = wallInput[inOffset + 6u];
    let baseH = wallInput[inOffset + 7u];
    let height = wallInput[inOffset + 8u];

    let nx1 = (x1 - uniforms.cx) * uniforms.scaleXY;
    let ny1 = (y1 - uniforms.cy) * uniforms.scaleXY;
    let nx2 = (x2 - uniforms.cx) * uniforms.scaleXY;
    let ny2 = (y2 - uniforms.cy) * uniforms.scaleXY;
    let nzTop = baseH + height;
    let nzBot = baseH;

    let dx = nx2 - nx1;
    let dy = ny2 - ny1;
    let wallLen = sqrt(dx*dx + dy*dy);
    let wnx = dy / wallLen;
    let wny = -dx / wallLen;
    let dotP = max(0.0, wnx * uniforms.lx + wny * uniforms.ly);
    let wallShade = 0.7 + 0.25 * dotP;

    let col = vec3<f32>(r, g, b) * wallShade;

    let facePosOffset = (uniforms.numTris * 18u) + (idx * 18u);
    let faceColOffset = (uniforms.numTris * 24u) + (idx * 24u);

    facePosOut[facePosOffset + 0u] = nx1; facePosOut[facePosOffset + 1u] = ny1; facePosOut[facePosOffset + 2u] = nzBot;
    facePosOut[facePosOffset + 3u] = nx2; facePosOut[facePosOffset + 4u] = ny2; facePosOut[facePosOffset + 5u] = nzTop;
    facePosOut[facePosOffset + 6u] = nx1; facePosOut[facePosOffset + 7u] = ny1; facePosOut[facePosOffset + 8u] = nzTop;

    facePosOut[facePosOffset + 9u]  = nx1; facePosOut[facePosOffset + 10u] = ny1; facePosOut[facePosOffset + 11u] = nzBot;
    facePosOut[facePosOffset + 12u] = nx2; facePosOut[facePosOffset + 13u] = ny2; facePosOut[facePosOffset + 14u] = nzBot;
    facePosOut[facePosOffset + 15u] = nx2; facePosOut[facePosOffset + 16u] = ny2; facePosOut[facePosOffset + 17u] = nzTop;

    for (var i = 0u; i < 6u; i++) {
        faceColOut[faceColOffset + i*4u + 0u] = col.r;
        faceColOut[faceColOffset + i*4u + 1u] = col.g;
        faceColOut[faceColOffset + i*4u + 2u] = col.b;
        faceColOut[faceColOffset + i*4u + 3u] = 1.0;
    }

    let edgePosOffset = idx * 24u;
    let edgeColOffset = idx * 32u;

    edgePosOut[edgePosOffset + 0u] = nx1; edgePosOut[edgePosOffset + 1u] = ny1; edgePosOut[edgePosOffset + 2u] = nzTop;
    edgePosOut[edgePosOffset + 3u] = nx2; edgePosOut[edgePosOffset + 4u] = ny2; edgePosOut[edgePosOffset + 5u] = nzTop;

    edgePosOut[edgePosOffset + 6u] = nx1; edgePosOut[edgePosOffset + 7u] = ny1; edgePosOut[edgePosOffset + 8u] = nzBot;
    edgePosOut[edgePosOffset + 9u] = nx2; edgePosOut[edgePosOffset + 10u] = ny2; edgePosOut[edgePosOffset + 11u] = nzBot;

    edgePosOut[edgePosOffset + 12u] = nx1; edgePosOut[edgePosOffset + 13u] = ny1; edgePosOut[edgePosOffset + 14u] = nzBot;
    edgePosOut[edgePosOffset + 15u] = nx1; edgePosOut[edgePosOffset + 16u] = ny1; edgePosOut[edgePosOffset + 17u] = nzTop;

    edgePosOut[edgePosOffset + 18u] = nx2; edgePosOut[edgePosOffset + 19u] = ny2; edgePosOut[edgePosOffset + 20u] = nzBot;
    edgePosOut[edgePosOffset + 21u] = nx2; edgePosOut[edgePosOffset + 22u] = ny2; edgePosOut[edgePosOffset + 23u] = nzTop;

    for (var i = 0u; i < 8u; i++) {
        edgeColOut[edgeColOffset + i*4u + 0u] = 0.4;
        edgeColOut[edgeColOffset + i*4u + 1u] = 0.4;
        edgeColOut[edgeColOffset + i*4u + 2u] = 0.4;
        edgeColOut[edgeColOffset + i*4u + 3u] = 1.0;
    }
}
`;

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
    if (state.facePosBuffer) state.facePosBuffer.destroy();
    if (state.faceColBuffer) state.faceColBuffer.destroy();
    if (state.edgePosBuffer) state.edgePosBuffer.destroy();
    if (state.edgeColBuffer) state.edgeColBuffer.destroy();
    if (state.triInputBuffer) state.triInputBuffer.destroy();
    if (state.wallInputBuffer) state.wallInputBuffer.destroy();
    if (state.computeUniformBuffer) state.computeUniformBuffer.destroy();
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

    // --- Compute Shader Data Preparation ---
    let numTris = 0;
    meshes.forEach(tris => numTris += tris.length);
    let numWalls = 0;
    if (edgePolygons) edgePolygons.forEach(poly => numWalls += poly.length);

    const triInputData = new Float32Array(numTris * 11);
    const wallInputData = new Float32Array(numWalls * 9);

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
            }
        });
    }

    const getStorageBuffer = (data, minSize) => {
        let size = data.byteLength;
        if (size === 0) size = minSize || 16;
        size = Math.ceil(size / 4) * 4; // 4-byte aligned
        const buf = device.createBuffer({
            size,
            usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST,
        });
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

    const computeUniformBuffer = device.createBuffer({
        size: 48,
        usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
    });
    device.queue.writeBuffer(computeUniformBuffer, 0, computeUniformData);

    const totalFaceVertices = numTris * 6 + numWalls * 6;
    const facePosBuffer = device.createBuffer({
        size: Math.max(16, totalFaceVertices * 3 * 4),
        usage: GPUBufferUsage.VERTEX | GPUBufferUsage.STORAGE,
    });
    const faceColBuffer = device.createBuffer({
        size: Math.max(16, totalFaceVertices * 4 * 4),
        usage: GPUBufferUsage.VERTEX | GPUBufferUsage.STORAGE,
    });

    const totalEdgeVertices = numWalls * 8;
    const edgePosBuffer = device.createBuffer({
        size: Math.max(16, totalEdgeVertices * 3 * 4),
        usage: GPUBufferUsage.VERTEX | GPUBufferUsage.STORAGE,
    });
    const edgeColBuffer = device.createBuffer({
        size: Math.max(16, totalEdgeVertices * 4 * 4),
        usage: GPUBufferUsage.VERTEX | GPUBufferUsage.STORAGE,
    });

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

    // --- Render Pipeline setup ---
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
        facePosBuffer, faceColBuffer, edgePosBuffer, edgeColBuffer, 
        triInputBuffer, wallInputBuffer, computeUniformBuffer, uniformBuffer,
        facePipeline, edgePipeline, bindGroup,
        totalFaceVertices, totalEdgeVertices
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

        if (canvas._wgpuState.totalFaceVertices > 0) {
            passEncoder.setPipeline(facePipeline);
            passEncoder.setVertexBuffer(0, canvas._wgpuState.facePosBuffer);
            passEncoder.setVertexBuffer(1, canvas._wgpuState.faceColBuffer);
            passEncoder.draw(canvas._wgpuState.totalFaceVertices);
        }

        // Edge lines removed per request
        // if (canvas._wgpuState.totalEdgeVertices > 0) {
        //     passEncoder.setPipeline(edgePipeline);
        //     passEncoder.setVertexBuffer(0, canvas._wgpuState.edgePosBuffer);
        //     passEncoder.setVertexBuffer(1, canvas._wgpuState.edgeColBuffer);
        //     passEncoder.draw(canvas._wgpuState.totalEdgeVertices);
        // }

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
