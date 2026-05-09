module Hywe.Shaders

/// <summary>
/// Provides WGSL shader source code for the WebGPU rendering pipeline.
/// Includes compute shaders for vertex transformation and post-processing (SSAO).
/// </summary>

let computeWgsl = """
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
        edgeColOut[edgeColOffset + i*4u + 0u] = 1.0;
        edgeColOut[edgeColOffset + i*4u + 1u] = 1.0;
        edgeColOut[edgeColOffset + i*4u + 2u] = 1.0;
        edgeColOut[edgeColOffset + i*4u + 3u] = 1.0;
    }
}
"""

let renderWgsl = """
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
"""

let postProcessWgsl = """
@group(0) @binding(0) var colorTex: texture_2d<f32>;
@group(0) @binding(1) var depthTex: texture_depth_multisampled_2d;
@group(0) @binding(2) var samp: sampler;

struct PostVertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
};

@vertex
fn vs_post(@builtin(vertex_index) vertexIndex: u32) -> PostVertexOutput {
    var pos = array<vec2<f32>, 3>(
        vec2<f32>(-1.0, -1.0), vec2<f32>(3.0, -1.0), vec2<f32>(-1.0, 3.0)
    );
    var out: PostVertexOutput;
    out.position = vec4<f32>(pos[vertexIndex], 0.0, 1.0);
    out.uv = pos[vertexIndex] * 0.5 + 0.5;
    out.uv.y = 1.0 - out.uv.y;
    return out;
}

@fragment
fn fs_post(@builtin(position) pos: vec4<f32>) -> @location(0) vec4<f32> {
    let ipos = vec2<i32>(pos.xy);
    let texSize = vec2<i32>(textureDimensions(colorTex));
    
    let baseColor = textureLoad(colorTex, ipos, 0);
    let centerDepth = textureLoad(depthTex, ipos, 0);

    if (centerDepth >= 1.0) {
        return baseColor;
    }

    var occlusion = 0.0;
    let samples = 12;
    let radius = 3.5;
    
    for (var i = 0; i < samples; i++) {
        let angle = f32(i) * 6.28318 / f32(samples);
        let offset = vec2<i32>(vec2<f32>(cos(angle), sin(angle)) * radius);
        let samplePos = clamp(ipos + offset, vec2<i32>(0), texSize - vec2<i32>(1));
        let sampleDepth = textureLoad(depthTex, samplePos, 0);
        
        let diff = centerDepth - sampleDepth;
        if (diff > 0.0001 && diff < 0.05) {
            occlusion += 1.0;
        }
    }

    let ao = 1.0 - (occlusion / f32(samples)) * 0.75;
    return vec4<f32>(baseColor.rgb * ao, baseColor.a);
}
"""
