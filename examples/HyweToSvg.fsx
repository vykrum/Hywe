#r "../src/Hywe.Core/bin/Debug/net10.0/Hywe.Core.dll"
// Outside this repository, use the NuGet package instead:
// #r "nuget: Hywe.Core"

open System
open System.IO
open Hywe.Core

/// Converts a single floor's Coxels into a self-contained SVG string with auto-fitted viewBox.
let levelToSvg (scale: int) (rooms: Coxel.Cxl[]) : string =
    let palette = [| "#4e79a7"; "#f28e2b"; "#e15759"; "#76b7b2"; "#59a14f"; "#edc948"; "#b07aa1" |]

    let roomGeometries =
        rooms
        |> Array.map (fun cxl ->
            let _, _, z = Hexel.hxlCrd cxl.Base
            let pts =
                Coxel.cxlPrm cxl z
                |> Goxel.cleanPolygon cxl.Seqn
                |> Array.map (fun (x, y) -> x * scale, -y * scale)
            let cx, cy = Coxel.cxlCnt cxl
            cxl, pts, (cx * scale, -cy * scale))

    let allPts = roomGeometries |> Array.collect (fun (_, pts, _) -> pts)
    let minX, minY, maxX, maxY =
        if allPts.Length = 0 then -50, -50, 50, 50
        else Goxel.bounds allPts

    let pad = 20
    let vx, vy = minX - pad, minY - pad
    let vw, vh = max 40 (maxX - minX + pad * 2), max 40 (maxY - minY + pad * 2)

    let body =
        roomGeometries
        |> Array.mapi (fun i (cxl, pts, (cx, cy)) ->
            let color = palette.[i % palette.Length]
            let ptAttr = pts |> Array.map (fun (x, y) -> sprintf "%d,%d" x y) |> String.concat " "
            let label = Coxel.prpVlu cxl.Name
            sprintf """  <polygon points="%s" fill="%s" fill-opacity="0.32" stroke="%s" stroke-width="0.8" />
  <text x="%d" y="%d" font-size="4.5" font-weight="600" text-anchor="middle" dominant-baseline="middle" fill="#1a1a1a">%s</text>"""
                ptAttr color color cx cy label)
        |> String.concat "\n"

    sprintf """<svg xmlns="http://www.w3.org/2000/svg" viewBox="%d %d %d %d" style="background:#ffffff;font-family:system-ui,sans-serif">
%s
</svg>""" vx vy vw vh body

/// Compiles a .hyw specification and returns (levelElevation, svgString) pairs.
let hyweToSvgs (hyweCode: string) =
    let rooms, _, _, _ =
        Zaxel.generateMultiLevelLayout hyweCode "0,0" [||] None None None

    rooms
    |> Array.groupBy (fun cxl -> let _, _, z = Hexel.hxlCrd cxl.Base in z)
    |> Array.sortBy fst
    |> Array.map (fun (z, lvlRooms) -> z, levelToSvg 10 lvlRooms)

// --- Demo Execution ---
let defaultSpec =
    "L0(Q=VRCWEE/W=30/H=20)(1/20/Living)(1.1/10/Entry)(1.2/12/Kitchen)" +
    "L1(Q=HRCWNN/W=30/H=20)(1/15/MasterBed)(1.1/6/Ensuite)"

let spec =
    fsi.CommandLineArgs
    |> Array.tryItem 1
    |> Option.defaultValue defaultSpec

spec
|> hyweToSvgs
|> Array.iter (fun (z, svg) ->
    let fileName = sprintf "layout_L%d.svg" z
    File.WriteAllText(fileName, svg)
    printfn "Generated %s (%d bytes)" (Path.GetFullPath fileName) svg.Length)
