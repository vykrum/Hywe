namespace Hywe.Core

module Layout =
    open System
    open Hexel
    open Coxel
    open Geometry
    open Parse

    /// <summary> Active pattern for safe integer parsing. </summary>
    let (|Int|_|) (s: string) =
        match Int32.TryParse s with
        | true, v -> Some v
        | _ -> None

    /// <summary> Scales node areas to fit boundary. Purely functional transformation. </summary>
    let applyReproportioning (boundaryArea: int64) (totalNodeArea: int) (rawTree: (string * int * string)[][]) (ratioOverride: float option) =
        let isUnbound = boundaryArea <= 0L
        
        let totalRequestedArea = float totalNodeArea

        let ratio = 
            match ratioOverride with
            | Some r -> r
            | None ->
                match isUnbound || totalRequestedArea <= 0.0 with
                | true -> 0.25
                | false -> (float boundaryArea / (totalRequestedArea * 4.0))
        
        let tree = 
            rawTree |> Array.map (fun row ->
                row |> Array.map (fun (id, area, lb) ->
                    let finalCount = 
                        match area > 0 with
                        | true -> max 1 (int (System.Math.Round(float area * ratio)))
                        | false -> 0
                    (Refid id, Count finalCount, Label lb)
                )
            )
        tree, ratio

    /// <summary> Core layout construction for a single level. </summary>
    let generateCxlLayout (attrs: Map<string, string>) (tree: (string * int * string)[][]) (entryAtrFallback: string) (seqOverride: Sqn option) (widthOverride: int option) (heightOverride: int option) (ouStrOverride: string option) (ilStrOverride: string option) (initialOcc : Hxl[]) (bsHxSetOverride: Cxl option) (ratioOverride: float option) (elvOverride: int option) : Cxl[] * (int*int)[][] * float =
        
        let seq = 
            seqOverride 
            |> Option.orElse (attrs |> Map.tryFind "Q" |> Option.bind tryParseUnion<Sqn>)
            |> Option.defaultValue VRCWEE
    
        let elv = elvOverride |> Option.defaultValue (attrs |> Map.tryFind "L" |> Option.bind (function Float v -> Some (int v) | _ -> None) |> Option.defaultValue 0)
        let bdWd = widthOverride |> Option.orElse (attrs |> Map.tryFind "W" |> Option.bind (function Int v -> Some v | _ -> None) |> Option.filter (fun v -> v > 0)) |> Option.defaultValue 0
        let bdHt = heightOverride |> Option.orElse (attrs |> Map.tryFind "H" |> Option.bind (function Int v -> Some v | _ -> None)) |> Option.defaultValue bdWd
        
        let bdOu =
            match attrs |> Map.tryFind "X" with
            | Some "1" -> [||]
            | _ ->
                match ouStrOverride with
                | Some ou when ou <> "" -> parseCoords ou
                | _ ->
                    match attrs |> Map.tryFind "O" with 
                    | Some a when a <> "" -> parseCoords a
                    | _ ->
                        match bsHxSetOverride with
                        | Some parent when bdWd > 0 -> cxlPrm parent elv |> Geometry.cleanPolygon parent.Seqn
                        | _ -> 
                            match bdWd > 0 with
                            | true -> parseCoords $"0,0,0,{bdHt},{bdWd},{bdHt},{bdWd},0"
                            | false -> [||]
    
        let bdIs =
            match ilStrOverride with
            | Some il when il <> "" -> parseIslands il
            | _ -> 
                match attrs |> Map.tryFind "I" with 
                | Some a -> parseIslands a 
                | None -> [||]
    
        let entryAtrRaw = attrs |> Map.tryFind "E" |> Option.defaultValue "0"
        let entryAtr = 
            if entryAtrRaw = "0" then "0"
            else
                entryAtrRaw.Split ','
                |> Array.map (fun s -> match System.Double.TryParse s with true, v -> string (int (Math.Round(v))) | _ -> s)
                |> String.concat ","

        let entryFallback =
            match entryAtrFallback.Split ',' with
            | [| Int x; Int y |] -> AV(x, y, elv)
            | _ -> 
                match bdWd with
                | 0 -> identity elv
                | _ -> AV(bdWd/2+2, bdHt/2+2, elv)
    
        let bndSet = 
            let ntArea = Geometry.polygonWithHolesArea bdOu bdIs
            if ntArea > 0L then 
                let allVtx = Array.append bdOu (bdIs |> Array.collect id)
                let minX = allVtx |> Array.minBy fst |> fst |> (fun x -> x - 2)
                let maxX = allVtx |> Array.maxBy fst |> fst |> (fun x -> x + 2)
                let minY = allVtx |> Array.minBy snd |> snd |> (fun y -> y - 2)
                let maxY = allVtx |> Array.maxBy snd |> snd |> (fun y -> y + 2)

                let blocked = ResizeArray<Hxl>()
                for x in minX .. maxX do
                    for y in minY .. maxY do
                        let pt = (x, y)
                        let isOutside = not (Geometry.pointInPolygon pt bdOu)
                        let isInsideIsland = bdIs |> Array.exists (Geometry.pointInPolygon pt)
                        if isOutside || isInsideIsland then
                            blocked.Add(RV(x, y, elv))
                blocked.ToArray()
            else [||]

        let ntArea = polygonWithHolesArea bdOu bdIs
        let occ = Array.append initialOcc bndSet |> hxlUni 1
    
        let totalRequestedArea = tree |> Array.concat |> Array.distinctBy (fun (id, _, _) -> id) |> Array.sumBy (fun (_, a, _) -> a)
        let scaledTree, finalRatio = applyReproportioning ntArea totalRequestedArea tree ratioOverride
        let flatEntries = scaledTree |> Array.concat
    
        let result =
            match Array.tryHead flatEntries with
            | None -> [||]
            | Some (id, ct, lb) ->
                let rec buildTree (tre : (Prp*Prp*Prp)[][]) acc occ =
                    match tre with 
                    | [||] -> acc
                    | _ ->
                        let group = Array.head tre
                        let parentId = group |> Array.head |> fun (i,_,_) -> i
                        match acc |> Array.tryFindIndex (fun x -> x.Rfid = parentId) with
                        | Some idx ->
                            let host = acc.[idx]
                            let updatedHost, newCxls, newOcc = coxelChildren seq elv host group occ
                            let nextAcc = Array.append (acc |> Array.mapi (fun i cx -> match i = idx with true -> updatedHost | false -> cx)) newCxls
                            buildTree (Array.tail tre) nextAcc newOcc
                        | None -> buildTree (Array.tail tre) acc occ
    
                let rootCxl, initialOcc = createBaseCoxel seq elv entryAtr entryFallback id ct lb occ bsHxSetOverride
                match flatEntries.Length < 2 with
                | true -> [| rootCxl |]
                | false -> buildTree scaledTree [| rootCxl |] initialOcc
    
        result, Array.append [|bdOu|] bdIs, finalRatio
