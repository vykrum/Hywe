namespace Hywe.Core

module Parse =
    open System
    open Microsoft.FSharp.Reflection
    open Hexel

    /// <summary> Represents a parsed level with its identifier/marker and its constituent blocks. </summary>
    type LevelData = {
        Marker: string
        Blocks: string[]
    }

    /// <summary> Parses a string into a discriminated union case safely. </summary>
    let tryParseUnion<'T> (s: string) : 'T option =
        if not (FSharpType.IsUnion typeof<'T>) then None
        else
            FSharpType.GetUnionCases typeof<'T>
            |> Array.tryFind (fun c -> c.Name.Equals(s, StringComparison.OrdinalIgnoreCase))
            |> Option.map (fun c -> FSharpValue.MakeUnion(c,[||]) :?> 'T)

    /// <summary> Active pattern for safe float parsing. </summary>
    let (|Float|_|) (s: string) =
        match Double.TryParse s with
        | true, v -> Some v
        | _ -> None

    /// <summary> Active pattern for safe integer parsing. </summary>
    let (|Int|_|) (s: string) =
        match Int32.TryParse s with
        | true, v -> Some v
        | _ -> None

    /// <summary> Recursively splits a Hywe string into levels using markers between parentheses. </summary>
    let splitIntoLevels (input: string) : LevelData[] =
        let rec loop marker blocks (remaining: string) =
            match remaining with
            | "" -> 
                match blocks with
                | [] -> []
                | _ -> [ { Marker = marker; Blocks = List.rev blocks |> List.toArray } ]
            | s when s.StartsWith("(") ->
                match s.IndexOf(')') with
                | -1 -> [] 
                | idx ->
                    let content = s.Substring(1, idx - 1)
                    let rest = s.Substring(idx + 1).Trim()
                    loop marker (content :: blocks) rest
            | s ->
                match s.IndexOf('(') with
                | -1 -> [] 
                | idx ->
                    let nextMarker = s.Substring(0, idx).Trim()
                    let rest = s.Substring(idx)
                    match blocks with
                    | [] -> loop nextMarker [] rest 
                    | _ -> { Marker = marker; Blocks = List.rev blocks |> List.toArray } :: loop nextMarker [] rest

        if String.IsNullOrWhiteSpace input then [||]
        else loop "" [] (input.Trim()) |> List.toArray

    /// <summary> Parses attribute blocks into a Map. </summary>
    let parseAttrs (block: string) =
        block.Split('/', StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose (fun token ->
            match token.Split('=', 2) with
            | [| k; v |] -> Some (k.Trim(), v.Trim())
            | _ -> None)
        |> Map.ofArray

    /// <summary> Legacy attribute extraction for a single block. </summary>
    let extractAttrsFromHyw (text: string) =
        let cleanText = text.Trim().Trim('|').Trim()
        match cleanText.IndexOf('('), cleanText.LastIndexOf(')') with
        | start, stop when start <> -1 && stop <> -1 && stop > start ->
            parseAttrs (cleanText.Substring(start + 1, stop - start - 1))
        | _ -> Map.empty

    /// <summary> Parses a comma-separated string of coordinates into a float array of points. </summary>
    let parsePolygon (s: string) : (float * float)[] =
        s.Split(',', StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose (fun x ->
            match Double.TryParse(x.Trim()) with
            | true, v -> Some v
            | _ -> None)
        |> fun nums ->
            match nums.Length % 2 with
            | 0 ->
                nums
                |> Array.chunkBySize 2
                |> Array.choose (function
                    | [| a; b |] -> Some (a, b)
                    | _ -> None)
            | _ -> [||]

    /// <summary> Parses a hyphen-separated string of polygons (islands). </summary>
    let parsePolyIslands (s: string) : (float * float)[][] =
        if String.IsNullOrWhiteSpace s then [||]
        else
            s.Split('-', StringSplitOptions.RemoveEmptyEntries)
            |> Array.map parsePolygon

    /// <summary> Converts float tuples to Point records with scaling. </summary>
    let polyToPoints scale (seg: (float*float)[]) =
        seg |> Array.map (fun (x,y) -> { Hexel.Point.X = int (System.Math.Round(x * scale)); Y = int (System.Math.Round(y * scale)) })

    let parseCoords value =
        parsePolygon value
        |> Array.map (fun (x, y) -> int (System.Math.Round(x)), int (System.Math.Round(y)))

    /// <summary> Parses multiple island polygons from a string. </summary>
    let parseIslands value =
        parsePolyIslands value
        |> Array.map (Array.map (fun (x, y) -> int (System.Math.Round(x)), int (System.Math.Round(y))))

    /// <summary> Parses hierarchy blocks into (id, value, label) using pattern matching. </summary>
    let parseHierarchy (block: string) =
        match block.Split('/', 3) with
        | [| id; Int v; lb |] -> Some (id.Trim(), v, lb.Trim())
        | _ -> None

    /// <summary> Organizes flat nodes into parent-child groups (e.g., [| parent; c1; c2 |]). </summary>
    let groupHierarchy (nodes: (string * int * string)[]) =
        let isChildOf (parentID: string) (childID: string) =
            childID.StartsWith(parentID + ".") && 
            not (childID.Substring(parentID.Length + 1).Contains("."))

        let groups = 
            nodes 
            |> Array.choose (fun (id, area, lb) ->
                let children = nodes |> Array.filter (fun (cid, _, _) -> isChildOf id cid)
                match children with
                | [||] -> 
                    // Only include as a group if it's the root (path ends in 1 or marker-1)
                    if id.EndsWith("1") && not (id.Contains(".")) then 
                        Some [| (id, area, lb) |]
                    else None
                | _ -> Some (Array.append [| (id, area, lb) |] children))
        
        groups |> Array.sortBy (fun group -> (Array.head group) |> fun (id, _, _) -> id)

    /// <summary> Extracts content from a single level with ID prefixing. </summary>
    let processLevel (level: LevelData) =
        match Array.toList level.Blocks with
        | [] -> Map.empty, [||]
        | attrBlock :: nodeBlocks ->
            let attrs = parseAttrs attrBlock
            let nodes = 
                nodeBlocks 
                |> List.choose parseHierarchy 
                |> List.map (fun (id, area, lb) -> 
                    let prefixedId = 
                        if String.IsNullOrWhiteSpace level.Marker then id 
                        else sprintf "%s-%s" level.Marker id
                    (prefixedId, area, lb)
                )
                |> List.toArray
            let tree = groupHierarchy nodes
            attrs, tree

    /// <summary> Injects or updates the sequence (Q) attribute in a Hywe string segment. </summary>
    let injectSqn (input: string) (lvl: int) (newSqn: string) =
        let levels = splitIntoLevels input
        match lvl >= 0 && lvl < levels.Length with
        | true ->
            let level = levels.[lvl]
            match Array.toList level.Blocks with
            | [] -> input 
            | attrBlock :: treeBlocks ->
                let attrs = parseAttrs attrBlock
                let newAttrs = attrs |> Map.add "Q" newSqn
                let newAttrBlock = 
                    newAttrs 
                    |> Map.toList 
                    |> List.map (fun (k, v) -> sprintf "%s=%s" k v) 
                    |> String.concat "/"
                let newBlocks = (newAttrBlock :: treeBlocks) |> List.map (sprintf "(%s)") |> String.concat ""
                
                levels 
                |> Array.mapi (fun i l -> 
                    match i = lvl with
                    | true -> sprintf "%s%s" l.Marker newBlocks
                    | false -> sprintf "%s%s" l.Marker (l.Blocks |> Array.map (sprintf "(%s)") |> String.concat ""))
                |> String.concat ""
        | false -> input

    /// <summary> Extracts the Q (Sequence) attribute from all levels in a Hywe string. </summary>
    let extractSequences (input: string) : Map<int, string> =
        splitIntoLevels input
        |> Array.mapi (fun i lvl ->
            match Array.tryHead lvl.Blocks with
            | Some attrBlock -> 
                let attrs = parseAttrs attrBlock
                attrs |> Map.tryFind "Q" |> Option.map (fun q -> (i, q))
            | None -> None
        )
        |> Array.choose id
        |> Map.ofArray
