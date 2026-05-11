namespace Hywe.Core

/// <summary> 
/// Lexel (Lexical Engine) 
/// Highlights the lexical analysis and tokenization of Hywe strings.
/// </summary>
module Lexel =
    open System
    open System.Text.RegularExpressions
    
    // --- Data Structures ---

    type LexelAttributes = {
        Sequence: string
        Level: int
        Base: string
        Scale: float
        Entry: string
        OuterBoundary: string
        Islands: string
        Thickness: float
        Width: float option
        Height: float option
    }

    type LexelNode = { 
        Id: string
        Area: int
        Label: string 
        Extrusion: float option
        Base: string option
    }

    type LexelBlock = {
        Marker: string
        Attributes: LexelAttributes
        Tree: LexelNode list list // Grouped nodes
    }

    // --- Core Active Patterns ---

    let (|Int|_|) (s: string) = 
        match Int32.TryParse s with true, v -> Some v | _ -> None

    let (|Float|_|) (s: string) = 
        match Double.TryParse s with true, v -> Some v | _ -> None

    // --- Primary API ---

    /// <summary> 
    /// Processes a Hywe attribute block into a structured record.
    /// Handles Sequence (Q), Level (L), Base (B), Scale (X), Entry (E), OuterBoundary (O), etc.
    /// </summary>
    let parseAttributes (block: string) : LexelAttributes =
        let attrs = 
            block.Trim('(', ')').Split('/', StringSplitOptions.RemoveEmptyEntries)
            |> Array.choose (fun t -> 
                match t.Split('=', 2) with 
                | [| k; v |] -> Some (k.Trim(), v.Trim()) 
                | _ -> None)
            |> Map.ofArray

        let getVal k f d = attrs |> Map.tryFind k |> Option.bind f |> Option.defaultValue d

        {
            Sequence = attrs |> Map.tryFind "Q" |> Option.defaultValue "VRCCNE"
            Level = getVal "L" (|Int|_|) 0
            Base = attrs |> Map.tryFind "B" |> Option.defaultValue "0"
            Scale = getVal "X" (|Float|_|) 1.0
            Entry = attrs |> Map.tryFind "E" |> Option.defaultValue "0"
            OuterBoundary = attrs |> Map.tryFind "O" |> Option.defaultValue ""
            Islands = attrs |> Map.tryFind "I" |> Option.defaultValue ""
            Thickness = getVal "T" (|Float|_|) 3.0
            Width = attrs |> Map.tryFind "W" |> Option.bind (|Float|_|)
            Height = attrs |> Map.tryFind "H" |> Option.bind (|Float|_|)
        }

    /// <summary> 
    /// Processes Hywe node blocks and organizes them into a hierarchical tree.
    /// Handles (ID/Area/Label/Extrusion/B=Base) format and dot-notation grouping.
    /// </summary>
    let parseNodes (blocks: string list) : LexelNode list list =
        let nodes = 
            blocks |> List.choose (fun b ->
                let bits = b.Trim('(', ')').Split('/')
                match bits.Length with
                | 3 -> 
                    match bits.[1] with
                    | Int area -> Some { Id = bits.[0].Trim(); Area = area; Label = bits.[2].Trim(); Extrusion = None; Base = None }
                    | _ -> None
                | 4 ->
                    match bits.[1], bits.[3] with
                    | Int area, Float extr -> Some { Id = bits.[0].Trim(); Area = area; Label = bits.[2].Trim(); Extrusion = Some extr; Base = None }
                    | Int area, s when s.StartsWith "B=" -> Some { Id = bits.[0].Trim(); Area = area; Label = bits.[2].Trim(); Extrusion = None; Base = Some (s.Substring(2)) }
                    | _ -> None
                | 5 ->
                    match bits.[1], bits.[3], bits.[4] with
                    | Int area, Float extr, s when s.StartsWith "B=" -> 
                        Some { Id = bits.[0].Trim(); Area = area; Label = bits.[2].Trim(); Extrusion = Some extr; Base = Some (s.Substring(2)) }
                    | _ -> None
                | _ -> None)

        let isChild (p: string) (c: string) = 
            c.StartsWith(p + ".") && not (c.Substring(p.Length + 1).Contains("."))
        
        nodes 
        |> List.choose (fun n ->
            let children = nodes |> List.filter (fun c -> isChild n.Id c.Id)
            match children, n.Id.EndsWith("1"), n.Id.Contains(".") with
            | [], true, false -> Some (n :: children)
            | [], _, _ -> None
            | _ -> Some (n :: children))
        |> List.sortBy (fun g -> g.[0].Id)

    /// <summary> 
    /// Unified entry point for processing a single Xyxel segment (attributes + nodes).
    /// </summary>
    let processXyxel (input: string) =
        Regex.Matches(input.Trim(), @"\(([^)]*)\)")
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Value)
        |> Seq.toList
        |> function
            | [] -> None
            | attrBlock :: nodeBlocks ->
                Some {|
                    Attributes = parseAttributes attrBlock
                    Tree = parseNodes nodeBlocks
                |}

    /// <summary> 
    /// Processes a multi-marker Hywe string, splitting it into individual segments and processing each.
    /// Handles L# and N# markers.
    /// </summary>
    let processFullString (input: string) : LexelBlock list =
        Regex.Matches(input.Trim(), @"([^()]*?)((?:\([^)]*\))+(?=(?:[^()]*\(|$)))")
        |> Seq.cast<Match>
        |> Seq.choose (fun m ->
            let marker = m.Groups.[1].Value.Trim()
            let content = m.Groups.[2].Value.Trim()
            processXyxel content
            |> Option.map (fun res -> { Marker = marker; Attributes = res.Attributes; Tree = res.Tree }))
        |> Seq.toList

    /// <summary> 
    /// Utility to extract sequence mapping from a full Hywe string.
    /// </summary>
    let extractSequences (input: string) : Map<int, string> =
        processFullString input
        |> List.mapi (fun i s -> i, s.Attributes.Sequence)
        |> Map.ofList

    /// <summary> 
    /// Injects or updates a specific level's sequence (Q=) within a full Hywe string.
    /// Supports L# and N# markers.
    /// </summary>
    let injectSqn (input: string) (lvl: int) (sqn: string) : string =
        let markers = Regex.Split(input, @"(?=[LN]\d+)") |> Array.filter (not << String.IsNullOrWhiteSpace)
        if lvl >= 0 && lvl < markers.Length then
            let m = markers.[lvl]
            let newM = Regex.Replace(m, @"Q=[^/)]*", "Q=" + sqn)
            markers.[lvl] <- newM
            String.Concat markers
        else input
