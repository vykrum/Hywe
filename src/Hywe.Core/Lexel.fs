namespace Hywe.Core

/// <summary> 
/// Lexel (Lexical Engine) 
/// Parses the lexical analysis and tokenization of Hywe syntax.
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

    type LevelBlock = {
        Marker: string
        Attributes: LexelAttributes
        Tree: LexelNode list list // Grouped nodes
    }

    type NestBlock = {
        Marker: string
        Attributes: LexelAttributes
        Tree: LexelNode list list // Grouped nodes
    }

    type LexelBlock =
        | Level of LevelBlock
        | Nest of NestBlock

    // --- Core Active Patterns ---

    let (|Int|_|) (s: string) = 
        match Int32.TryParse s with true, v -> Some v | _ -> None

    let (|Float|_|) (s: string) = 
        match Double.TryParse s with true, v -> Some v | _ -> None

    // --- Primary API ---

    /// <summary> 
    /// Processes a Hywe attribute block into a structured record.
    /// Handles Sequence (Q), Level (L), Base (B), Scale (X), Entry (E), OuterBoundary (O), Islands (I), Thickness (T), Width (W), and Height (H).
    /// </summary>
    /// <param name="block">Raw string of the attribute block, e.g. "(Q=VRCWEE/L=0/W=20/H=30)".</param>
    /// <returns>A populated LexelAttributes record.</returns>
    /// <example>
    /// <code>
    /// let attrs = Lexel.parseAttributes "(Q=VRCWEE/L=0/X=1.0/W=20/H=30)"
    /// </code>
    /// </example>
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
    /// <param name="markerPrefix">Prefix marker (e.g. "L0" or "N1") prepended to generated node IDs.</param>
    /// <param name="blocks">List of node block strings, e.g. ["(1/25/Living)"; "(1.1/12/Dining)"].</param>
    /// <returns>A list of node groups forming the hierarchical spatial tree.</returns>
    /// <example>
    /// <code>
    /// let blocks = [ "(1/25/Living)"; "(1.1/12/Dining)"; "(1.2/8/Kitchen)" ]
    /// let tree = Lexel.parseNodes "L0" blocks
    /// </code>
    /// </example>
    let parseNodes (markerPrefix: string) (blocks: string list) : LexelNode list list =
        let nodes = 
            blocks |> List.choose (fun b ->
                let bits = b.Trim('(', ')').Split('/')
                match bits.Length with
                | 3 -> 
                    match bits.[1] with
                    | Int area -> Some { Id = $"{markerPrefix}.{bits.[0].Trim()}"; Area = area; Label = bits.[2].Trim(); Extrusion = None; Base = None }
                    | _ -> None
                | 4 ->
                    match bits.[1], bits.[3] with
                    | Int area, Float extr -> Some { Id = $"{markerPrefix}.{bits.[0].Trim()}"; Area = area; Label = bits.[2].Trim(); Extrusion = Some extr; Base = None }
                    | Int area, s when s.StartsWith "B=" -> Some { Id = $"{markerPrefix}.{bits.[0].Trim()}"; Area = area; Label = bits.[2].Trim(); Extrusion = None; Base = Some (s.Substring(2)) }
                    | _ -> None
                | 5 ->
                    match bits.[1], bits.[3], bits.[4] with
                    | Int area, Float extr, s when s.StartsWith "B=" -> 
                        Some { Id = $"{markerPrefix}.{bits.[0].Trim()}"; Area = area; Label = bits.[2].Trim(); Extrusion = Some extr; Base = Some (s.Substring(2)) }
                    | _ -> None
                | _ -> None)

        let isChild (p: string) (c: string) = 
            c.StartsWith(p + ".") && not (c.Substring(p.Length + 1).Contains("."))
        
        nodes 
        |> List.choose (fun n ->
            let children = nodes |> List.filter (fun c -> isChild n.Id c.Id)
            let origId = n.Id.Substring(markerPrefix.Length + 1)
            match children, origId = "1", origId.Contains(".") with
            | [], true, false -> Some (n :: children)
            | [], _, _ -> None
            | _ -> Some (n :: children))
        |> List.sortBy (fun g -> g.[0].Id)

    /// <summary> 
    /// Unified entry point for parsing and processing a single Xyxel segment containing attributes and node blocks.
    /// </summary>
    /// <param name="markerPrefix">Level or nest marker prefix (e.g. "L0").</param>
    /// <param name="input">Raw segment input containing parenthesis-delimited blocks.</param>
    /// <returns>Option containing the parsed Attributes and Tree.</returns>
    /// <example>
    /// <code>
    /// let segment = Lexel.processXyxel "L0" "(Q=VRCWEE/W=30)(1/20/Hall)(1.1/10/Room)"
    /// </code>
    /// </example>
    let processXyxel (markerPrefix: string) (input: string) =
        Regex.Matches(input.Trim(), @"\(([^)]*)\)")
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Value)
        |> Seq.toList
        |> function
            | [] -> None
            | attrBlock :: nodeBlocks ->
                Some {|
                    Attributes = parseAttributes attrBlock
                    Tree = parseNodes markerPrefix nodeBlocks
                |}

    /// <summary> 
    /// Processes a full multi-marker Hywe specification string, splitting into individual level (L#) and nest (N#) segments.
    /// </summary>
    /// <param name="input">Full multi-level Hywe string.</param>
    /// <returns>List of parsed LexelBlock items (Level or Nest).</returns>
    /// <example>
    /// <code>
    /// let spec = "L0(Q=VRCWEE)(1/20/Hall)L1(Q=HRCWNN)(1/15/Master)"
    /// let blocks = Lexel.processFullString spec
    /// </code>
    /// </example>
    let processFullString (input: string) : LexelBlock list =
        Regex.Matches(input.Trim(), @"([^()]*?)((?:\([^)]*\))+(?=(?:[^()]*\(|$)))")
        |> Seq.cast<Match>
        |> Seq.choose (fun m ->
            let markerRaw = m.Groups.[1].Value.Trim()
            let marker = match String.IsNullOrEmpty(markerRaw) with | true -> "L0" | false -> markerRaw
            let content = m.Groups.[2].Value.Trim()
            processXyxel marker content
            |> Option.map (fun res -> 
                match marker.StartsWith("N") with
                | true -> Nest { Marker = marker; Attributes = res.Attributes; Tree = res.Tree }
                | false -> Level { Marker = marker; Attributes = res.Attributes; Tree = res.Tree }
            ))
        |> Seq.toList

    /// <summary> 
    /// Extracts a level index to traversal sequence string mapping from a full Hywe string.
    /// </summary>
    /// <param name="input">Full multi-level Hywe string.</param>
    /// <returns>Map from level index (0-based) to sequence name (e.g. 0 -> "VRCWEE").</returns>
    /// <example>
    /// <code>
    /// let sqnMap = Lexel.extractSequences "L0(Q=VRCWEE)(1/10/Room)L1(Q=HRCWNN)(1/10/Office)"
    /// </code>
    /// </example>
    let extractSequences (input: string) : Map<int, string> =
        processFullString input
        |> List.mapi (fun i s -> 
            let attrs = match s with | Level l -> l.Attributes | Nest n -> n.Attributes
            i, attrs.Sequence)
        |> Map.ofList

    /// <summary> 
    /// Injects or updates a specific level's sequence (Q=) within a full Hywe string.
    /// Supports both level (L#) and nest (N#) markers.
    /// </summary>
    /// <param name="input">Full Hywe specification string.</param>
    /// <param name="lvl">Zero-based level index to modify.</param>
    /// <param name="sqn">New traversal sequence string (e.g. "HRCWNN").</param>
    /// <returns>Modified specification string with updated Q= sequence parameter.</returns>
    /// <example>
    /// <code>
    /// let updatedSpec = Lexel.injectSqn "L0(Q=VRCWEE)(1/10/Room)" 0 "HRCWNN"
    /// </code>
    /// </example>
    let injectSqn (input: string) (lvl: int) (sqn: string) : string =
        let markers = Regex.Split(input, @"(?=[LN]\d+)") |> Array.filter (not << String.IsNullOrWhiteSpace)
        match lvl >= 0 && lvl < markers.Length with
        | true ->
            markers 
            |> Array.mapi (fun i m ->
                match i = lvl with
                | true -> 
                    if Regex.IsMatch(m, @"Q=[^/)]*") then
                        Regex.Replace(m, @"Q=[^/)]*", "Q=" + sqn)
                    elif Regex.IsMatch(m, @"^([LN]\d+)\(([^)]*)\)") then
                        Regex.Replace(m, @"^([LN]\d+)\(([^)]*)\)", fun (matched: Match) ->
                            let marker = matched.Groups.[1].Value
                            let inner = matched.Groups.[2].Value
                            if String.IsNullOrWhiteSpace inner then $"{marker}(Q={sqn})"
                            else $"{marker}(Q={sqn}/{inner})"
                        )
                    elif Regex.IsMatch(m, @"^([LN]\d+)") then
                        Regex.Replace(m, @"^([LN]\d+)", $"$1(Q={sqn})")
                    else m
                | false -> m
            )
            |> String.concat ""
        | false -> input
