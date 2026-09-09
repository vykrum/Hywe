namespace Hywe.Node

open System

module Coloring =

    let private clamp (value: int) : int = min 255 (max 0 value)

    let hexToRgb (hex: string) : int * int * int =
        let cleanHex = hex.TrimStart('#')
        let r = Convert.ToInt32(cleanHex.Substring(0, 2), 16)
        let g = Convert.ToInt32(cleanHex.Substring(2, 2), 16)
        let b = Convert.ToInt32(cleanHex.Substring(4, 2), 16)
        (r, g, b)

    /// Deterministic pastel generator — first color is base color
    let generatePastels (rootHex: string) (count: int) (opacity: float) : string[] =
        let (baseR, baseG, baseB) = hexToRgb rootHex

        [| 0 .. count - 1 |]
        |> Array.map (fun i ->
            match i = 0 with
            | true ->
                $"rgba({baseR}, {baseG}, {baseB}, {opacity})"
            | false ->
                let hueShift = (i * 137) % 360
                let angleRad = float hueShift * Math.PI / 180.0

                let vary cmpnent phase =
                    let offset = int (40.0 * Math.Sin(angleRad + phase))
                    clamp ((cmpnent + offset + 255) >>> 1)

                let r = vary baseR 0.0
                let g = vary baseG 2.0
                let b = vary baseB 4.0

                $"rgba({r}, {g}, {b}, {opacity})"
        )

    let rec private flattenTree (node: TreeNode) : TreeNode list =
        node :: (node.Children |> List.collect flattenTree)

    let rec private mapTree (f: TreeNode -> TreeNode) (node: TreeNode) : TreeNode =
        let updated = f node
        { updated with Children = updated.Children |> List.map (mapTree f) }

    /// Returns tree with default white nodes (None)
    let colorTree (root: TreeNode) : TreeNode =
        root |> mapTree (fun n -> { n with Color = None })

    /// Colors an entire SubModel associating colors strictly with action states:
    /// Blue for elevated nodes, Green for nested nodes, and None (white) for standard nodes.
    let colorModel (model: SubModel) : SubModel =
        let nestAnchorIds =
            model.NestAnchors
            |> Map.toSeq
            |> Seq.map snd
            |> Set.ofSeq

        let applyColor (lvl: int) (n: TreeNode) =
            let isElevatedAnchor = model.LevelAnchors |> Map.exists (fun targetLvl aId -> targetLvl > lvl && aId = n.Id)
            let isAnchorForThisLevel = lvl > 0 && (model.LevelAnchors |> Map.tryFind lvl = Some n.Id)
            let isElevated = n.Level > lvl || isElevatedAnchor || isAnchorForThisLevel
            let isNested = nestAnchorIds.Contains n.Id

            let color = 
                if isElevated then Some "#3498db"
                elif isNested then Some "#2ecc71"
                else None

            { n with Color = color }

        let rec updateTree lvl node =
            let updated = applyColor lvl node
            { updated with Children = updated.Children |> List.map (updateTree lvl) }

        let coloredLevels = model.Levels |> Map.map (fun lvl tree -> updateTree lvl tree)
        let coloredNests = model.Nests |> Map.map (fun _ tree -> updateTree 0 tree)

        { model with Levels = coloredLevels; Nests = coloredNests }
