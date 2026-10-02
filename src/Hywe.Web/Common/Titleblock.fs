module Titleblock

open System
open System.Globalization
open System.Text.RegularExpressions
open ModelTypes
    let private logoPath = "M 167 836 Q 167 850 179 857 L 279 915 Q 317 937 317 893 L 317 600 Q 317 575 342 575 L 500 575 Q 525 575 525 600 L 525 738 Q 525 788 575 788 L 748 788 Q 841 788 760 834 L 488 992 Q 450 1013 488 1035 L 588 1093 Q 600 1100 613 1093 L 1021 857 Q 1033 850 1033 836 L 1033 364 Q 1033 350 1021 343 L 921 285 Q 883 263 883 307 L 883 613 Q 883 638 858 638 L 700 638 Q 675 638 675 613 L 675 450 Q 675 425 650 425 L 430 425 Q 337 425 418 378 L 713 208 Q 750 187 713 165 L 613 104 Q 600 100 588 104 L 179 343 Q 167 350 167 364 L 167 836 Z"

    let private escapeXml (str: string) =
        if String.IsNullOrEmpty str then ""
        else
            str
                .Replace("&", "&amp;")
                .Replace("<", "&lt;")
                .Replace(">", "&gt;")
                .Replace("\"", "&quot;")
                .Replace("'", "&apos;")

    let private viewBoxRegex = Regex(@"viewBox=[""']\s*([-\d.]+)\s+([-\d.]+)\s+([-\d.]+)\s+([-\d.]+)[""']", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
    let private widthRegex = Regex(@"width=[""']\s*([-\d.]+)[""']", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
    let private heightRegex = Regex(@"height=[""']\s*([-\d.]+)[""']", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
    let private xmlHeaderRegex = Regex(@"<\?xml[\s\S]*?\?>", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
    let private docTypeRegex = Regex(@"<!DOCTYPE[\s\S]*?>", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)

    /// <summary>
    /// Applies an architectural title block, drawing border, and logo framing to an SVG string.
    /// Pure functional and executed in WebAssembly without JS interop overhead.
    /// </summary>
    let apply (svgString: string) (meta: ExportMetadata) : string =
        if String.IsNullOrWhiteSpace svgString then svgString
        elif svgString.Contains("id=\"hywe-titleblock-container\"") then svgString
        else
            let inv = CultureInfo.InvariantCulture
            let projectTitle = if String.IsNullOrWhiteSpace meta.ProjectTitle then "Hywe Exploration" else meta.ProjectTitle
            let author = if String.IsNullOrWhiteSpace meta.Author then "Hywe Design Team" else meta.Author
            let date = if String.IsNullOrWhiteSpace meta.Date then DateTime.Now.ToString("yyyy-MM-dd") else meta.Date
            let level = if String.IsNullOrWhiteSpace meta.Level then "L0" else meta.Level

            // Parse SVG dimensions
            let vbMatch = viewBoxRegex.Match(svgString)
            let minX, minY, origW, origH =
                if vbMatch.Success then
                    let parseOrDefault (g: Group) (def: float) =
                        match Double.TryParse(g.Value, NumberStyles.Float, inv) with
                        | true, v -> v
                        | _ -> def
                    let mx = parseOrDefault vbMatch.Groups.[1] 0.0
                    let my = parseOrDefault vbMatch.Groups.[2] 0.0
                    let w = parseOrDefault vbMatch.Groups.[3] 800.0
                    let h = parseOrDefault vbMatch.Groups.[4] 600.0
                    mx, my, (if w <= 0.0 then 800.0 else w), (if h <= 0.0 then 600.0 else h)
                else
                    let wMatch = widthRegex.Match(svgString)
                    let hMatch = heightRegex.Match(svgString)
                    let parseOrDefault (m: Match) (def: float) =
                        if m.Success then
                            match Double.TryParse(m.Groups.[1].Value, NumberStyles.Float, inv) with
                            | true, v -> v
                            | _ -> def
                        else def
                    let w = parseOrDefault wMatch 800.0
                    let h = parseOrDefault hMatch 600.0
                    0.0, 0.0, (if w <= 0.0 then 800.0 else w), (if h <= 0.0 then 600.0 else h)

            // Extract inner content of the SVG
            let cleanSvg = docTypeRegex.Replace(xmlHeaderRegex.Replace(svgString, ""), "")
            let openTagEnd = cleanSvg.IndexOf('>')
            let closeTagStart = cleanSvg.LastIndexOf("</svg>", StringComparison.OrdinalIgnoreCase)
            let innerContent =
                if openTagEnd <> -1 && closeTagStart <> -1 && closeTagStart > openTagEnd then
                    cleanSvg.Substring(openTagEnd + 1, closeTagStart - openTagEnd - 1)
                else
                    cleanSvg

            let sheetH = Math.Round(origH * 1.2)
            let barH = Math.Max(16.0, Math.Round(sheetH * 0.056))
            let s = barH / 42.0
            let bMargin = Math.Max(4.0, Math.Round(sheetH * 0.016))
            let padX = Math.Round(sheetH * 0.04)
            let rawSheetW = Math.Round(origW + padX * 2.0)

            let logoW = Math.Round(44.0 * s)
            let wordmarkW = Math.Round(100.0 * s)
            let dateW = Math.Round(105.0 * s)
            let levelW = Math.Round(85.0 * s)
            let minTextW = Math.Round(200.0 * s)
            let minBarW = logoW + wordmarkW + dateW + levelW + minTextW
            let minSheetW = minBarW + bMargin * 2.0
            let sheetW = if rawSheetW < minSheetW then minSheetW else rawSheetW

            let bW = sheetW - bMargin * 2.0
            let bH = sheetH - bMargin * 2.0

            let drawingAreaH = bH - barH
            let shiftX = Math.Round((sheetW - origW) / 2.0) - minX
            let shiftY = bMargin + Math.Round((drawingAreaH - origH) / 2.0) - minY

            let barX = bMargin
            let barY = bMargin + bH - barH
            let barW = bW

            let logoSize = Math.Round(22.0 * s)
            let logoX = barX + Math.Round((logoW - logoSize) / 2.0)
            let logoY = barY + Math.Round((barH - logoSize) / 2.0)

            let wordmarkX = barX + logoW
            let projX = wordmarkX + wordmarkW
            let dateX = barX + barW - dateW
            let levelX = dateX - levelW
            let availTextW = Math.Max(minTextW, levelX - projX)
            let projW = Math.Round(availTextW * 0.58)
            let authorX = projX + projW

            let labelFontSize = String.Format(inv, "{0:0.0}px", 6.5 * s)
            let valueFontSize = String.Format(inv, "{0:0.0}px", 11.0 * s)
            let dateFontSize = String.Format(inv, "{0:0.0}px", 10.5 * s)
            let wordmarkFontSize = String.Format(inv, "{0:0.0}px", 11.5 * s)
            let wordmarkSpacing = String.Format(inv, "{0:0.0}px", 1.5 * s)
            let urlFontSize = String.Format(inv, "{0:0.0}px", 7.5 * s)
            let urlSpacing = String.Format(inv, "{0:0.0}px", 0.5 * s)

            let labelY = barY + Math.Round(barH * 0.36)
            let valueY = barY + Math.Round(barH * 0.72)
            let wmY = barY + Math.Round(barH * 0.45)
            let urlY = barY + Math.Round(barH * 0.74)
            let textPadX = Math.Max(3.0, Math.Round(12.0 * s))
            let borderWidth = String.Format(inv, "{0:0.0}", Math.Max(0.8, 1.2 * s))
            let divWidth = String.Format(inv, "{0:0.0}", Math.Max(0.6, 1.0 * s))

            String.Format(inv, """<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {0:0} {1:0}" width="{0:0}" height="{1:0}">
    <defs>
        <style>
            .tb-font {{ font-family: 'Outfit', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; }}
        </style>
    </defs>
    <!-- Background -->
    <rect width="{0:0}" height="{1:0}" fill="#ffffff" />

    <!-- Drawing Sheet Border -->
    <rect x="{2:0}" y="{3:0}" width="{4:0}" height="{5:0}" fill="none" stroke="#0f172a" stroke-width="{6}" />

    <!-- Drawing Content -->
    <g id="drawing-content" transform="translate({7:0}, {8:0})">
        {9}
    </g>

    <!-- Single Row Bottom Titleblock (Proportionately Scaled) -->
    <g id="hywe-titleblock-container">
        <!-- Top Divider of the Bottom Bar -->
        <line x1="{10:0}" y1="{11:0}" x2="{12:0}" y2="{11:0}" stroke="#0f172a" stroke-width="{6}" />

        <!-- 1. Logo Compartment -->
        <svg x="{13:0}" y="{14:0}" width="{15:0}" height="{15:0}" viewBox="0 0 1200 1200">
            <path fill="#0f172a" d="{16}" />
        </svg>
        <line x1="{17:0}" y1="{11:0}" x2="{17:0}" y2="{18:0}" stroke="#e2e8f0" stroke-width="{19}" />

        <!-- 2. Wordmark Compartment: 'H Y W E' + 'www.hywe.in' -->
        <text x="{20:0}" y="{21:0}" class="tb-font" font-size="{22}" font-weight="700" fill="#0f172a" letter-spacing="{23}">H Y W E</text>
        <text x="{20:0}" y="{24:0}" class="tb-font" font-size="{25}" font-weight="500" fill="#64748b" letter-spacing="{26}">www.hywe.in</text>
        <line x1="{27:0}" y1="{11:0}" x2="{27:0}" y2="{18:0}" stroke="#e2e8f0" stroke-width="{19}" />

        <!-- 3. Project Title Compartment (Full / Untruncated) -->
        <text x="{28:0}" y="{29:0}" class="tb-font" font-size="{30}" font-weight="700" fill="#64748b" letter-spacing="0.8px">PROJECT</text>
        <text x="{28:0}" y="{31:0}" class="tb-font" font-size="{32}" font-weight="600" fill="#0f172a">{33}</text>
        <line x1="{34:0}" y1="{11:0}" x2="{34:0}" y2="{18:0}" stroke="#e2e8f0" stroke-width="{19}" />

        <!-- 4. Author Compartment (Full / Untruncated) -->
        <text x="{35:0}" y="{29:0}" class="tb-font" font-size="{30}" font-weight="700" fill="#64748b" letter-spacing="0.8px">AUTHOR</text>
        <text x="{35:0}" y="{31:0}" class="tb-font" font-size="{36}" font-weight="500" fill="#0f172a">{37}</text>
        <line x1="{38:0}" y1="{11:0}" x2="{38:0}" y2="{18:0}" stroke="#e2e8f0" stroke-width="{19}" />

        <!-- 5. Level Compartment -->
        <text x="{39:0}" y="{29:0}" class="tb-font" font-size="{30}" font-weight="700" fill="#64748b" letter-spacing="0.8px">LEVEL</text>
        <text x="{39:0}" y="{31:0}" class="tb-font" font-size="{32}" font-weight="600" fill="#0f172a">{40}</text>
        <line x1="{41:0}" y1="{11:0}" x2="{41:0}" y2="{18:0}" stroke="#e2e8f0" stroke-width="{19}" />

        <!-- 6. Date Compartment -->
        <text x="{42:0}" y="{29:0}" class="tb-font" font-size="{30}" font-weight="700" fill="#64748b" letter-spacing="0.8px">DATE</text>
        <text x="{42:0}" y="{31:0}" class="tb-font" font-size="{43}" font-weight="500" fill="#0f172a">{44}</text>
    </g>
</svg>""",
                sheetW, sheetH,
                bMargin, bMargin, bW, bH, borderWidth,
                shiftX, shiftY,
                innerContent,
                barX, barY, (barX + barW),
                logoX, logoY, logoSize,
                logoPath,
                wordmarkX, (barY + barH), divWidth,
                (wordmarkX + textPadX), wmY, wordmarkFontSize, wordmarkSpacing,
                urlY, urlFontSize, urlSpacing,
                projX,
                (projX + textPadX), labelY, labelFontSize, valueY, valueFontSize, (escapeXml projectTitle),
                authorX,
                (authorX + textPadX), valueFontSize, (escapeXml author),
                levelX,
                (levelX + textPadX), (escapeXml level),
                dateX,
                (dateX + textPadX), dateFontSize, (escapeXml date))
