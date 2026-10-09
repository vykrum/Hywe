/// <summary>
/// Functional SVG title block framing generator.
/// Renders standardized architectural borders, project metadata compartments, and brand vector marks onto SVG drawings.
/// </summary>
module Titleblock

open System
open System.Globalization
open System.Text.RegularExpressions
open ModelTypes

/// <summary> SVG path definition data for the Hywe brand mark. </summary>
let private logoPath = "M 167 836 Q 167 850 179 857 L 279 915 Q 317 937 317 893 L 317 600 Q 317 575 342 575 L 500 575 Q 525 575 525 600 L 525 738 Q 525 788 575 788 L 748 788 Q 841 788 760 834 L 488 992 Q 450 1013 488 1035 L 588 1093 Q 600 1100 613 1093 L 1021 857 Q 1033 850 1033 836 L 1033 364 Q 1033 350 1021 343 L 921 285 Q 883 263 883 307 L 883 613 Q 883 638 858 638 L 700 638 Q 675 638 675 613 L 675 450 Q 675 425 650 425 L 430 425 Q 337 425 418 378 L 713 208 Q 750 187 713 165 L 613 104 Q 600 100 588 104 L 179 343 Q 167 350 167 364 L 167 836 Z"

/// <summary> Regex matching viewBox attribute in SVG markup. </summary>
let private viewBoxRegex = Regex(@"viewBox=[""']\s*([-\d.]+)\s+([-\d.]+)\s+([-\d.]+)\s+([-\d.]+)[""']", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)

/// <summary> Regex matching width attribute in SVG markup. </summary>
let private widthRegex = Regex(@"width=[""']\s*([-\d.]+)[""']", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)

/// <summary> Regex matching height attribute in SVG markup. </summary>
let private heightRegex = Regex(@"height=[""']\s*([-\d.]+)[""']", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)

/// <summary> Regex matching XML declaration headers. </summary>
let private xmlHeaderRegex = Regex(@"<\?xml[\s\S]*?\?>", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)

/// <summary> Regex matching DOCTYPE declarations. </summary>
let private docTypeRegex = Regex(@"<!DOCTYPE[\s\S]*?>", RegexOptions.IgnoreCase ||| RegexOptions.Compiled)

/// <summary>
/// Escapes special XML characters (&amp;, &lt;, &gt;, &quot;, &apos;) for safe embedding in SVG text nodes.
/// </summary>
/// <param name="str">Input text string to escape.</param>
/// <returns>XML-safe escaped string.</returns>
let private escapeXml (str: string) =
    match str with
    | null | "" -> ""
    | s ->
        s.Replace("&", "&amp;")
         .Replace("<", "&lt;")
         .Replace(">", "&gt;")
         .Replace("\"", "&quot;")
         .Replace("'", "&apos;")

/// <summary>
/// Active pattern parsing a float string using <see cref="CultureInfo.InvariantCulture"/>.
/// </summary>
let private (|Float|_|) (str: string) =
    match Double.TryParse(str, NumberStyles.Float, CultureInfo.InvariantCulture) with
    | true, v -> Some v
    | _ -> None

/// <summary>
/// Active pattern parsing a strictly positive float string using <see cref="CultureInfo.InvariantCulture"/>.
/// </summary>
let private (|PositiveFloat|_|) (str: string) =
    match Double.TryParse(str, NumberStyles.Float, CultureInfo.InvariantCulture) with
    | true, v when v > 0.0 -> Some v
    | _ -> None

/// <summary>
/// Attempts to parse viewBox attribute dimensions (<c>minX</c>, <c>minY</c>, <c>width</c>, <c>height</c>).
/// </summary>
let private parseViewBox (svgString: string) =
    match viewBoxRegex.Match(svgString) with
    | m when m.Success ->
        match m.Groups.[1].Value, m.Groups.[2].Value, m.Groups.[3].Value, m.Groups.[4].Value with
        | Float mx, Float my, PositiveFloat w, PositiveFloat h -> Some (mx, my, w, h)
        | Float mx, Float my, Float _, Float _ -> Some (mx, my, 800.0, 600.0)
        | _ -> None
    | _ -> None

/// <summary>
/// Parses explicit SVG width and height attributes as fallback dimensions when viewBox is absent.
/// </summary>
let private parseWidthHeight (svgString: string) =
    let parseAttr (regex: Regex) defaultVal =
        match regex.Match(svgString) with
        | m when m.Success ->
            match m.Groups.[1].Value with
            | PositiveFloat v -> v
            | _ -> defaultVal
        | _ -> defaultVal

    let w = parseAttr widthRegex 800.0
    let h = parseAttr heightRegex 600.0
    0.0, 0.0, w, h

/// <summary>
/// Extracts drawing dimensions (<c>minX</c>, <c>minY</c>, <c>width</c>, <c>height</c>) from SVG content.
/// </summary>
let private extractSvgDimensions (svgString: string) =
    parseViewBox svgString
    |> Option.defaultValue (parseWidthHeight svgString)

/// <summary>
/// Strips XML/DOCTYPE headers and extracts inner SVG element body contents.
/// </summary>
let private extractInnerContent (svgString: string) =
    let cleanHeader = xmlHeaderRegex.Replace(svgString, "")
    let cleanSvg = docTypeRegex.Replace(cleanHeader, "")

    let openTagEnd = cleanSvg.IndexOf('>')
    let closeTagStart = cleanSvg.LastIndexOf("</svg>", StringComparison.OrdinalIgnoreCase)
    
    match openTagEnd, closeTagStart with
    | op, cl when op >= 0 && cl > op -> cleanSvg.Substring(op + 1, cl - op - 1)
    | _ -> cleanSvg

/// <summary> Calculated drawing sheet geometry and scaled font parameters. </summary>
type private SheetLayout = {
    SheetWidth: float
    SheetHeight: float
    BorderMargin: float
    BorderWidth: string
    DividerWidth: string
    ShiftX: float
    ShiftY: float
    BarX: float
    BarY: float
    BarWidth: float
    BarHeight: float
    LogoX: float
    LogoY: float
    LogoSize: float
    WordmarkX: float
    ProjX: float
    AuthorX: float
    LevelX: float
    DateX: float
    LabelY: float
    ValueY: float
    WordmarkY: float
    UrlY: float
    TextPadX: float
    LabelFontSize: string
    ValueFontSize: string
    DateFontSize: string
    WordmarkFontSize: string
    WordmarkSpacing: string
    UrlFontSize: string
    UrlSpacing: string
}

/// <summary>
/// Calculates proportional sheet dimensions, drawing offsets, and compartment coordinates.
/// </summary>
let private computeLayout (origW: float) (origH: float) (minX: float) (minY: float) : SheetLayout =
    let inv = CultureInfo.InvariantCulture
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
    let sheetW = max minSheetW rawSheetW

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

    {
        SheetWidth = sheetW
        SheetHeight = sheetH
        BorderMargin = bMargin
        BorderWidth = String.Format(inv, "{0:0.0}", Math.Max(0.8, 1.2 * s))
        DividerWidth = String.Format(inv, "{0:0.0}", Math.Max(0.6, 1.0 * s))
        ShiftX = shiftX
        ShiftY = shiftY
        BarX = barX
        BarY = barY
        BarWidth = barW
        BarHeight = barH
        LogoX = logoX
        LogoY = logoY
        LogoSize = logoSize
        WordmarkX = wordmarkX
        ProjX = projX
        AuthorX = authorX
        LevelX = levelX
        DateX = dateX
        LabelY = barY + Math.Round(barH * 0.36)
        ValueY = barY + Math.Round(barH * 0.72)
        WordmarkY = barY + Math.Round(barH * 0.45)
        UrlY = barY + Math.Round(barH * 0.74)
        TextPadX = Math.Max(3.0, Math.Round(12.0 * s))
        LabelFontSize = String.Format(inv, "{0:0.0}px", 6.5 * s)
        ValueFontSize = String.Format(inv, "{0:0.0}px", 11.0 * s)
        DateFontSize = String.Format(inv, "{0:0.0}px", 10.5 * s)
        WordmarkFontSize = String.Format(inv, "{0:0.0}px", 11.5 * s)
        WordmarkSpacing = String.Format(inv, "{0:0.0}px", 1.5 * s)
        UrlFontSize = String.Format(inv, "{0:0.0}px", 7.5 * s)
        UrlSpacing = String.Format(inv, "{0:0.0}px", 0.5 * s)
    }

/// <summary>
/// Normalizes export metadata fields with default fallbacks.
/// </summary>
let private normalizeMetadata (meta: ExportMetadata) : ExportMetadata =
    let defaultIfBlank fallback text =
        match text with
        | null -> fallback
        | s when String.IsNullOrWhiteSpace s -> fallback
        | s -> s.Trim()
    {
        ProjectTitle = meta.ProjectTitle |> defaultIfBlank "Hywe Exploration"
        Author = meta.Author |> defaultIfBlank "Hywe Design Team"
        Date = meta.Date |> defaultIfBlank (DateTime.Now.ToString("yyyy-MM-dd"))
        Level = meta.Level |> defaultIfBlank "L0"
    }

/// <summary>
/// Applies an architectural title block, drawing border, and logo framing to an SVG string.
/// Pure functional transformation without side effects or JS interop dependencies.
/// </summary>
/// <param name="svgString">Input raw SVG string.</param>
/// <param name="meta">Export metadata snapshot (project title, author, date, level).</param>
/// <returns>Framed SVG string containing the architectural title block container.</returns>
let apply (svgString: string) (meta: ExportMetadata) : string =
    match svgString with
    | null | "" -> svgString
    | s when String.IsNullOrWhiteSpace s -> s
    | s when s.Contains("id=\"hywe-titleblock-container\"") -> s
    | s ->
        let inv = CultureInfo.InvariantCulture
        let normalizedMeta = normalizeMetadata meta
        let minX, minY, origW, origH = extractSvgDimensions s
        let innerContent = extractInnerContent s
        let layout = computeLayout origW origH minX minY

        let bW = layout.SheetWidth - layout.BorderMargin * 2.0
        let bH = layout.SheetHeight - layout.BorderMargin * 2.0

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
            layout.SheetWidth, layout.SheetHeight,
            layout.BorderMargin, layout.BorderMargin, bW, bH, layout.BorderWidth,
            layout.ShiftX, layout.ShiftY,
            innerContent,
            layout.BarX, layout.BarY, (layout.BarX + layout.BarWidth),
            layout.LogoX, layout.LogoY, layout.LogoSize,
            logoPath,
            layout.WordmarkX, (layout.BarY + layout.BarHeight), layout.DividerWidth,
            (layout.WordmarkX + layout.TextPadX), layout.WordmarkY, layout.WordmarkFontSize, layout.WordmarkSpacing,
            layout.UrlY, layout.UrlFontSize, layout.UrlSpacing,
            layout.ProjX,
            (layout.ProjX + layout.TextPadX), layout.LabelY, layout.LabelFontSize, layout.ValueY, layout.ValueFontSize, (escapeXml normalizedMeta.ProjectTitle),
            layout.AuthorX,
            (layout.AuthorX + layout.TextPadX), layout.ValueFontSize, (escapeXml normalizedMeta.Author),
            layout.LevelX,
            (layout.LevelX + layout.TextPadX), (escapeXml normalizedMeta.Level),
            layout.DateX,
            (layout.DateX + layout.TextPadX), layout.DateFontSize, (escapeXml normalizedMeta.Date))
