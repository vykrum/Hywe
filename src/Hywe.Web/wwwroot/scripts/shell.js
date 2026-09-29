// @ts-nocheck
// ============================================================================
// Hywe Shell & Global Interop Utilities
// Unified browser runtime bridge for PWA lifecycle, DOM interaction,
// SVG serialization, URL hash state, and service integrations.
// ============================================================================

// --- Service Worker Registration ---
if ('serviceWorker' in navigator) {
    window.addEventListener('load', () => {
        navigator.serviceWorker.register('/service-worker.js')
            .then(reg => console.log('Service worker registered:', reg))
            .catch(err => console.error('Service worker registration failed:', err));
    });
}

// --- URL Hash & LZ-String State Management ---
window.getUrlHash = function () {
    let hash = "";
    if (window.location.hash && window.location.hash.length > 1) {
        hash = window.location.hash.substring(1);
    } else {
        const url = window.location.href;
        const idx = url.indexOf('#');
        if (idx !== -1) hash = url.substring(idx + 1);
    }
    if (hash) {
        try {
            return window.LZString ? LZString.decompressFromEncodedURIComponent(hash) : hash;
        } catch (e) { return hash; }
    }
    return "";
};

window.setUrlHash = function (hash) {
    if (hash) {
        let encoded = window.LZString ? LZString.compressToEncodedURIComponent(hash) : encodeURIComponent(hash);
        window.history.replaceState(null, null, "#" + encoded);
    } else {
        window.history.replaceState(null, null, window.location.pathname);
    }
};

window.shareUrl = async (title, text, url) => {
    if (navigator.share) {
        try {
            await navigator.share({ title, text, url });
            return true;
        } catch (e) {
            if (e.name !== 'AbortError') console.error(e);
            return window.copyToClipboard(url);
        }
    }
    return window.copyToClipboard(url);
};

window.shareCompressedHash = async (title, text, hash) => {
    let encoded = window.LZString ? LZString.compressToEncodedURIComponent(hash) : encodeURIComponent(hash);
    const fullUrl = window.location.origin + window.location.pathname + "#" + encoded;
    return window.shareUrl(title, text, fullUrl);
};

// --- DOM Interaction & Pointer Management ---
window.capturePointer = function (elemId, ptrId) {
    try {
        var el = document.getElementById(elemId);
        if (el && el.setPointerCapture) el.setPointerCapture(ptrId);
    } catch (e) {}
};

window.releasePointer = function (elemId, ptrId) {
    try {
        var el = document.getElementById(elemId);
        if (el && el.releasePointerCapture) el.releasePointerCapture(ptrId);
    } catch (e) {}
};

window.clickElement = (id) => {
    const el = document.getElementById(id);
    if (el) el.click();
};

window.copyToClipboard = function (text) {
    if (navigator.clipboard && navigator.clipboard.writeText) {
        return navigator.clipboard.writeText(text).then(() => true).catch(() => false);
    } else {
        const textArea = document.createElement("textarea");
        textArea.value = text;
        document.body.appendChild(textArea);
        textArea.select();
        try {
            const successful = document.execCommand('copy');
            document.body.removeChild(textArea);
            return Promise.resolve(successful);
        } catch (err) {
            document.body.removeChild(textArea);
            return Promise.resolve(false);
        }
    }
};

// --- SVG Layout & Coordinates Utilities ---
window.getSvgInfo = function (svgId) {
    const svg = document.getElementById(svgId);
    if (!svg) {
        return {
            left: 0, top: 0, width: window.innerWidth, height: window.innerHeight,
            viewBoxX: 0, viewBoxY: 0, viewBoxW: window.innerWidth,
            viewBoxH: window.innerHeight
        };
    }
    const rect = svg.getBoundingClientRect();
    let vb = { x: 0, y: 0, w: rect.width, h: rect.height };
    try {
        const vbStr = svg.getAttribute('viewBox');
        if (vbStr) {
            const parts = vbStr.trim().split(/\s+|,/).map(parseFloat);
            if (parts.length >= 4 && parts.every(p => !isNaN(p))) {
                vb.x = parts[0]; vb.y = parts[1]; vb.w = parts[2]; vb.h = parts[3];
            } else {
                vb.w = rect.width; vb.h = rect.height;
            }
        } else if (svg.viewBox && svg.viewBox.baseVal) {
            vb = {
                x: svg.viewBox.baseVal.x,
                y: svg.viewBox.baseVal.y,
                w: svg.viewBox.baseVal.width || rect.width,
                h: svg.viewBox.baseVal.height || rect.height
            };
        }
    } catch (e) {
        vb = { x: 0, y: 0, w: rect.width, h: rect.height };
    }

    return {
        left: rect.left,
        top: rect.top,
        width: rect.width,
        height: rect.height,
        viewBoxX: vb.x,
        viewBoxY: vb.y,
        viewBoxW: vb.w,
        viewBoxH: vb.h
    };
};

window.getSvgWidth = function (svgId) {
    const el = document.getElementById(svgId);
    if (el) {
        const r = el.getBoundingClientRect();
        if (r && r.width > 0) return r.width;
    }
    const container = document.getElementById('hywe-svg-wrapper') || document.getElementById('map-and-svg-container') || document.querySelector('.boundary-svg-container');
    if (container) {
        const r = container.getBoundingClientRect();
        if (r && r.width > 0) return r.width;
    }
    return Math.min(800, Math.max(280, (window.innerWidth || 360) - 32));
};

window.getSvgCoords = function (svgId, clientX, clientY) {
    const svg = document.getElementById(svgId);
    if (!svg || !svg.createSVGPoint) return { x: clientX, y: clientY };

    const pt = svg.createSVGPoint();
    pt.x = clientX;
    pt.y = clientY;

    const ctm = svg.getScreenCTM();
    if (!ctm) {
        const info = window.getSvgInfo(svgId);
        return {
            x: info.viewBoxX + (clientX - info.left) * info.viewBoxW / info.width,
            y: info.viewBoxY + (clientY - info.top) * info.viewBoxH / info.height
        };
    }

    const inv = ctm.inverse();
    const svgP = pt.matrixTransform(inv);
    return { x: svgP.x, y: svgP.y };
};

// --- File Downloads & Serialization ---
window.downloadFile = async function (fileName, content, contentType) {
    const blob = new Blob([content], { type: contentType });

    // On mobile, the Web Share API provides a superior experience
    const isMobile = /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent);
    if (isMobile) {
        try {
            const file = new File([blob], fileName, { type: contentType });
            if (navigator.canShare && navigator.canShare({ files: [file] })) {
                await navigator.share({
                    files: [file],
                    title: fileName,
                    text: "Hywe Export"
                });
                return;
            }
        } catch (e) {
            console.warn("Web Share API not supported or user cancelled, falling back to download", e);
        }
    }

    // Fallback: Use FileReader to convert Blob to Base64 Data URL
    const reader = new FileReader();
    reader.onload = function () {
        const a = document.createElement("a");
        a.href = reader.result;
        a.download = fileName;
        document.body.appendChild(a);
        a.click();
        setTimeout(() => {
            document.body.removeChild(a);
        }, 500);
    };
    reader.readAsDataURL(blob);
};

// --- Minimalist Architectural Titleblock & Template Engine for Exports ---
window.escapeXml = function (str) {
    if (str === null || str === undefined) return "";
    return String(str)
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;")
        .replace(/'/g, "&apos;");
};

window.truncateStr = function (str, maxLen) {
    if (!str) return "";
    str = String(str).trim();
    if (str.length <= maxLen) return str;
    return str.substring(0, maxLen - 1) + "…";
};

const HYWE_LOGO_PATH = "M 167 836 Q 167 850 179 857 L 279 915 Q 317 937 317 893 L 317 600 Q 317 575 342 575 L 500 575 Q 525 575 525 600 L 525 738 Q 525 788 575 788 L 748 788 Q 841 788 760 834 L 488 992 Q 450 1013 488 1035 L 588 1093 Q 600 1100 613 1093 L 1021 857 Q 1033 850 1033 836 L 1033 364 Q 1033 350 1021 343 L 921 285 Q 883 263 883 307 L 883 613 Q 883 638 858 638 L 700 638 Q 675 638 675 613 L 675 450 Q 675 425 650 425 L 430 425 Q 337 425 418 378 L 713 208 Q 750 187 713 165 L 613 104 Q 600 100 588 104 L 179 343 Q 167 350 167 364 L 167 836 Z";

window.applyArchitecturalTitleblock = function (svgString, meta) {
    if (!svgString || typeof svgString !== "string") return svgString;

    // Idempotency: avoid double-wrapping
    if (svgString.includes('id="hywe-titleblock-container"')) {
        return svgString;
    }

    meta = meta || {};
    const projectTitle = meta.projectTitle || meta.ProjectTitle || "Hywe Exploration";
    const author = meta.author || meta.Author || "Hywe Design Team";
    const date = meta.date || meta.Date || new Date().toISOString().slice(0, 10);
    const level = meta.level || meta.Level || "L0";

    // Parse viewBox or width/height
    let minX = 0, minY = 0, origW = 800, origH = 600;
    const vbMatch = svgString.match(/viewBox=["']\s*([-\d.]+)\s+([-\d.]+)\s+([-\d.]+)\s+([-\d.]+)["']/i);
    if (vbMatch) {
        minX = parseFloat(vbMatch[1]);
        minY = parseFloat(vbMatch[2]);
        origW = parseFloat(vbMatch[3]);
        origH = parseFloat(vbMatch[4]);
    } else {
        const wMatch = svgString.match(/width=["']\s*([-\d.]+)["']/i);
        const hMatch = svgString.match(/height=["']\s*([-\d.]+)["']/i);
        if (wMatch) origW = parseFloat(wMatch[1]);
        if (hMatch) origH = parseFloat(hMatch[1]);
    }
    if (!origW || isNaN(origW) || origW <= 0) origW = 800;
    if (!origH || isNaN(origH) || origH <= 0) origH = 600;

    // Extract inner content of the SVG
    let cleanSvg = svgString.replace(/<\?xml[\s\S]*?\?>/i, "").replace(/<!DOCTYPE[\s\S]*?>/i, "");
    const openTagEnd = cleanSvg.indexOf('>');
    const closeTagStart = cleanSvg.lastIndexOf('</svg>');
    let innerContent = "";
    if (openTagEnd !== -1 && closeTagStart !== -1 && closeTagStart > openTagEnd) {
        innerContent = cleanSvg.substring(openTagEnd + 1, closeTagStart);
    } else {
        innerContent = cleanSvg;
    }

    // Total sheet height is proportionately scaled to frame drawing content
    const sheetH = Math.round(origH * 1.2);

    // Title block height is strictly proportional to the exported image height (~5.6% of sheet height)
    const barH = Math.max(16, Math.round(sheetH * 0.056));

    // Continuous proportional scale factor for title block typography, icons, and compartments (normalized to reference barH = 42)
    const s = barH / 42;

    // Outer sheet border margin (proportional to sheet height)
    const bMargin = Math.max(4, Math.round(sheetH * 0.016));

    // Horizontal margins and minimum sheet width ensuring all compartments fit without truncating
    const padX = Math.round(sheetH * 0.04);
    let sheetW = Math.round(origW + padX * 2);

    // Proportionately scaled column widths & geometry
    const logoW = Math.round(44 * s);
    const wordmarkW = Math.round(100 * s);
    const dateW = Math.round(105 * s);
    const levelW = Math.round(85 * s);
    const minTextW = Math.round(200 * s);
    const minBarW = logoW + wordmarkW + dateW + levelW + minTextW;
    const minSheetW = minBarW + bMargin * 2;
    if (sheetW < minSheetW) {
        sheetW = minSheetW;
    }

    // Outer sheet border dimensions
    const bW = sheetW - bMargin * 2;
    const bH = sheetH - bMargin * 2;

    // Center original drawing horizontally and vertically within available drawing frame
    const drawingAreaH = bH - barH;
    const shiftX = Math.round((sheetW - origW) / 2) - minX;
    const shiftY = bMargin + Math.round((drawingAreaH - origH) / 2) - minY;

    // Single-row bottom bar geometry across the bottom of the border
    const barX = bMargin;
    const barY = bMargin + bH - barH;
    const barW = bW;

    // Internal geometry for compartments
    const logoSize = Math.round(22 * s);
    const logoX = barX + Math.round((logoW - logoSize) / 2);
    const logoY = barY + Math.round((barH - logoSize) / 2);

    const wordmarkX = barX + logoW;
    const projX = wordmarkX + wordmarkW;
    const dateX = barX + barW - dateW;
    const levelX = dateX - levelW;
    const availTextW = Math.max(minTextW, levelX - projX);
    const projW = Math.round(availTextW * 0.58);
    const authorX = projX + projW;

    // Proportionately scaled typography & offsets
    const labelFontSize = (6.5 * s).toFixed(1) + "px";
    const valueFontSize = (11 * s).toFixed(1) + "px";
    const dateFontSize = (10.5 * s).toFixed(1) + "px";
    const wordmarkFontSize = (11.5 * s).toFixed(1) + "px";
    const wordmarkSpacing = (1.5 * s).toFixed(1) + "px";
    const urlFontSize = (7.5 * s).toFixed(1) + "px";
    const urlSpacing = (0.5 * s).toFixed(1) + "px";

    const labelY = barY + Math.round(barH * 0.36);
    const valueY = barY + Math.round(barH * 0.72);
    const wmY = barY + Math.round(barH * 0.45);
    const urlY = barY + Math.round(barH * 0.74);
    const textPadX = Math.max(3, Math.round(12 * s));
    const borderWidth = Math.max(0.8, 1.2 * s).toFixed(1);
    const divWidth = Math.max(0.6, 1.0 * s).toFixed(1);

    const wrappedSvg = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${sheetW} ${sheetH}" width="${sheetW}" height="${sheetH}">
    <defs>
        <style>
            .tb-font { font-family: 'Outfit', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; }
        </style>
    </defs>
    <!-- Background -->
    <rect width="${sheetW}" height="${sheetH}" fill="#ffffff" />

    <!-- Drawing Sheet Border -->
    <rect x="${bMargin}" y="${bMargin}" width="${bW}" height="${bH}" fill="none" stroke="#0f172a" stroke-width="${borderWidth}" />

    <!-- Drawing Content -->
    <g id="drawing-content" transform="translate(${shiftX}, ${shiftY})">
        ${innerContent}
    </g>

    <!-- Single Row Bottom Titleblock (Proportionately Scaled) -->
    <g id="hywe-titleblock-container">
        <!-- Top Divider of the Bottom Bar -->
        <line x1="${barX}" y1="${barY}" x2="${barX + barW}" y2="${barY}" stroke="#0f172a" stroke-width="${borderWidth}" />

        <!-- 1. Logo Compartment -->
        <svg x="${logoX}" y="${logoY}" width="${logoSize}" height="${logoSize}" viewBox="0 0 1200 1200">
            <path fill="#0f172a" d="${HYWE_LOGO_PATH}" />
        </svg>
        <line x1="${wordmarkX}" y1="${barY}" x2="${wordmarkX}" y2="${barY + barH}" stroke="#e2e8f0" stroke-width="${divWidth}" />

        <!-- 2. Wordmark Compartment: 'H Y W E' + 'www.hywe.in' -->
        <text x="${wordmarkX + textPadX}" y="${wmY}" class="tb-font" font-size="${wordmarkFontSize}" font-weight="700" fill="#0f172a" letter-spacing="${wordmarkSpacing}">H Y W E</text>
        <text x="${wordmarkX + textPadX}" y="${urlY}" class="tb-font" font-size="${urlFontSize}" font-weight="500" fill="#64748b" letter-spacing="${urlSpacing}">www.hywe.in</text>
        <line x1="${projX}" y1="${barY}" x2="${projX}" y2="${barY + barH}" stroke="#e2e8f0" stroke-width="${divWidth}" />

        <!-- 3. Project Title Compartment (Full / Untruncated) -->
        <text x="${projX + textPadX}" y="${labelY}" class="tb-font" font-size="${labelFontSize}" font-weight="700" fill="#64748b" letter-spacing="0.8px">PROJECT</text>
        <text x="${projX + textPadX}" y="${valueY}" class="tb-font" font-size="${valueFontSize}" font-weight="600" fill="#0f172a">${window.escapeXml(projectTitle)}</text>
        <line x1="${authorX}" y1="${barY}" x2="${authorX}" y2="${barY + barH}" stroke="#e2e8f0" stroke-width="${divWidth}" />

        <!-- 4. Author Compartment (Full / Untruncated) -->
        <text x="${authorX + textPadX}" y="${labelY}" class="tb-font" font-size="${labelFontSize}" font-weight="700" fill="#64748b" letter-spacing="0.8px">AUTHOR</text>
        <text x="${authorX + textPadX}" y="${valueY}" class="tb-font" font-size="${valueFontSize}" font-weight="500" fill="#0f172a">${window.escapeXml(author)}</text>
        <line x1="${levelX}" y1="${barY}" x2="${levelX}" y2="${barY + barH}" stroke="#e2e8f0" stroke-width="${divWidth}" />

        <!-- 5. Level Compartment -->
        <text x="${levelX + textPadX}" y="${labelY}" class="tb-font" font-size="${labelFontSize}" font-weight="700" fill="#64748b" letter-spacing="0.8px">LEVEL</text>
        <text x="${levelX + textPadX}" y="${valueY}" class="tb-font" font-size="${valueFontSize}" font-weight="600" fill="#0f172a">${window.escapeXml(level)}</text>
        <line x1="${dateX}" y1="${barY}" x2="${dateX}" y2="${barY + barH}" stroke="#e2e8f0" stroke-width="${divWidth}" />

        <!-- 6. Date Compartment -->
        <text x="${dateX + textPadX}" y="${labelY}" class="tb-font" font-size="${labelFontSize}" font-weight="700" fill="#64748b" letter-spacing="0.8px">DATE</text>
        <text x="${dateX + textPadX}" y="${valueY}" class="tb-font" font-size="${dateFontSize}" font-weight="500" fill="#0f172a">${window.escapeXml(date)}</text>
    </g>
</svg>`;

    return wrappedSvg;
};

window.downloadSvgWithTitleblock = function (filename, svgString, metadata) {
    const withTitleblock = window.applyArchitecturalTitleblock(svgString, metadata);
    const xmlHeader = '<?xml version="1.0" standalone="no"?>\r\n';
    window.downloadFile(filename, xmlHeader + withTitleblock, "image/svg+xml;charset=utf-8");
};

window.downloadSvgFile = function (svgId, filename, metadata) {
    const svg = document.getElementById(svgId);
    if (!svg) return;

    const serializer = new XMLSerializer();
    let source = serializer.serializeToString(svg);
    source = source.replace(/<!--.*?-->/g, "");
    source = source.replace(/xmlns="http:\/\/www\.w3\.org\/1999\/xhtml"/g, "");

    if (!source.includes('xmlns="http://www.w3.org/2000/svg"')) {
        source = source.replace('<svg ', '<svg xmlns="http://www.w3.org/2000/svg" ');
    }

    source = source.replace(/<textpath/g, "<textPath")
                   .replace(/<\/textpath>/g, "</textPath>")
                   .replace(/viewbox=/g, "viewBox=")
                   .replace(/startoffset=/g, "startOffset=")
                   .replace(/textlength=/g, "textLength=")
                   .replace(/lengthadjust=/g, "lengthAdjust=")
                   .replace(/\s*onclick:stoppropagation(?:=["'][^"']*["'])?/gi, "");

    const withTitleblock = window.applyArchitecturalTitleblock(source, metadata);
    const xmlHeader = '<?xml version="1.0" standalone="no"?>\r\n';
    const finalSvg = xmlHeader + withTitleblock;
    window.downloadFile(filename, finalSvg, "image/svg+xml;charset=utf-8");
};

window.downloadSvgAsPng = function (fileName, svgString, metadata) {
    const withTitleblock = window.applyArchitecturalTitleblock(svgString, metadata);
    const blob = new Blob([withTitleblock], { type: "image/svg+xml;charset=utf-8" });
    const url = URL.createObjectURL(blob);
    const img = new Image();
    img.onload = function () {
        let nativeWidth = img.width || 800;
        let nativeHeight = img.height || 600;

        const vbMatch = withTitleblock.match(/viewBox=["'][^"']*?[\d.-]+\s+[\d.-]+\s+([\d.-]+)\s+([\d.-]+)["']/i) || 
                        withTitleblock.match(/viewBox=["']([\d.-]+)\s+([\d.-]+)\s+([\d.-]+)\s+([\d.-]+)["']/i);

        if (vbMatch) {
            const w = parseFloat(vbMatch[vbMatch.length - 2]);
            const h = parseFloat(vbMatch[vbMatch.length - 1]);
            if (w > 0 && h > 0 && (nativeWidth <= 300 || w > nativeWidth)) {
                nativeWidth = w;
                nativeHeight = h;
            }
        }

        const scale = 3;
        const canvas = document.createElement("canvas");
        canvas.width = Math.round(nativeWidth * scale);
        canvas.height = Math.round(nativeHeight * scale);
        const ctx = canvas.getContext("2d");

        ctx.fillStyle = "white";
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        ctx.drawImage(img, 0, 0, canvas.width, canvas.height);
        URL.revokeObjectURL(url);

        canvas.toBlob(function (pngBlob) {
            if (pngBlob !== null) {
                const objUrl = URL.createObjectURL(pngBlob);
                const a = document.createElement("a");
                a.href = objUrl;
                a.download = fileName;
                document.body.appendChild(a);
                a.click();
                setTimeout(() => {
                    document.body.removeChild(a);
                    URL.revokeObjectURL(objUrl);
                }, 500);
            } else {
                console.error("Hywe: Canvas toBlob failed.");
            }
        }, "image/png");
    };
    img.onerror = function (e) {
        console.error("Hywe: Failed to render SVG into Image for PNG conversion. Invalid SVG format.", e);
    };
    img.src = url;
};

window.downloadSvgElementAsPng = function (svgId, filename, metadata) {
    const svg = document.getElementById(svgId);
    if (!svg) return;

    const serializer = new XMLSerializer();
    let source = serializer.serializeToString(svg);
    source = source.replace(/<!--.*?-->/g, "");
    source = source.replace(/xmlns="http:\/\/www\.w3\.org\/1999\/xhtml"/g, "");

    if (!source.includes('xmlns="http://www.w3.org/2000/svg"')) {
        source = source.replace('<svg ', '<svg xmlns="http://www.w3.org/2000/svg" ');
    }

    source = source.replace(/<textpath/g, "<textPath")
                   .replace(/<\/textpath>/g, "</textPath>")
                   .replace(/viewbox=/g, "viewBox=")
                   .replace(/startoffset=/g, "startOffset=")
                   .replace(/textlength=/g, "textLength=")
                   .replace(/lengthadjust=/g, "lengthAdjust=")
                   .replace(/\s*onclick:stoppropagation(?:=["'][^"']*["'])?/gi, "");

    window.downloadSvgAsPng(filename, source, metadata);
};

window.download3DView = async function (canvasId, filename, format, metadata) {
    const pngDataUrl = await window.captureCanvasWebGPU(canvasId);
    if (!pngDataUrl) {
        console.error("Hywe: captureCanvasWebGPU returned empty data URL.");
        return;
    }
    const img = new Image();
    await new Promise((resolve) => {
        img.onload = resolve;
        img.onerror = resolve;
        img.src = pngDataUrl;
    });
    const w = img.width || 800;
    const h = img.height || 600;
    const rawSvg = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${w} ${h}" width="${w}" height="${h}">
        <image href="${pngDataUrl}" width="${w}" height="${h}" preserveAspectRatio="xMidYMid meet" />
    </svg>`;
    const withTitleblock = window.applyArchitecturalTitleblock(rawSvg, metadata || {});
    if (format === 'svg') {
        const xmlHeader = '<?xml version="1.0" standalone="no"?>\r\n';
        window.downloadFile(filename, xmlHeader + withTitleblock, "image/svg+xml;charset=utf-8");
    } else {
        window.downloadSvgAsPng(filename, withTitleblock, metadata);
    }
};

window.readHywFile = (fileInputId) => {
    const input = document.getElementById(fileInputId);
    if (!input || !input.files.length) return Promise.resolve("");

    return new Promise((resolve) => {
        const reader = new FileReader();
        reader.onload = (e) => {
            const result = e.target.result;
            input.value = "";
            resolve(result);
        };
        reader.readAsText(input.files[0]);
    });
};

window.openReport = function (html) {
    const w = window.open('', '_blank');
    w.document.write(html);
    w.document.close();
    w.focus();
    setTimeout(() => w.print(), 600);
};

// --- External Services Interop ---
window.recordToHynteract = async (apiUri, payload) => {
    try {
        const enrichedPayload = { ...(payload || {}), website: "" };
        const response = await fetch(apiUri, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                'X-Hywe-Key': 'hywe-hynteract'
            },
            body: JSON.stringify(enrichedPayload)
        });
        if (!response.ok) {
            const rawErr = await response.text();
            console.error("API Error:", rawErr);
            let errMsg = response.statusText;
            let errCode = "UNKNOWN_ERROR";
            try {
                const parsed = JSON.parse(rawErr);
                if (parsed.error) errMsg = parsed.error;
                if (parsed.code) errCode = parsed.code;
            } catch (_) {}
            return { ok: false, error: errMsg, code: errCode };
        }
        return { ok: true, error: "", code: "" };
    } catch (e) {
        console.error("Network/Fetch Error:", e);
        return { ok: false, error: "Network error while submitting to dataset. Please check your connection and try again.", code: "NETWORK_ERROR" };
    }
};

window.fetchHFGallery = async () => {
    try {
        const url = `https://hynteract.vercel.app/api/gallery`;
        const res = await fetch(url);
        if (!res.ok) throw new Error("Failed to fetch gallery");
        const data = await res.json();
        return data.entries || [];
    } catch (e) {
        console.error("Network/Fetch Error:", e);
        return [];
    }
};

window.fetchGalleryDefinition = async (rowIdx) => {
    try {
        const url = `https://datasets-server.huggingface.co/rows?dataset=vykrum%2Fhywe-spatial-dataset&config=default&split=train&offset=${rowIdx}&length=1`;
        const res = await fetch(url);
        if (!res.ok) throw new Error("Failed to fetch specific definition");
        const data = await res.json();
        if (data && data.rows && data.rows.length > 0) {
            return data.rows[0].row.definition || "";
        }
        return "";
    } catch (e) {
        console.error("Network/Fetch Error for definition:", e);
        return "";
    }
};

// --- Hymap Iframe Communication ---
window.addEventListener('message', function (event) {
    if (event.data && event.data.type === 'MAP_LOCKED_DATA') {
        const input = document.getElementById('hymap-data');
        const trigger = document.getElementById('hymap-trigger');
        if (input && trigger) {
            input.value = JSON.stringify(event.data);
            trigger.click();
        }
    }
});

// --- Blazor Application Registrations ---
window.registerUndoRedo = function (dotnetRef) {
    window._undoRedoDotNet = dotnetRef;
};

window.registerHashChange = function (dotnetRef) {
    window._hashChangeDotNet = dotnetRef;
    window.addEventListener('hashchange', function () {
        if (!window._hashChangeDotNet) return;
        const hash = window.getUrlHash();
        window._hashChangeDotNet.invokeMethodAsync('HandleHashChange', hash);
    });
};

// --- PWA & Privacy Detection ---
window.registerPwaInstall = function (dotnetRef) {
    window.hywePwaDotNetRef = dotnetRef;
    if (window.hyweDeferredPrompt) dotnetRef.invokeMethodAsync('SetInstallPromptAvailable', true);

    if (window.matchMedia('(display-mode: standalone)').matches || window.navigator.standalone === true) {
        dotnetRef.invokeMethodAsync('SetIsStandalone', true);
        dotnetRef.invokeMethodAsync('SetInstallPromptAvailable', false);
        dotnetRef.invokeMethodAsync('SetPrivacyAlert', false);
        return;
    }
    
    dotnetRef.invokeMethodAsync('SetIsStandalone', false);

    async function checkPrivacy() {
        let isPrivacy = false;
        try {
            const ua = navigator.userAgent.toLowerCase();
            if (navigator.brave && await navigator.brave.isBrave()) isPrivacy = true;
            else if (ua.includes('duckduckgo') || ua.includes('ddg')) isPrivacy = true;
            if (navigator.storage && navigator.storage.estimate) {
                const { quota } = await navigator.storage.estimate();
                if (quota && quota < 120000000) isPrivacy = true; 
            }
        } catch(e) {}
        if (isPrivacy) dotnetRef.invokeMethodAsync('SetPrivacyAlert', true);
    }
    checkPrivacy();

    window.addEventListener('appinstalled', () => {
        dotnetRef.invokeMethodAsync('SetInstallPromptAvailable', false);
        dotnetRef.invokeMethodAsync('SetPrivacyAlert', false);
        window.hyweDeferredPrompt = null;
    });
};

window.triggerPwaInstall = async function () {
    if (!window.hyweDeferredPrompt) return false;
    window.hyweDeferredPrompt.prompt();
    const { outcome } = await window.hyweDeferredPrompt.userChoice;
    window.hyweDeferredPrompt = null;
    return outcome === 'accepted';
};

// --- Global Keyboard Shortcuts ---
document.addEventListener('keydown', function (e) {
    if (!window._undoRedoDotNet) return;
    const tag = e.target.tagName;
    if (tag === 'INPUT' || tag === 'TEXTAREA') return;
    if (e.ctrlKey && !e.shiftKey && e.key === 'z') { 
        e.preventDefault();
        window._undoRedoDotNet.invokeMethodAsync('HandleUndo'); 
    } else if ((e.ctrlKey && e.key === 'y') || (e.ctrlKey && e.shiftKey && e.key === 'z')) { 
        e.preventDefault();
        window._undoRedoDotNet.invokeMethodAsync('HandleRedo'); 
    }
});
