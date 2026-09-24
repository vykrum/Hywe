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

window.downloadSvgFile = function (svgId, filename) {
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

    const xmlHeader = '<?xml version="1.0" standalone="no"?>\r\n';
    const finalSvg = xmlHeader + source;
    window.downloadFile(filename, finalSvg, "image/svg+xml;charset=utf-8");
};

window.downloadSvgAsPng = function (fileName, svgString) {
    const blob = new Blob([svgString], { type: "image/svg+xml;charset=utf-8" });
    const url = URL.createObjectURL(blob);
    const img = new Image();
    img.onload = function () {
        let nativeWidth = img.width || 800;
        let nativeHeight = img.height || 600;

        const vbMatch = svgString.match(/viewBox=["'][^"']*?[\d.-]+\s+[\d.-]+\s+([\d.-]+)\s+([\d.-]+)["']/i) || 
                        svgString.match(/viewBox=["']([\d.-]+)\s+([\d.-]+)\s+([\d.-]+)\s+([\d.-]+)["']/i);

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
        canvas.width = nativeWidth * scale;
        canvas.height = nativeHeight * scale;
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

window.downloadSvgElementAsPng = function (svgId, filename) {
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

    const xmlHeader = '<?xml version="1.0" standalone="no"?>\r\n';
    const finalSvg = xmlHeader + source;
    window.downloadSvgAsPng(filename, finalSvg);
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
        const url = `https://datasets-server.huggingface.co/rows?dataset=vykrum%2Fhywe-training-data&config=default&split=train&offset=${rowIdx}&length=1`;
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
