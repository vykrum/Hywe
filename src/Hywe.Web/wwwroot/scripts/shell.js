// Hywe Shell & Global Interop Utilities

window.getUrlHash = function() {
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

window.setUrlHash = function(hash) {
    if (hash) {
        let encoded = window.LZString ? LZString.compressToEncodedURIComponent(hash) : encodeURIComponent(hash);
        window.history.replaceState(null, null, "#" + encoded);
    } else {
        window.history.replaceState(null, null, window.location.pathname);
    }
};

window.capturePointer = function(elemId, ptrId) {
    try {
        var el = document.getElementById(elemId);
        if (el && el.setPointerCapture) el.setPointerCapture(ptrId);
    } catch (e) {}
};

window.releasePointer = function(elemId, ptrId) {
    try {
        var el = document.getElementById(elemId);
        if (el && el.releasePointerCapture) el.releasePointerCapture(ptrId);
    } catch (e) {}
};

window.copyToClipboard = function(text) {
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

window.clickElement = (id) => {
    const el = document.getElementById(id);
    if (el) el.click();
};

window.downloadFile = async function (fileName, content, contentType) {
    const blob = new Blob([content], { type: contentType });

    // On mobile, the Web Share API provides a superior experience for files
    // that Android doesn't have a default viewer for (like SVG).
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
                return; // Successfully shared, skip the hidden download
            }
        } catch (e) {
            console.warn("Web Share API not supported or user cancelled, falling back to download", e);
        }
    }

    // Fallback: Use FileReader to convert Blob to Base64 Data URL
    const reader = new FileReader();
    reader.onload = function() {
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

window.downloadSvgAsPng = function(fileName, svgString) {
    const blob = new Blob([svgString], { type: "image/svg+xml;charset=utf-8" });
    const url = URL.createObjectURL(blob);
    const img = new Image();
    img.onload = function() {
        let nativeWidth = img.width || 800;
        let nativeHeight = img.height || 600;
        
        // Parse viewBox to get native high-res dimensions
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
        
        // Increase resolution multiplier
        const scale = 3;
        const canvas = document.createElement("canvas");
        canvas.width = nativeWidth * scale;
        canvas.height = nativeHeight * scale;
        const ctx = canvas.getContext("2d");
        
        // Add a white background since SVG is transparent
        ctx.fillStyle = "white";
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        ctx.drawImage(img, 0, 0, canvas.width, canvas.height);
        URL.revokeObjectURL(url);

        canvas.toBlob(function(pngBlob) {
            const isSuccess = pngBlob !== null;
            if (isSuccess) {
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
    img.onerror = function(e) {
        console.error("Hywe: Failed to render SVG into Image for PNG conversion. Invalid SVG format.", e);
    };
    img.src = url;
};

window.openReport = function(html) {
    const w = window.open('', '_blank');
    w.document.write(html);
    w.document.close();
    w.focus();
    setTimeout(() => w.print(), 600);
};

// Keyboard Shortcuts Handler
document.addEventListener('keydown', function(e) {
    if (!window._undoRedoDotNet) return;
    const tag = e.target.tagName;
    if (tag === 'INPUT' || tag === 'TEXTAREA') return;
    if (e.ctrlKey && !e.shiftKey && e.key === 'z') { 
        e.preventDefault(); window._undoRedoDotNet.invokeMethodAsync('HandleUndo'); 
    } else if ((e.ctrlKey && e.key === 'y') || (e.ctrlKey && e.shiftKey && e.key === 'z')) { 
        e.preventDefault(); window._undoRedoDotNet.invokeMethodAsync('HandleRedo'); 
    }
});
