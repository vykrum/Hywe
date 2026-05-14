// --- Service Worker Registration ---
if ('serviceWorker' in navigator) {
    window.addEventListener('load', () => {
        navigator.serviceWorker.register('/service-worker.js')
            .then(reg => console.log('Service worker registered:', reg))
            .catch(err => console.error('Service worker registration failed:', err));
    });
}


// --- SVG Utilities ---
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


// ------------------------------------
//   FILE OPS & BROWSER STORAGE
// ------------------------------------

// Import .hyw file
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

window.clickElement = (id) => {
    const el = document.getElementById(id);
    if (el) el.click();
};


// Record Descriptions to Hynteract 
window.recordToHynteract = async (apiUri, payload) => {
    try {
        const response = await fetch(apiUri, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(payload)
        });

        if (!response.ok) {
            console.error("API Error:", await response.text());
        }
        return response.ok;
    } catch (e) {
        console.error("Network/Fetch Error:", e);
        return false;
    }
};

window.downloadFile = function (fileName, content, contentType) {
    const blob = new Blob([content], { type: contentType });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = fileName;
    document.body.appendChild(a);
    a.click();
    setTimeout(() => {
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
    }, 0);
};

window.openReport = function(html) {
    const w = window.open('', '_blank');
    w.document.write(html);
    w.document.close();
    w.focus();
    setTimeout(() => w.print(), 600);
};

window.downloadReport = function(html, fileName) {
    console.log("Hywe: Starting direct PDF export...");
    const element = document.createElement('div');
    element.innerHTML = html;
    
    const opt = {
        margin:       0,
        filename:     fileName,
        image:        { type: 'jpeg', quality: 0.90 },
        html2canvas:  { scale: 1, useCORS: true, logging: false, letterRendering: true },
        jsPDF:        { unit: 'mm', format: 'a3', orientation: 'landscape', compress: true },
        pagebreak:    { mode: ['avoid-all', 'css', 'legacy'] }
    };

    html2pdf().set(opt).from(element).save().then(() => {
        console.log("Hywe: PDF export complete.");
    }).catch(err => {
        console.error("Hywe: PDF export failed:", err);
    });
};

window.captureCanvas = function(canvasId) {
    const canvas = document.getElementById(canvasId);
    if (!canvas) return null;
    try {
        return canvas.toDataURL('image/png');
    } catch(e) {
        console.warn("captureCanvas: toDataURL failed on WebGPU canvas.", e);
        return null;
    }
};
// --- Undo / Redo keyboard shortcuts ---
let _undoRedoDotNet = null;
window.registerUndoRedo = function(dotnetRef) { _undoRedoDotNet = dotnetRef; };

// --- Hash Change (Share Link Navigation) ---
let _hashChangeDotNet = null;
window.registerHashChange = function(dotnetRef) {
    _hashChangeDotNet = dotnetRef;
    window.addEventListener('hashchange', function() {
        if (!_hashChangeDotNet) return;
        const hash = window.getUrlHash();
        _hashChangeDotNet.invokeMethodAsync('HandleHashChange', hash);
    });
};
document.addEventListener('keydown', function(e) {
    if (!_undoRedoDotNet) return;
    const tag = e.target.tagName;
    if (tag === 'INPUT' || tag === 'TEXTAREA') return;
    if (e.ctrlKey && !e.shiftKey && e.key === 'z') { e.preventDefault(); _undoRedoDotNet.invokeMethodAsync('HandleUndo'); }
    else if ((e.ctrlKey && e.key === 'y') || (e.ctrlKey && e.shiftKey && e.key === 'z')) { e.preventDefault(); _undoRedoDotNet.invokeMethodAsync('HandleRedo'); }
});



// --- PWA & Privacy Detection ---
window.hywePwaDotNetRef = null;

window.registerPwaInstall = function(dotnetRef) {
    window.hywePwaDotNetRef = dotnetRef;
    
    // If we stashed a prompt in the head before F# was ready, notify it now
    if (window.hyweDeferredPrompt) {
        console.log("Hywe: Notifying F# of stashed install prompt.");
        dotnetRef.invokeMethodAsync('SetInstallPromptAvailable', true);
    }

    // Check if already installed
    if (window.matchMedia('(display-mode: standalone)').matches || window.navigator.standalone === true) {
        dotnetRef.invokeMethodAsync('SetIsStandalone', true);
        dotnetRef.invokeMethodAsync('SetInstallPromptAvailable', false);
        dotnetRef.invokeMethodAsync('SetPrivacyAlert', false);
        return;
    }
    
    dotnetRef.invokeMethodAsync('SetIsStandalone', false);
    console.log("Hywe: Running in browser (Not Installed).");

    // High-Privacy Browser Detection
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

    window.addEventListener('appinstalled', (evt) => {
        console.log("Hywe: App installed");
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

// --- URL Hash Management (Deep Linkability) ---
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
            const decoded = decodeURIComponent(hash);
            console.log("Hywe: URL hash found:", decoded.substring(0, 20) + "...");
            return decoded;
        } catch (e) {
            console.log("Hywe: URL hash found (raw):", hash.substring(0, 20) + "...");
            return hash;
        }
    }
    return "";
};

window.setUrlHash = function(hash) {
    if (hash) {
        window.history.replaceState(null, null, "#" + encodeURIComponent(hash));
    } else {
        window.history.replaceState(null, null, window.location.pathname);
    }
};

window.copyToClipboard = function(text) {
    if (navigator.clipboard && navigator.clipboard.writeText) {
        return navigator.clipboard.writeText(text).then(() => true).catch(() => false);
    } else {
        // Fallback
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
