// --- Service Worker Registration ---
if ('serviceWorker' in navigator) {
    window.addEventListener('load', () => {
        navigator.serviceWorker.register('/service-worker.js')
            .then(reg => console.log('Service worker registered:', reg))
            .catch(err => console.error('Service worker registration failed:', err));
    });
}

// --- HYWE Metadata Centralization ---
const PUBLISHED_DATE = "2022-08-15T00:00:00Z";
const MODIFIED_DATE = "2025-11-08T00:00:00Z"; // or new Date().toISOString()

document.addEventListener("DOMContentLoaded", () => {
    // Update meta tags
    const metaPub = document.querySelector('meta[property="article:published_time"]');
    const metaMod = document.querySelector('meta[property="article:modified_time"]');
    if (metaPub) metaPub.setAttribute("content", PUBLISHED_DATE);
    if (metaMod) metaMod.setAttribute("content", MODIFIED_DATE);

    // Update JSON-LD structured data
    const ld = document.getElementById("hywe-schema");
    if (ld) {
        try {
            const data = JSON.parse(ld.textContent);
            data.datePublished = PUBLISHED_DATE;
            data.dateModified = MODIFIED_DATE;
            ld.textContent = JSON.stringify(data, null, 2);
        } catch (err) {
            console.warn("Could not parse HYWE JSON-LD:", err);
        }
    }

    // --- Loading Animation ---
    const loader = document.getElementById('loading-frame');
    const content = document.getElementById('page-content');
    const intro = document.getElementById('introduction');
    const tapMsg = document.querySelector('.tapText');
    const mainDiv = document.getElementById('main');
    const footer = document.getElementById('footer');

    // Ensure starting opacity for fade-in targets
    if (content) content.style.opacity = '0';
    if (mainDiv) mainDiv.style.opacity = '0';
    if (footer) footer.style.opacity = '0';

    // === Seamless Loader Fade Out + Content Fade In ===
    if (loader) {
        requestAnimationFrame(() => {
            loader.style.transition = 'opacity 0.8s ease';
            loader.style.opacity = '0';                // fade out loader
            if (content) content.classList.add('fade-in'); // fade in page content
            if (intro) intro.classList.add('ready');       // fade in intro
            if (tapMsg) tapMsg.classList.add('visible');
        });

        loader.addEventListener('transitionend', (e) => {
            if (e.propertyName === 'opacity') loader.style.display = 'none';
        }, { once: true });
    }

    // === Show Main App After Intro Tap or Timeout ===
    function showMain() {
        if (!intro) return;
        intro.classList.add('fade-out');        // fades intro to 0.25 opacity
        intro.addEventListener('transitionend', () => {
            intro.style.display = 'none';       // hide intro
            mainDiv.style.display = 'block';    // show main
            footer.style.display = 'flex';      // show footer
            requestAnimationFrame(() => {
                mainDiv.style.opacity = '1';    // fade in main
                footer.style.opacity = '1';     // fade in footer
            });
        }, { once: true });
    }

    // Tap to continue
    if (intro) intro.addEventListener('click', showMain);

    // Auto-show main after 10s if no tap
    setTimeout(() => {
        if (intro && intro.style.display !== 'none') showMain();
    }, 10000);

});

// --- Polygon Editor Utility Functions ---
// return basic bounding client rect + viewBox mapping used by toSvgCoordsFromInfo
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
        } else {
            if (svg.viewBox && svg.viewBox.baseVal) {
                vb.x = svg.viewBox.baseVal.x || 0;
                vb.y = svg.viewBox.baseVal.y || 0;
                vb.w = svg.viewBox.baseVal.width || rect.width;
                vb.h = svg.viewBox.baseVal.height || rect.height;
            } else {
                vb.w = rect.width; vb.h = rect.height;
            }
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

// precise mapping: client coords -> svg coords (used for double-click and contextmenu)
window.getSvgCoords = function (svgId, clientX, clientY) {
    const svg = document.getElementById(svgId);
    if (!svg || !svg.createSVGPoint) {
        return { x: clientX, y: clientY };
    }
    const pt = svg.createSVGPoint();
    pt.x = clientX;
    pt.y = clientY;
    const ctm = svg.getScreenCTM();
    if (!ctm) {
        const info = window.getSvgInfo(svgId);
        const x = info.viewBoxX + (clientX - info.left) * info.viewBoxW / info.width;
        const y = info.viewBoxY + (clientY - info.top) * info.viewBoxH / info.height;
        return { x: x, y: y };
    }
    const inv = ctm.inverse();
    const svgP = pt.matrixTransform(inv);
    return { x: svgP.x, y: svgP.y };
};

window.hywe = {
    receiveSvgData: function (data) {
        console.log("[hywe] Received data from F#:", data);
        // Example: if you want to visualize
        // document.getElementById("svg-log").innerText = JSON.stringify(data, null, 2);
    }
};
