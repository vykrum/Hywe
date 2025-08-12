navigator.serviceWorker.register('service-worker.js');
// --- Loading Animation ---
window.addEventListener('load', () => {
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

        loader.addEventListener('transitionend', () => {
            loader.style.display = 'none'; // remove after fade to prevent click blocking
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

// Initialize
document.addEventListener('DOMContentLoaded', () => {
    colorizeHierarchyAuto('hierarchy-text');
});

// --- Polygon Editor ---
// return basic bounding client rect + viewBox mapping used by toSvgCoordsFromInfo
window.getSvgInfo = function (svgId) {
    const svg = document.getElementById(svgId);
    if (!svg) {
        // return a fallback rect to avoid exceptions
        return {
            left: 0, top: 0, width: window.innerWidth, height: window.innerHeight,
            viewBoxX: 0, viewBoxY: 0, viewBoxW: svg ? svg.viewBox.baseVal.width : window.innerWidth,
            viewBoxH: svg ? svg.viewBox.baseVal.height : window.innerHeight
        };
    }
    const rect = svg.getBoundingClientRect();
    // parse viewBox: "minX minY width height"
    let vb = { x: 0, y: 0, w: rect.width, h: rect.height };
    try {
        const vbStr = svg.getAttribute('viewBox');
        if (vbStr) {
            const parts = vbStr.trim().split(/\s+|,/).map(parseFloat);
            if (parts.length >= 4 && parts.every(p => !isNaN(p))) {
                vb.x = parts[0]; vb.y = parts[1]; vb.w = parts[2]; vb.h = parts[3];
            } else {
                // fallback size
                vb.w = rect.width; vb.h = rect.height;
            }
        } else {
            // fallback to svg width/height or rect size
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
        // swallow any parse errors and fallback to rect
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
    // transform to svg coordinates
    const ctm = svg.getScreenCTM();
    if (!ctm) {
        // fallback to getSvgInfo method (approx)
        const info = window.getSvgInfo(svgId);
        const x = info.viewBoxX + (clientX - info.left) * info.viewBoxW / info.width;
        const y = info.viewBoxY + (clientY - info.top) * info.viewBoxH / info.height;
        return { x: x, y: y };
    }
    const inv = ctm.inverse();
    const svgP = pt.matrixTransform(inv);
    return { x: svgP.x, y: svgP.y };
};




