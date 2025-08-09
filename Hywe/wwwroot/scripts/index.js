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

    // Auto-show main after 5s if no tap
    setTimeout(() => {
        if (intro && intro.style.display !== 'none') showMain();
    }, 5000);
});

// Initialize
document.addEventListener('DOMContentLoaded', () => {
    colorizeHierarchyAuto('hierarchy-text');
});

// --- Polygon Editor ---
window.clientToSvgPoint = function (svg, clientX, clientY) {
    var pt = svg.createSVGPoint();
    pt.x = clientX;
    pt.y = clientY;
    var svgP = pt.matrixTransform(svg.getScreenCTM().inverse());
    return { x: svgP.x, y: svgP.y };


/*window.getSvgCoords = function (svgId, clientX, clientY) {
    // Cache the SVG element and its CTM
    if (!polygonEditorSvgCache || polygonEditorSvgCache.id !== svgId) {
        polygonEditorSvgCache = document.getElementById(svgId);
        polygonEditorCtmCache = polygonEditorSvgCache?.getScreenCTM();
    }

    if (!polygonEditorSvgCache || !polygonEditorCtmCache) return { x: 0, y: 0 };

    // Convert screen coordinates to SVG coordinates
    const pt = polygonEditorSvgCache.createSVGPoint();
    pt.x = clientX;
    pt.y = clientY;

    const svgPoint = pt.matrixTransform(polygonEditorCtmCache.inverse());
    return { x: svgPoint.x, y: svgPoint.y };
};*/

/*window.getScaledFontSize = function (id, logicalWidth, desiredSize) {
    const el = document.getElementById(id);
    if (!el) return desiredSize;
    const width = el.getBoundingClientRect().width;
    const scale = width / logicalWidth;
    return Math.max(6, desiredSize / scale);
};

window.registerJsInteropHandlers = function (dotnetRef) {
    document.addEventListener("pointerup", () => {
        dotnetRef.invokeMethodAsync("OnGlobalPointerUp");
    });
    function reportResize() {
        const width = window.innerWidth;
        const height = window.innerHeight;
        dotnetRef.invokeMethodAsync("OnWindowResize", width, height);
    }
    window.addEventListener("resize", reportResize);
    reportResize();
};*/
