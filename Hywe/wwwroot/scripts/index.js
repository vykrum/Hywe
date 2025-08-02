navigator.serviceWorker.register('service-worker.js');

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



// JS interop helpers for Blazor
window.getSvgCoords = function (svgId, clientX, clientY) {
    const svg = document.getElementById(svgId);
    if (!svg) return { x: 0, y: 0 };
    const pt = svg.createSVGPoint();
    pt.x = clientX;
    pt.y = clientY;
    const screenCTM = svg.getScreenCTM();
    if (!screenCTM) return { x: 0, y: 0 };
    const svgPoint = pt.matrixTransform(screenCTM.inverse());
    return { x: svgPoint.x, y: svgPoint.y };
};

window.getScaledFontSize = function (id, logicalWidth, desiredSize) {
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
};
