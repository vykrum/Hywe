navigator.serviceWorker.register('service-worker.js');

window.addEventListener('load', function () {
    const loader = document.getElementById('loading-frame');
    const content = document.getElementById('page-content');
    const intro = document.getElementById('introduction');
    const tapMsg = document.querySelector('.tapText');
    const mainDiv = document.getElementById('main');
    const footer = document.getElementById('footer');

    // Smooth fade-out loader - fade-in intro
    if (loader) {
        // Delay fade-out just enough to let intro/video start rendering
        requestAnimationFrame(() => {
            loader.style.transition = 'opacity 0.8s ease';
            loader.style.opacity = '0.75';
        });

        loader.addEventListener('transitionend', () => {
            loader.style.display = 'none';           // remove white overlay
            if (intro) intro.classList.add('ready'); // fade in intro
            if (tapMsg) tapMsg.classList.add('visible');
            if (content) content.classList.add('visible');
        }, { once: true });
    }

    // Show main content
    function showMain() {
        if (!intro) return;
        intro.style.transition = 'opacity 0.8s ease';
        intro.style.opacity = '0.75';
        intro.addEventListener('transitionend', () => {
            intro.style.display = 'none';
            mainDiv.style.display = 'block';
            footer.style.display = 'flex';
            requestAnimationFrame(() => {
                mainDiv.style.opacity = '1';
                footer.style.opacity = '1';
            });
        }, { once: true });
    }

    // Tap anywhere to continue
    if (intro) intro.addEventListener('click', showMain);

    // Auto-hide intro if user never taps
    setTimeout(() => {
        if (intro && intro.style.display !== 'none') showMain();
    }, 10000);
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
