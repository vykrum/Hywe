navigator.serviceWorker.register('service-worker.js');

window.addEventListener('load', function () {
    const loader = document.getElementById('loading-frame');
    const content = document.getElementById('page-content');
    const intro = document.getElementById('introduction');
    const tapMsg = document.querySelector('.tapText');
    const mainDiv = document.getElementById('main');
    const footer = document.getElementById('footer');

    // 1. Loader fade-out (visual transition)
    setTimeout(() => {
        if (loader) loader.remove();
        if (content) content.classList.add('visible');
        if (intro) intro.classList.add('ready');
    }, 500);

    // 2. Show "Tap to Continue" after 2 seconds
    setTimeout(() => {
        if (tapMsg) tapMsg.classList.add('visible');
    }, 2000);

    // 3. Show main content on tap or auto-hide
    function showMain() {
        intro.style.opacity = '0';
        setTimeout(() => {
            intro.style.display = 'none';
            mainDiv.style.display = 'block';
            footer.style.display = 'flex';
            requestAnimationFrame(() => {
                mainDiv.style.opacity = '1';
                footer.style.opacity = '1';
            });
        }, 500);
    }

    // Tap anywhere in intro area
    if (intro) intro.addEventListener('click', showMain);

    // Auto-hide if user never taps
    setTimeout(() => {
        if (intro && intro.style.display !== 'none') showMain();
    }, 10000);
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