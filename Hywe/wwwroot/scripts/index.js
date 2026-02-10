// --- Service Worker Registration ---
if ('serviceWorker' in navigator) {
    window.addEventListener('load', () => {
        navigator.serviceWorker.register('/service-worker.js')
            .then(reg => console.log('Service worker registered:', reg))
            .catch(err => console.error('Service worker registration failed:', err));
    });
}

function applyWillChange(el, property = 'opacity') {
    if (!el) return;
    el.style.willChange = property;
}
function clearWillChange(el) {
    if (!el) return;
    el.style.willChange = '';
}
function fadeWithWillChange(el, action) {
    if (!el) return;

    applyWillChange(el);

    const handler = () => {
        clearWillChange(el);
        el.removeEventListener('transitionend', handler);
    };

    el.addEventListener('transitionend', handler);
    action(); // perform the opacity change
}

// --- HYWE Metadata Centralization ---
const PUBLISHED_DATE = "2022-08-15T00:00:00Z";
const MODIFIED_DATE = "2025-11-08T00:00:00Z";

document.addEventListener("DOMContentLoaded", () => {

    // Update meta tags
    const metaPub = document.querySelector('meta[property="article:published_time"]');
    const metaMod = document.querySelector('meta[property="article:modified_time"]');
    if (metaPub) metaPub.setAttribute("content", PUBLISHED_DATE);
    if (metaMod) metaMod.setAttribute("content", MODIFIED_DATE);

    // Update JSON-LD
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

    // --- Loading Animation Elements ---
    const loader = document.getElementById('loading-frame');
    const content = document.getElementById('page-content');
    const intro = document.getElementById('introduction');
    const tapMsg = document.querySelector('.tapText');
    const mainDiv = document.getElementById('main');
    const footer = document.getElementById('footer');

    // Ensure starting opacity for fade targets
    if (content) content.style.opacity = '0';
    if (mainDiv) mainDiv.style.opacity = '0';
    if (footer) footer.style.opacity = '0';

    // ---------------------------------------------------
    //  LOADER FADE OUT  + CONTENT FADE IN (with delay)
    // ---------------------------------------------------

    const LOADING_DURATION = 2000; // <-- increase for longer "Loading..."

    if (loader) {
        setTimeout(() => {
            requestAnimationFrame(() => {
                fadeWithWillChange(loader, () => {
                    loader.style.transition = 'opacity 0.8s ease';
                    loader.style.opacity = '0';    // fade out loader
                });

                if (content) {
                    fadeWithWillChange(content, () => {
                        content.classList.add('fade-in'); // fade in page content
                    });
                }

                if (intro) intro.classList.add('ready');
                if (tapMsg) tapMsg.classList.add('visible');
            });
        }, LOADING_DURATION);

        loader.addEventListener('transitionend', (e) => {
            if (e.propertyName === 'opacity') loader.style.display = 'none';
        }, { once: true });
    }


    // ------------------------------------
    //   MAIN SCREEN SHOW (Tap or Timeout)
    // ------------------------------------
    function showMain() {
        if (!intro) return;

        fadeWithWillChange(intro, () => {
            intro.classList.add('fade-out');
        });

        intro.addEventListener('transitionend', () => {
            intro.style.display = 'none';
            mainDiv.style.display = 'block';
            footer.style.display = 'flex';

            requestAnimationFrame(() => {
                fadeWithWillChange(mainDiv, () => { mainDiv.style.opacity = '1'; });
                fadeWithWillChange(footer, () => { footer.style.opacity = '1'; });
            });
        }, { once: true });
    }

    if (intro) intro.addEventListener('click', showMain);

    // Auto show after 10s
    setTimeout(() => {
        if (intro && intro.style.display !== 'none') showMain();
    }, 10000);
});

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

// Debug receiver
window.hywe = {
    receiveSvgData: function (data) {
        console.log("[hywe] Received data from F#:", data);
    }
};

// ------------------------------------
//   MAIN SCREEN SHOW (Tap or Timeout)
// ------------------------------------
// Local Persistence
window.saveToBrowser = (key, data) => localStorage.setItem(key, data);
window.loadFromBrowser = (key) => localStorage.getItem(key) || "";

// Download .hyw file
window.downloadHywFile = (filename, content) => {
    const blob = new Blob([content], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const anchor = document.createElement('a');

    anchor.href = url;
    anchor.download = filename;

    document.body.appendChild(anchor);
    anchor.click();
    document.body.removeChild(anchor);

    URL.revokeObjectURL(url);
};

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

window.clickElement = (id) => document.getElementById(id).click();

window.hywePdfEngine = {
    parseColor: function (colorStr) {
        if (!colorStr || typeof colorStr !== 'string' || !colorStr.startsWith("rgba"))
            return colorStr || "#000000";
        const values = colorStr.replace(/rgba?\(|\)/g, '').split(',').map(v => parseInt(v.trim()));
        return "#" + ((1 << 24) + (values[0] << 16) + (values[1] << 8) + values[2]).toString(16).slice(1);
    },

    renderContactSheet: function (configs) {
        try {
            const { jsPDF } = window.jspdf || window;
            const doc = new jsPDF({ orientation: 'landscape', unit: 'mm', format: 'a4' });

            // 1. Calculate Global Max with zero-protection
            const globalMaxW = Math.max(...configs.map(c => Number(c.w) || 1));
            const globalMaxH = Math.max(...configs.map(c => Number(c.h) || 1));

            const pageWidth = 297;
            const pageHeight = 210;
            const margin = 10;
            const cols = 6;
            const rows = 4;
            const cellW = (pageWidth - (margin * 2)) / cols;
            const cellH = (pageHeight - (margin * 3)) / rows;

            // 2. Final scale safety
            const scale = Math.min((cellW * 0.8) / globalMaxW, (cellH * 0.8) / globalMaxH) || 1.0;

            configs.forEach((cfg, i) => {
                const col = i % cols;
                const row = Math.floor(i / cols);

                // Centering math
                const cellCenterX = margin + (col * cellW) + (cellW / 2);
                const cellCenterY = margin + (row * cellH) + (cellH / 2);
                const ox = cellCenterX - (globalMaxW * scale / 2);
                const oy = cellCenterY - (globalMaxH * scale / 2);

                if (cfg.shapes && Array.isArray(cfg.shapes)) {
                    cfg.shapes.forEach(s => {
                        // Validate points exist and have at least one X,Y pair
                        if (s.points && s.points.length >= 2) {
                            doc.setFillColor(this.parseColor(s.color));

                            // Force numeric conversion to stop jsPDF.scale error
                            const startX = Number(ox + (s.points[0] * scale));
                            const startY = Number(oy + (s.points[1] * scale));

                            if (isFinite(startX) && isFinite(startY)) {
                                doc.moveTo(startX, startY);
                                for (let j = 2; j < s.points.length; j += 2) {
                                    const px = Number(ox + (s.points[j] * scale));
                                    const py = Number(oy + (s.points[j + 1] * scale));
                                    if (isFinite(px) && isFinite(py)) doc.lineTo(px, py);
                                }
                                doc.fill();
                            }
                        }
                    });
                }
            });

            // 3. Simple Legend
            const legendItems = {};
            if (configs[0]?.shapes) {
                configs[0].shapes.forEach(s => { if (s.name) legendItems[s.name] = s.color; });
                doc.setFontSize(7);
                let idx = 0;
                Object.entries(legendItems).forEach(([name, color]) => {
                    const lx = margin + ((idx % 8) * 35);
                    const ly = pageHeight - 15 + (Math.floor(idx / 8) * 5);
                    doc.setFillColor(this.parseColor(color));
                    doc.circle(lx, ly - 1, 1.5, 'F');
                    doc.text(name, lx + 4, ly);
                    idx++;
                });
            }

            doc.save(`Hyweave_Batch_${Date.now()}.pdf`);
        } catch (err) {
            console.error("PDF Export Failure:", err);
            alert("Export failed. Check console for details.");
        }
    }
};