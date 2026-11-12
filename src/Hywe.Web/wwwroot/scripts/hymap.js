// window.Hymap namespace to expose functions to F# Bolero
window.Hymap = {
    map: null,
    _loadingDeps: null,
    savedCenter: null,
    savedZoom: null,
    _isRestoring: false,
    _lastDispatchedWidth: 0,
    _lastDispatchedHeight: 0,
    _dotnetRef: null,
    _refreshTimer: null,

    registerInterop: function(ref) {
        this._dotnetRef = ref;
    },

    saveView: function() {
        if (!this.map || this._isRestoring) return;
        const container = document.getElementById('hymap-container');
        // Leaflet calculates center using containerSize / 2. If the container is hidden or collapsed
        // (e.g. display: none on panel switch), getCenter() erroneously returns the top-left coordinate.
        // Therefore, only record coordinates when the container is visibly laid out.
        if (!container || container.offsetWidth < 100 || container.offsetHeight < 100) {
            return;
        }
        try {
            const c = this.map.getCenter();
            const z = this.map.getZoom();
            if (c && typeof c.lat === 'number' && !isNaN(c.lat) && typeof c.lng === 'number' && !isNaN(c.lng) && typeof z === 'number' && !isNaN(z)) {
                this.savedCenter = { lat: c.lat, lng: c.lng };
                this.savedZoom = z;
            }
        } catch (e) {
            // Ignore transient states
        }
    },

    refreshSize: function() {
        if (this._refreshTimer) {
            clearTimeout(this._refreshTimer);
            this._refreshTimer = null;
        }

        const applyLayout = () => {
            const container = document.getElementById('hymap-container');
            if (!container || container.offsetWidth < 100 || container.offsetHeight < 100) {
                return false;
            }

            if (!this.map) {
                this.init();
                return true;
            }

            if (this.map._container !== container) {
                this.map.remove();
                this.map = null;
                this.init();
                return true;
            }

            this._isRestoring = true;
            try {
                // Synchronize Leaflet internal size cache with live container geometry
                const w = container.clientWidth || container.offsetWidth;
                const h = container.clientHeight || container.offsetHeight;
                if (w > 0 && h > 0 && window.L && window.L.Point) {
                    this.map._size = new L.Point(w, h);
                    this.map._sizeChanged = false;
                }

                // Invalidate size with animate: false and reset: true
                this.map.invalidateSize({ animate: false, pan: false, reset: true, debounceMove: false });

                // Reset map pane to prevent residual pan translation from skewing tile origin
                if (this.map._mapPane && window.L && window.L.DomUtil) {
                    L.DomUtil.setPosition(this.map._mapPane, new L.Point(0, 0));
                }

                const targetCenter = this.savedCenter || [12.9716, 77.5946];
                const targetZoom = this.savedZoom || 13;
                this.map.setView(targetCenter, targetZoom, { animate: false, reset: true });

                // Force layer tiles to refresh to the active container frame
                this.map.eachLayer(layer => {
                    if (layer && typeof layer._update === 'function') {
                        layer._update();
                    }
                });

                this.updateDistanceLabel(false);
            } catch (err) {
                console.error("Hymap layout restoration error:", err);
            } finally {
                setTimeout(() => {
                    this._isRestoring = false;
                }, 100);
            }
            return true;
        };

        const tryRefresh = (attempt) => {
            const container = document.getElementById('hymap-container');
            if (!container || container.offsetWidth < 100 || container.offsetHeight < 100) {
                if (attempt < 15) {
                    this._refreshTimer = setTimeout(() => tryRefresh(attempt + 1), 30);
                }
                return;
            }

            // Primary restoration
            applyLayout();

            // Next animation frame for smooth synchronization with browser reflow
            if (typeof requestAnimationFrame === 'function') {
                requestAnimationFrame(() => {
                    applyLayout();
                });
            }

            // Verification pass to guarantee CSS reflow and aspect-ratio have completely settled
            this._refreshTimer = setTimeout(() => {
                applyLayout();
            }, 100);
        };

        tryRefresh(0);
    },

    ensureDependencies: function() {
        if (window.L && window.L.Control && (window.L.Control.Geocoder || window.L.Control.geocoder)) {
            return Promise.resolve();
        }
        if (this._loadingDeps) {
            return this._loadingDeps;
        }
        this._loadingDeps = new Promise((resolve, reject) => {
            if (!document.getElementById('leaflet-css')) {
                const link = document.createElement('link');
                link.id = 'leaflet-css';
                link.rel = 'stylesheet';
                link.href = 'https://unpkg.com/leaflet@1.9.4/dist/leaflet.css';
                document.head.appendChild(link);
            }
            if (!document.getElementById('leaflet-geocoder-css')) {
                const link = document.createElement('link');
                link.id = 'leaflet-geocoder-css';
                link.rel = 'stylesheet';
                link.href = 'https://unpkg.com/leaflet-control-geocoder/dist/Control.Geocoder.css';
                document.head.appendChild(link);
            }

            const loadScript = (src) => new Promise((res, rej) => {
                const s = document.createElement('script');
                s.src = src;
                s.onload = res;
                s.onerror = rej;
                document.body.appendChild(s);
            });

            const loadL = window.L ? Promise.resolve() : loadScript('https://unpkg.com/leaflet@1.9.4/dist/leaflet.js');
            const loadTurf = window.turf ? Promise.resolve() : loadScript('https://cdn.jsdelivr.net/npm/@turf/turf@6/turf.min.js');

            Promise.all([loadL, loadTurf]).then(() => {
                if (window.L && (!window.L.Control || (!window.L.Control.Geocoder && !window.L.Control.geocoder))) {
                    return loadScript('https://unpkg.com/leaflet-control-geocoder/dist/Control.Geocoder.js');
                }
            }).then(resolve).catch(reject);
        });
        return this._loadingDeps;
    },
    
    init: async function() {
        const container = document.getElementById('hymap-container');
        if (!container) return;

        await this.ensureDependencies();

        if (this.map && this.map._container !== container) {
            this.map.remove();
            this.map = null;
        }

        if (this.map) {
            setTimeout(() => {
                this.refreshSize();
            }, 50);
            return;
        }

        setTimeout(() => {
            if (this.map) return;
            const initialCenter = this.savedCenter || [12.9716, 77.5946];
            const initialZoom = this.savedZoom || 13;

            this.map = L.map('hymap-container', { 
                maxZoom: 24, 
                zoomControl: false,
                attributionControl: false,
                zoomSnap: 0.1,
                zoomDelta: 0.1
            }).setView(initialCenter, initialZoom);

            this.saveView();
            
            // Add OpenStreetMap tiles
            L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
                maxNativeZoom: 19,
                maxZoom: 24,
                attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
            }).addTo(this.map);
            
            // Add attribution control at bottom-left
            L.control.attribution({ position: 'bottomleft', prefix: false }).addTo(this.map);

            // Add custom zoom control
            L.control.zoom({ position: 'bottomright' }).addTo(this.map);
            
            // Add search control
            L.Control.geocoder({
                defaultMarkGeocode: false,
                position: 'topleft'
            }).on('markgeocode', (e) => {
                const bbox = e.geocode.bbox;
                const poly = L.polygon([
                    bbox.getSouthEast(),
                    bbox.getNorthEast(),
                    bbox.getNorthWest(),
                    bbox.getSouthWest()
                ]);
                this.map.fitBounds(poly.getBounds());
                this.saveView();
            }).addTo(this.map);
            
            // Prevent zooming in too much by dynamically setting maxZoom so that map width remains >= 25m
            const updateMaxZoom = () => {
                if (!this.map) return;
                const containerWidth = this.map.getSize().x;
                if (containerWidth <= 0) return;
                const center = this.map.getCenter();
                if (!center) return;
                const centerLat = center.lat;
                const svg = document.getElementById('polygon-editor-svg');
                let ratio = 0;
                if (svg && svg.hasAttribute('data-padding-ratio')) {
                    ratio = parseFloat(svg.getAttribute('data-padding-ratio'));
                }
                const activeWidthPx = containerWidth * (1 - ratio);
                if (activeWidthPx > 0) {
                    const maxZ = Math.log2((activeWidthPx * 156543.03 * Math.cos(centerLat * Math.PI / 180)) / 25.0);
                    this.map.setMaxZoom(maxZ);
                }
            };
            this.map.on('resize', updateMaxZoom);
            this.map.on('move', () => {
                updateMaxZoom();
                this.updateDistanceLabel(false);
            });
            this.map.on('moveend', () => {
                this.saveView();
                this.updateDistanceLabel(true);
            });
            this.map.on('dragend', () => {
                this.saveView();
                this.updateDistanceLabel(true);
            });
            this.map.on('zoomend', () => {
                this.saveView();
                this.updateDistanceLabel(true);
            });
            updateMaxZoom();
            
            this.map.whenReady(() => {
                this.saveView();
                this.updateDistanceLabel(false);
            });
        }, 100);
    },

    getCalculatedWidth: function(bounds) {
        let exactWidth = this.map.distance(bounds.getNorthWest(), bounds.getNorthEast());
        const svg = document.getElementById('polygon-editor-svg');
        if (svg && svg.hasAttribute('data-padding-ratio')) {
            const ratio = parseFloat(svg.getAttribute('data-padding-ratio'));
            exactWidth = exactWidth * (1 - ratio);
        }
        return exactWidth;
    },

    updateDistanceLabel: function(triggerElmish = true) {
        if (!this.map) return;
        const container = document.getElementById('hymap-container');
        if (!container || container.offsetWidth < 50 || container.offsetHeight < 50) return;

        const bounds = this.map.getBounds();
        if (!bounds || !bounds.isValid()) return;

        const calcWidth = this.getCalculatedWidth(bounds);
        let exactHeight = this.map.distance(bounds.getNorthWest(), bounds.getSouthWest());
        const svg = document.getElementById('polygon-editor-svg');
        if (svg && svg.hasAttribute('data-padding-ratio')) {
            const ratio = parseFloat(svg.getAttribute('data-padding-ratio'));
            exactHeight = exactHeight * (1 - ratio);
        }

        if (isNaN(calcWidth) || isNaN(exactHeight) || calcWidth < 10 || exactHeight < 10) return;
        
        let label = document.getElementById('hymap-distance-label');
        if (!label) {
            label = document.createElement('div');
            label.id = 'hymap-distance-label';
            label.style.cssText = "position: absolute; top: 6px; left: 50%; transform: translateX(-50%); z-index: 1000; background: transparent; font-size: 12px; font-weight: 700; color: #1a1a1a; text-shadow: 0px 0px 4px rgba(255,255,255,0.9), 0px 1px 2px rgba(255,255,255,1); pointer-events: none; letter-spacing: 0.5px;";
            const labelParent = document.getElementById('hymap-wrapper') || document.getElementById('map-and-svg-container') || container.parentElement;
            if (labelParent) labelParent.appendChild(label);
        }
        if (label) {
            label.innerText = 'Map Width: ' + Math.round(calcWidth) + ' meters';
        }
        
        if (triggerElmish && !this._isRestoring) {
            const roundedW = Math.round(calcWidth);
            const roundedH = Math.round(exactHeight);
            if (this._lastDispatchedWidth !== roundedW || this._lastDispatchedHeight !== roundedH) {
                this._lastDispatchedWidth = roundedW;
                this._lastDispatchedHeight = roundedH;
                if (this._dotnetRef) {
                    this._dotnetRef.invokeMethodAsync('UpdateMapDimensions', roundedW, roundedH);
                } else if (window._undoRedoDotNet) {
                    window._undoRedoDotNet.invokeMethodAsync('UpdateMapDimensions', roundedW, roundedH);
                }
            }
        }
    },

    lockMap: async function() {
        if (!this.map) return;
        this.saveView();

        // 1. Freeze the map
        this.map.dragging.disable();
        this.map.touchZoom.disable();
        this.map.doubleClickZoom.disable();
        this.map.scrollWheelZoom.disable();
        this.map.boxZoom.disable();
        this.map.keyboard.disable();
        if (this.map.tap) this.map.tap.disable();

        // 2. Calculate Width and Height in meters
        const bounds = this.map.getBounds();
        const northWest = bounds.getNorthWest();
        const southWest = bounds.getSouthWest();

        // Calculate the inner geographical bounds (excluding SVG padding)
        let innerNorth = bounds.getNorth();
        let innerSouth = bounds.getSouth();
        let innerEast = bounds.getEast();
        let innerWest = bounds.getWest();

        const svg = document.getElementById('polygon-editor-svg');
        if (svg && svg.hasAttribute('data-padding-ratio')) {
            const ratio = parseFloat(svg.getAttribute('data-padding-ratio'));
            const mapSize = this.map.getSize();
            
            // padding ratio is the total padding fraction (e.g. 0.166 => 16.6%)
            const padX = mapSize.x * (ratio / 2);
            const padY = mapSize.y * (ratio / 2);
            
            const innerNw = this.map.containerPointToLatLng(L.point(padX, padY));
            const innerSe = this.map.containerPointToLatLng(L.point(mapSize.x - padX, mapSize.y - padY));
            
            innerNorth = innerNw.lat;
            innerSouth = innerSe.lat;
            innerEast = innerSe.lng;
            innerWest = innerNw.lng;
        }
        
        const calcWidth = this.getCalculatedWidth(bounds);
        let exactHeight = this.map.distance(northWest, southWest);
        if (svg && svg.hasAttribute('data-padding-ratio')) {
            const ratio = parseFloat(svg.getAttribute('data-padding-ratio'));
            exactHeight = exactHeight * (1 - ratio);
        }
        


        // 3. Generate 100x100 Topography Grid via AWS Terrarium Tiles
        let elevations = [];
        try {
            const gridSize = 100;
            const z = Math.min(Math.round(this.map.getZoom()), 14); // Terrarium optimal max zoom
            
            const west = innerWest;
            const east = innerEast;
            const north = innerNorth;
            const south = innerSouth;
            
            const lon2tile = (lon, zoom) => Math.floor((lon + 180) / 360 * Math.pow(2, zoom));
            const lat2tile = (lat, zoom) => Math.floor((1 - Math.log(Math.tan(lat * Math.PI / 180) + 1 / Math.cos(lat * Math.PI / 180)) / Math.PI) / 2 * Math.pow(2, zoom));
            
            const minX = lon2tile(west, z);
            const maxX = lon2tile(east, z);
            const minY = lat2tile(north, z);
            const maxY = lat2tile(south, z);
            
            const canvas = document.createElement('canvas');
            const ctx = canvas.getContext('2d', { willReadFrequently: true });
            
            canvas.width = (maxX - minX + 1) * 256;
            canvas.height = (maxY - minY + 1) * 256;
            
            const tilePromises = [];
            for (let x = minX; x <= maxX; x++) {
                for (let y = minY; y <= maxY; y++) {
                    tilePromises.push(new Promise((resolve) => {
                        const img = new Image();
                        img.crossOrigin = "Anonymous";
                        img.onload = () => {
                            ctx.drawImage(img, (x - minX) * 256, (y - minY) * 256, 256, 256);
                            resolve();
                        };
                        img.onerror = () => resolve(); 
                        img.src = `https://s3.amazonaws.com/elevation-tiles-prod/terrarium/${z}/${x}/${y}.png`;
                    }));
                }
            }
            
            await Promise.all(tilePromises);
            
            // Sample the 100x100 grid
            for (let i = 0; i < gridSize; i++) {
                for (let j = 0; j < gridSize; j++) {
                    const lat = north - (north - south) * (i / (gridSize - 1));
                    const lon = west + (east - west) * (j / (gridSize - 1));
                    
                    const x = ((lon + 180) / 360 * Math.pow(2, z));
                    const y = ((1 - Math.log(Math.tan(lat * Math.PI / 180) + 1 / Math.cos(lat * Math.PI / 180)) / Math.PI) / 2 * Math.pow(2, z));
                    
                    const px = Math.floor((x - minX) * 256);
                    const py = Math.floor((y - minY) * 256);
                    
                    if (px >= 0 && px < canvas.width && py >= 0 && py < canvas.height) {
                        const pData = ctx.getImageData(px, py, 1, 1).data;
                        const elevation = (pData[0] * 256 + pData[1] + pData[2] / 256) - 32768;
                        elevations.push(elevation);
                    } else {
                        elevations.push(0);
                    }
                }
            }
        } catch (err) {
            console.error("Topography Extraction Error:", err);
        }

        // 4. Send topography data to F#
        const payload = JSON.stringify({
            widthMeters: Math.round(calcWidth),
            heightMeters: Math.round(exactHeight),
            elevations: elevations,
            extents: {
                north: innerNorth,
                south: innerSouth,
                east: innerEast,
                west: innerWest
            }
        });

        if (this._dotnetRef) {
            this._dotnetRef.invokeMethodAsync('ReceiveMapTopography', Math.round(calcWidth), Math.round(exactHeight), payload);
        } else if (window._undoRedoDotNet) {
            window._undoRedoDotNet.invokeMethodAsync('ReceiveMapTopography', Math.round(calcWidth), Math.round(exactHeight), payload);
        } else {
            const hiddenData = document.getElementById('hymap-data');
            const triggerBtn = document.getElementById('hymap-trigger');
            if (hiddenData && triggerBtn) {
                hiddenData.value = payload;
                triggerBtn.click();
            }
        }
    },

    unlockMap: function() {
        if (!this.map) return;
        // Unfreeze the map
        this.map.dragging.enable();
        this.map.touchZoom.enable();
        this.map.doubleClickZoom.enable();
        this.map.scrollWheelZoom.enable();
        this.map.boxZoom.enable();
        this.map.keyboard.enable();
        if (this.map.tap) this.map.tap.enable();
    },

    exportMapImage: async function() {
        if (!this.map) return;
        try {
            const bounds = this.map.getBounds();
            
            let innerNorth = bounds.getNorth();
            let innerSouth = bounds.getSouth();
            let innerEast = bounds.getEast();
            let innerWest = bounds.getWest();

            const svg = document.getElementById('polygon-editor-svg');
            if (svg && svg.hasAttribute('data-padding-ratio')) {
                const ratio = parseFloat(svg.getAttribute('data-padding-ratio'));
                const mapSize = this.map.getSize();
                const padX = mapSize.x * (ratio / 2);
                const padY = mapSize.y * (ratio / 2);
                const innerNw = this.map.containerPointToLatLng(L.point(padX, padY));
                const innerSe = this.map.containerPointToLatLng(L.point(mapSize.x - padX, mapSize.y - padY));
                innerNorth = innerNw.lat;
                innerSouth = innerSe.lat;
                innerEast = innerSe.lng;
                innerWest = innerNw.lng;
            }

            const z = Math.round(this.map.getZoom());
            const lon2tile = (lon, zoom) => Math.floor((lon + 180) / 360 * Math.pow(2, zoom));
            const lat2tile = (lat, zoom) => Math.floor((1 - Math.log(Math.tan(lat * Math.PI / 180) + 1 / Math.cos(lat * Math.PI / 180)) / Math.PI) / 2 * Math.pow(2, zoom));
            
            const minX = lon2tile(innerWest, z);
            const maxX = lon2tile(innerEast, z);
            const minY = lat2tile(innerNorth, z);
            const maxY = lat2tile(innerSouth, z);
            
            const canvas = document.createElement('canvas');
            const ctx = canvas.getContext('2d');
            
            canvas.width = (maxX - minX + 1) * 256;
            canvas.height = (maxY - minY + 1) * 256;
            
            const tilePromises = [];
            for (let x = minX; x <= maxX; x++) {
                for (let y = minY; y <= maxY; y++) {
                    tilePromises.push(new Promise((resolve) => {
                        const img = new Image();
                        img.crossOrigin = "Anonymous";
                        img.onload = () => {
                            ctx.drawImage(img, (x - minX) * 256, (y - minY) * 256, 256, 256);
                            resolve();
                        };
                        img.onerror = () => resolve(); 
                        img.src = `https://a.tile.openstreetmap.org/${z}/${x}/${y}.png`;
                    }));
                }
            }
            
            await Promise.all(tilePromises);
            
            // Crop it exactly to the inner bounds
            const nwPoint = this.map.project([innerNorth, innerWest], z);
            const sePoint = this.map.project([innerSouth, innerEast], z);
            
            const tileNwPoint = new L.Point(minX * 256, minY * 256);
            
            const cropX = nwPoint.x - tileNwPoint.x;
            const cropY = nwPoint.y - tileNwPoint.y;
            const cropW = sePoint.x - nwPoint.x;
            const cropH = sePoint.y - nwPoint.y;
            
            const finalCanvas = document.createElement('canvas');
            finalCanvas.width = cropW;
            finalCanvas.height = cropH;
            const finalCtx = finalCanvas.getContext('2d');
            finalCtx.drawImage(canvas, cropX, cropY, cropW, cropH, 0, 0, cropW, cropH);
            
            // Add OpenStreetMap attribution to the exported image
            finalCtx.font = "11px sans-serif";
            const text = "© OpenStreetMap contributors";
            const textWidth = finalCtx.measureText(text).width;
            
            finalCtx.fillStyle = "rgba(255, 255, 255, 0.7)";
            finalCtx.fillRect(cropW - textWidth - 10, cropH - 20, textWidth + 10, 20);
            
            finalCtx.fillStyle = "#333";
            finalCtx.fillText(text, cropW - textWidth - 5, cropH - 6);
            
            if (finalCanvas.toBlob && window.downloadFile) {
                finalCanvas.toBlob(blob => {
                    if (blob) {
                        window.downloadFile("hywe-map-extents.png", blob, "image/png");
                    }
                }, "image/png");
            } else {
                const dataUrl = finalCanvas.toDataURL("image/png");
                if (window.triggerDirectDownload) {
                    window.triggerDirectDownload(dataUrl, "hywe-map-extents.png");
                } else {
                    const a = document.createElement('a');
                    a.href = dataUrl;
                    a.download = "hywe-map-extents.png";
                    document.body.appendChild(a);
                    a.click();
                    setTimeout(() => a.remove(), 1000);
                }
            }
            
        } catch (err) {
            console.error("Map Image Extraction Error:", err);
        }
    }
};
