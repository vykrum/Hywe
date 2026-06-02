// window.Hymap namespace to expose functions to F# Bolero
window.Hymap = {
    map: null,
    
    init: function() {
        const container = document.getElementById('hymap-container');
        if (!container) return;

        // If the map exists but the container element was destroyed and recreated by Bolero,
        // we must destroy the old map instance so Leaflet can attach to the new container.
        if (this.map && this.map._container !== container) {
            this.map.remove();
            this.map = null;
        }

        if (this.map) {
            // Already initialized, just force it to recalculate its size
            setTimeout(() => {
                this.map.invalidateSize();
                this.updateDistanceLabel();
            }, 100);
            return;
        }
        // Initialize the map
        // Add a slight delay to ensure Bolero has rendered the container
        setTimeout(() => {
            if (this.map) return; // double check inside timeout
            this.map = L.map('hymap-container', { 
                maxZoom: 24, 
                zoomControl: false,
                zoomSnap: 0.1,
                zoomDelta: 0.1
            }).setView([12.9716, 77.5946], 13);
            
            // Add OpenStreetMap tiles
            L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
                maxNativeZoom: 19,
                maxZoom: 24,
                attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
            }).addTo(this.map);
            
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
            }).addTo(this.map);
            
            // Prevent zooming in too much by dynamically setting maxZoom so that map width remains >= 25m
            const updateMaxZoom = () => {
                const centerLat = this.map.getCenter().lat;
                const containerWidth = this.map.getSize().x;
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
            this.map.on('move', updateMaxZoom);
            updateMaxZoom();
            
            // Update distance label
            this.map.on('move', () => this.updateDistanceLabel());
            this.map.whenReady(() => this.updateDistanceLabel());
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

    updateDistanceLabel: function() {
        if (!this.map) return;
        const bounds = this.map.getBounds();
        const calcWidth = this.getCalculatedWidth(bounds);
        let exactHeight = this.map.distance(bounds.getNorthWest(), bounds.getSouthWest());
        const svg = document.getElementById('polygon-editor-svg');
        if (svg && svg.hasAttribute('data-padding-ratio')) {
            const ratio = parseFloat(svg.getAttribute('data-padding-ratio'));
            exactHeight = exactHeight * (1 - ratio);
        }
        
        let label = document.getElementById('hymap-distance-label');
        if (!label) {
            label = document.createElement('div');
            label.id = 'hymap-distance-label';
            label.style.cssText = "position: absolute; top: 15px; left: 50%; transform: translateX(-50%); z-index: 1000; background: transparent; font-size: 13px; font-weight: 700; color: #1a1a1a; text-shadow: 0px 0px 4px rgba(255,255,255,0.9), 0px 1px 2px rgba(255,255,255,1); pointer-events: none; letter-spacing: 0.5px;";
            const container = document.getElementById('hymap-container');
            if (container) container.parentElement.appendChild(label);
        }
        if (label) {
            label.innerText = `Map Width: ${Math.round(calcWidth)} meters`;
        }
        
        const liveData = document.getElementById('hymap-live-data');
        const liveTrigger = document.getElementById('hymap-live-trigger');
        if (liveData && liveTrigger) {
            liveData.value = JSON.stringify({
                widthMeters: Math.round(calcWidth),
                heightMeters: Math.round(exactHeight)
            });
            liveTrigger.click();
        }
    },

    lockMap: async function() {
        if (!this.map) return;

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

        // 4. Write to hidden field and trigger click so F# picks it up
        const hiddenData = document.getElementById('hymap-data');
        const triggerBtn = document.getElementById('hymap-trigger');
        
        if (hiddenData && triggerBtn) {
            hiddenData.value = JSON.stringify({
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
            triggerBtn.click();
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
            
            const dataUrl = finalCanvas.toDataURL("image/png");
            
            const a = document.createElement('a');
            a.href = dataUrl;
            a.download = "hywe-map-extents.png";
            a.click();
            
        } catch (err) {
            console.error("Map Image Extraction Error:", err);
        }
    }
};
