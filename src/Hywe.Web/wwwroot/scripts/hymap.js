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
            
            this.map = L.map('hymap-container', { maxZoom: 24, zoomControl: false }).setView([51.505, -0.09], 13);
            
            // Add OpenStreetMap tiles
            L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
                maxNativeZoom: 19,
                maxZoom: 24,
                attribution: '© OpenStreetMap'
            }).addTo(this.map);
            
            // Add custom zoom control
            L.control.zoom({ position: 'bottomright' }).addTo(this.map);
            
            // Add search control
            L.Control.geocoder({
                defaultMarkGeocode: false,
                position: 'bottomleft'
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
            
            // Prevent zooming in too much (ensure map width >= 30m)
            this.map.on('zoomend', () => {
                const bounds = this.map.getBounds();
                const calcWidth = this.getCalculatedWidth(bounds);
                if (calcWidth < 30) {
                    this.map.zoomOut();
                }
            });
            
            // Update distance label
            this.map.on('move', () => this.updateDistanceLabel());
            this.map.whenReady(() => this.updateDistanceLabel());
        }, 100);
    },

    getScaleDivisor: function(width, height) {
        const dim = Math.max(width, height);
        if (dim <= 0) return 1; // Prevent infinite loop if map bounds are invalid
        
        let divisor = 1;
        while (dim / divisor >= 100) {
            divisor *= 10;
        }
        while (dim / divisor < 10) {
            divisor /= 10;
        }
        return divisor;
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
        
        const divisor = this.getScaleDivisor(calcWidth, exactHeight);
        const scaledWidth = calcWidth / divisor;
        const scaledHeight = exactHeight / divisor;

        const label = document.getElementById('hymap-distance-label');
        if (label) {
            label.innerText = `Map Width: ${Math.round(scaledWidth)} units`;
        }
        
        const liveData = document.getElementById('hymap-live-data');
        const liveTrigger = document.getElementById('hymap-live-trigger');
        if (liveData && liveTrigger) {
            liveData.value = JSON.stringify({
                widthMeters: Math.round(scaledWidth),
                heightMeters: Math.round(scaledHeight)
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
        
        const calcWidth = this.getCalculatedWidth(bounds);
        let exactHeight = this.map.distance(northWest, southWest);
        const svg = document.getElementById('polygon-editor-svg');
        if (svg && svg.hasAttribute('data-padding-ratio')) {
            const ratio = parseFloat(svg.getAttribute('data-padding-ratio'));
            exactHeight = exactHeight * (1 - ratio);
        }
        
        const divisor = this.getScaleDivisor(calcWidth, exactHeight);
        const scaledWidth = calcWidth / divisor;
        const scaledHeight = exactHeight / divisor;

        // 3. Generate a grid of points within the current visible bounds for topography
        const bbox = [bounds.getWest(), bounds.getSouth(), bounds.getEast(), bounds.getNorth()];
        const cellSideKm = (calcWidth / 1000) / 10; 
        
        let points = [];
        try {
            const grid = turf.pointGrid(bbox, cellSideKm, { units: 'kilometers' });
            points = grid.features.map(f => ({
                latitude: f.geometry.coordinates[1],
                longitude: f.geometry.coordinates[0]
            }));
        } catch (e) {
            console.error("Error generating grid", e);
        }

        // 4. Fetch Topography
        let elevations = [];
        if (points.length > 0 && points.length < 500) {
            try {
                const response = await fetch('https://api.open-elevation.com/api/v1/lookup', {
                    method: 'POST',
                    headers: { 'Accept': 'application/json', 'Content-Type': 'application/json' },
                    body: JSON.stringify({ locations: points })
                });
                
                if (response.ok) {
                    const result = await response.json();
                    elevations = result.results;
                }
            } catch (err) {
                console.error("Topography API Error:", err);
            }
        }

        // 5. Write to hidden field and trigger click so F# picks it up
        const hiddenData = document.getElementById('hymap-data');
        const triggerBtn = document.getElementById('hymap-trigger');
        
        if (hiddenData && triggerBtn) {
            hiddenData.value = JSON.stringify({
                widthMeters: Math.round(scaledWidth),
                heightMeters: Math.round(scaledHeight),
                points: elevations
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
    }
};
