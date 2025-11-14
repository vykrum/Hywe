/// <reference lib="webworker" />

declare const self: ServiceWorkerGlobalScope;
export { };

self.addEventListener("install", () => {
    console.log("[SW] install");
    self.skipWaiting();
});

self.addEventListener("activate", () => {
    console.log("[SW] activate");
    self.clients.claim();
});

// No caching – always hit the network
self.addEventListener("fetch", (event) => {
    // Allow the request to pass through untouched
    return;
});
