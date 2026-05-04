// Service Worker for Hywe
const CACHE_NAME = 'hywe-cache-v1';

self.addEventListener('install', (event) => {
    self.skipWaiting();
});

self.addEventListener('activate', (event) => {
    event.waitUntil(clients.claim());
});

// A fetch handler is REQUIRED for PWA installability.
// This is a "pass-through" handler that lets the browser handle the network request
// but satisfies the PWA requirement.
self.addEventListener('fetch', (event) => {
    // We don't need to cache anything manually as Blazor handles assets,
    // but the presence of this listener is mandatory.
    return;
});
