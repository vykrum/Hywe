// @ts-nocheck
// Global stash for the PWA install prompt to capture it as early as possible
window.hyweDeferredPrompt = null;
window.addEventListener('beforeinstallprompt', (e) => {
    e.preventDefault();
    window.hyweDeferredPrompt = e;
    console.log("Hywe: Install prompt captured (head).");
    // If the PWA bridge is already registered, notify it immediately
    if (window.hywePwaDotNetRef) {
        window.hywePwaDotNetRef.invokeMethodAsync('SetInstallPromptAvailable', true);
    }
});
