module Hywe.Index

open Bolero.Html
open ModelTypes

let coreScript =
    script {
        rawHtml """
        window.getUrlHash = function() {
            let hash = "";
            if (window.location.hash && window.location.hash.length > 1) {
                hash = window.location.hash.substring(1);
            } else {
                const url = window.location.href;
                const idx = url.indexOf('#');
                if (idx !== -1) hash = url.substring(idx + 1);
            }
            if (hash) {
                try { return decodeURIComponent(hash); } catch (e) { return hash; }
            }
            return "";
        };

        window.setUrlHash = function(hash) {
            if (hash) window.history.replaceState(null, null, "#" + encodeURIComponent(hash));
            else window.history.replaceState(null, null, window.location.pathname);
        };

        window.copyToClipboard = function(text) {
            if (navigator.clipboard && navigator.clipboard.writeText) {
                return navigator.clipboard.writeText(text).then(() => true).catch(() => false);
            } else {
                const textArea = document.createElement("textarea");
                textArea.value = text;
                document.body.appendChild(textArea);
                textArea.select();
                try {
                    const successful = document.execCommand('copy');
                    document.body.removeChild(textArea);
                    return Promise.resolve(successful);
                } catch (err) {
                    document.body.removeChild(textArea);
                    return Promise.resolve(false);
                }
            }
        };

        window.shareUrl = async (title, text, url) => {
            if (navigator.share) {
                try {
                    await navigator.share({ title, text, url });
                    return true;
                } catch (e) {
                    if (e.name !== 'AbortError') console.error(e);
                    return window.copyToClipboard(url);
                }
            }
            return window.copyToClipboard(url);
        };

        window.clickElement = (id) => {
            const el = document.getElementById(id);
            if (el) el.click();
        };

        window.downloadFile = function (fileName, content, contentType) {
            const blob = new Blob([content], { type: contentType });
            const url = URL.createObjectURL(blob);
            const a = document.createElement("a");
            a.href = url;
            a.download = fileName;
            document.body.appendChild(a);
            a.click();
            setTimeout(() => {
                document.body.removeChild(a);
                URL.revokeObjectURL(url);
            }, 0);
        };

        window.openReport = function(html) {
            const w = window.open('', '_blank');
            w.document.write(html);
            w.document.close();
            w.focus();
            setTimeout(() => w.print(), 600);
        };

        // Keyboard Shortcuts Handler
        document.addEventListener('keydown', function(e) {
            if (!window._undoRedoDotNet) return;
            const tag = e.target.tagName;
            if (tag === 'INPUT' || tag === 'TEXTAREA') return;
            if (e.ctrlKey && !e.shiftKey && e.key === 'z') { 
                e.preventDefault(); window._undoRedoDotNet.invokeMethodAsync('HandleUndo'); 
            } else if ((e.ctrlKey && e.key === 'y') || (e.ctrlKey && e.shiftKey && e.key === 'z')) { 
                e.preventDefault(); window._undoRedoDotNet.invokeMethodAsync('HandleRedo'); 
            }
        });
        """
    }

/// The top navigation header with branding
let siteHeader =
    header {
        attr.``class`` "site-header"
        div {
            img {
                attr.src "https://hywe.in/images/icon-32x32.png"
                attr.width "30"
                attr.height "30"
                attr.alt "HYWE logo"
            }
        }
        div { attr.``class`` "title"; text "H Y W E" }
        div {
            attr.``class`` "logo-acronym"
            img {
                attr.src "https://vykrum.github.io/Hywe/images/hyweLogoAcronym.png"
                attr.width "200"
                attr.height "45"
                attr.alt "HYWE acronym"
            }
        }
    }


/// The fullscreen loading screen shown before WASM is ready
let loadingScreen (current: AppScreen) =
    let isHidden = current <> LoadingScreen
    div {
        attr.id "loading-frame"
        attr.style (if isHidden then "opacity: 0; pointer-events: none;" else "")
        
        if not isHidden then
            video {
                attr.``class`` "fullscreen-bg"
                attr.autoplay true
                attr.muted true
                attr.loop true
                "playsinline" => "playsinline"
                source { attr.src "images/loader.webm"; attr.``type`` "video/webm" }
                source { attr.src "images/loader.mp4"; attr.``type`` "video/mp4" }
            }
        section {
            attr.id "intro-load"
            attr.``class`` "intro-section"
            text "Weave spatial configurations with"
            br {}
            br {}
            strong { text " H Y W E " }
            br {}
            br {}
            text "An endogenous space planning concept that introduces a novel and distinctive approach to early-stage design."
            br {}
            br {}
            text "Outline the intended flow-based hierarchy to generate spatial configurations defined by sequence and connections."
            div {
                attr.``class`` "loadingText"
                text "Loading"
                span {}
            }
        }
    }

/// The introduction splash screen shown after loading
let introSplash (current: AppScreen) (dispatch: Message -> unit) =
    let isVisible = current = IntroScreen
    let isMain = current = MainScreen
    
    div {
        attr.id "introduction"
        attr.``class`` (if isVisible then "ready" else if isMain then "fade-out" else "")
        attr.style (if isMain then "pointer-events: none; display: none;" else "")
            
        on.click (fun _ -> dispatch TransitionToMain)

        if not isMain then
            video {
                attr.id "intro-video"
                attr.``class`` "fullscreen-bg"
                attr.autoplay true
                attr.muted true
                attr.loop true
                "playsinline" => "playsinline"
                source { attr.src "images/loader.webm"; attr.``type`` "video/webm" }
                source { attr.src "images/loader.mp4"; attr.``type`` "video/mp4" }
            }
        section {
            attr.id "intro-main"
            attr.``class`` "intro-section"
            text "Weave spatial configurations with"
            br {}
            br {}
            strong { text " H Y W E " }
            br {}
            br {}
            text "An endogenous space planning concept that introduces a novel and distinctive approach to early-stage design."
            br {}
            br {}
            text "Outline the intended flow-based hierarchy to generate spatial configurations defined by sequence and connections."
            
            div {
                attr.``class`` ("tapText" + (if isVisible then " visible" else ""))
                attr.style "margin-top: 16px;"
                text "Tap to Continue"
                span {}
            }
        }
    }

/// The site footer with social links and license
let siteFooter (current: AppScreen) =
    let isVisible = current = MainScreen
    footer {
        attr.id "footer"
        attr.``class`` (if isVisible then "fade-in" else "")
        attr.style (if isVisible then "flex-wrap: wrap; justify-content: center; display: flex; opacity: 1; transition: opacity 0.5s ease; flex-direction: column; align-items: center;" else "display: none; opacity: 0;")
        
        div {
            attr.style "display: flex; gap: 25px; padding: 20px;"
            a {
                attr.href "https://forms.gle/TnH8ghGYz3ugEfWg9"
                attr.target "_blank"
                attr.rel "noopener noreferrer"
                img { attr.width "20"; attr.height "20"; attr.src "https://vykrum.github.io/Hywe/images/message.svg"; attr.alt "Email"; attr.style "display: block;" }
            }
            a {
                attr.href "https://www.linkedin.com/company/hywein/"
                attr.target "_blank"
                attr.rel "noopener noreferrer"
                img { attr.width "20"; attr.height "20"; attr.src "https://vykrum.github.io/Hywe/images/linkedin.svg"; attr.alt "LinkedIn"; attr.style "display: block;" }
            }
            a {
                attr.href "https://x.com/_hywe_"
                attr.target "_blank"
                attr.rel "noopener noreferrer"
                img { attr.width "20"; attr.height "20"; attr.src "https://vykrum.github.io/Hywe/images/X.svg"; attr.alt "X (Twitter)"; attr.style "display: block;" }
            }
            a {
                attr.href "https://github.com/vykrum/Hywe"
                attr.target "_blank"
                attr.rel "noopener noreferrer"
                img { attr.width "20"; attr.height "20"; attr.src "https://vykrum.github.io/Hywe/images/github.svg"; attr.alt "GitHub"; attr.style "display: block;" }
            }
            a {
                attr.href "https://huggingface.co/vykrum/datasets"
                attr.target "_blank"
                attr.rel "noopener noreferrer"
                img { attr.width "20"; attr.height "20"; attr.src "https://vykrum.github.io/Hywe/images/hugging-face.svg"; attr.alt "HuggingFace"; attr.style "display: block;" }
            }
        }
        div {
            attr.``class`` "license-footer"
            text "© 2022–2026 Vikram Subbaiah · Released under the "
            a {
                attr.href "https://github.com/vykrum/Hywe/blob/394fddf8edb5c43f594d008c0c876e092f4d38cf/LICENSE#L11"
                attr.target "_blank"
                text "MIT License"
            }
        }
    }
