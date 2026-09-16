module Shell

open Bolero.Html
open ModelTypes

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
            text "A relational, flow-based spatial design environment for early-stage architecture."
            br {}
            br {}
            text "Outline the intended hierarchy to generate spatial configurations defined by sequence and connections."
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
            text "A relational, flow-based spatial design environment for early-stage architecture."
            br {}
            br {}
            text "Outline the intended hierarchy to generate spatial configurations defined by sequence and connections."
            
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
                attr.href "https://linkedin.hywe.in"
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
                attr.href "https://repo.hywe.in"
                attr.target "_blank"
                attr.rel "noopener noreferrer"
                img { attr.width "20"; attr.height "20"; attr.src "https://vykrum.github.io/Hywe/images/github.svg"; attr.alt "GitHub"; attr.style "display: block;" }
            }
            a {
                attr.href "https://data.hywe.in"
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
