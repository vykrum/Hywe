module Hywe.Shell

open Bolero.Html
open ModelTypes

/// JSON-LD Structured Data Schema for HYWE
let jsonLd =
    script {
        attr.``type`` "application/ld+json"
        attr.id "hywe-schema"
        rawHtml (sprintf """
        {
          "@context": "https://schema.org",
          "@type": ["WebApplication", "CreativeWork", "SoftwareApplication"],
          "applicationCategory": "DesignApplication",
          "name": "HYWE",
          "alternateName": "HYWE – Hygrid Woven Ensemble",
          "url": "https://hywe.in",
          "logo": "https://hywe.in/images/web-app-manifest-192x192.png",
          "author": {
            "@type": "Person",
            "name": "Vikram Subbaiah",
            "url": "https://github.com/vykrum"
          },
          "description": "Weave controlled spatial layouts through an approach powered by design computation. An F#-driven sandbox for early-stage planning across all design scales.",
          "inLanguage": "en",
          "keywords": [ "spatial", "design", "wasm", "fsharp"],
          "license": "https://github.com/vykrum/Hywe/blob/main/LICENSE",
          "creativeWorkStatus": "InDevelopment",
          "isAccessibleForFree": true,
          "browserRequirements": "Requires a modern browser with WebAssembly support. Recommended: latest versions of Chrome, Edge, Firefox, or Safari.",
          "operatingSystem": "All",
          "datePublished": "%s",
          "dateModified": "%s"
        }
        """ PUBLISHED_DATE MODIFIED_DATE)
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

/// Offscreen hidden metadata and about content (SEO friendly)
let aboutSection =
    concat {
        h1 { attr.``class`` "offscreen-hidden"; text "HYWE – Hygrid Woven Ensemble" }
        
        section {
            attr.id "about-hywe"
            attr.``class`` "offscreen-hidden"
            
            h2 { text "About Hywe" }
            p {
                attr.style "font-style: italic; margin-top: -10px;"
                text "Hywe — "
                strong { text "Hygrid Woven Ensemble" }
            }

            p {
                strong { text "HYWE" }
                text " ("; em { text "Hygrid Woven Ensemble" }; text ") is a design-driven approach to spatial planning powered by design computation."
            }

            p { text "It is not a product — it’s a design computation philosophy rethinking how spatial configurations emerge. Developed from first principles with no reference to existing tools or paradigms, Hywe challenges the conventional evolution of space layout tools by treating diagrams not as endpoints, but as active generators of spatial logic." }

            p { text "Unlike traditional space planning software aimed at production workflows, Hywe operates in the speculative domain of early-stage intent. It prioritizes **flow-based hierarchy and spatial topology** over simple wall-to-wall adjacency. By introducing the concept of the "; em { text "Hygrid" }; text "—a hybrid orthogonal-hexagonal grid system—it enables designers to compose unconventional spatial topologies through structured, procedural definitions rather than object-based manipulation." }

            h3 { text "Philosophy" }
            p { text "Hywe is built around the belief that spatial reasoning can be computationally expressive without mimicking architectural software norms. The tool promotes a kind of design thinking where **spatial topology**—bubble-like in its initial intent—drives layout formation through a logic-native syntax. Every part of its system, from geometry to interactivity, has been constructed from scratch to reflect this procedural paradigm of flow-based hierarchy." }
            p { text "At its core, Hywe questions the assumption that digital tools must imitate human drafting. Instead, it proposes that computation itself can embody design intent — not as a secondary automation layer, but as a generative framework for decision-making. This perspective encourages designers to think algorithmically, transforming intuition into structured, procedural relationships. In doing so, Hywe enables a dialogue between spatial logic and creative agency that is both analytical and expressive." }
            p { text "The name "; strong { text "Hywe" }; text " reflects its foundation: a "; em { text "Hygrid" }; text "-based system that weaves together spatial definitions into an "; em { text "ensemble" }; text "—a coherent, emergent structure shaped by procedural logic rather than visual convention." }

            h3 { text "Technical Orientation" }
            p { text "Hywe’s technical architecture merges abstract logic with visual immediacy. Its geometry engine is written entirely in F#, compiled to WebAssembly for speed and precision. The interface, built on Bolero, allows designers to visualize the dynamic evolution of spatial systems in real time. Each polygon, relationship, and constraint is computed natively in the browser, making the experience seamless and platform-independent." }
            ul {
                li { text "Written in "; strong { text "F#" }; text " and compiled to "; strong { text ".NET WebAssembly (WASM)" }; text " via "; strong { text "Blazor" }; text "." }
                li { text "Interactive SVG canvas with dynamic polygon editing and logic-driven layout operations." }
                li { text "No dependency on visual precedents—entire computational stack developed independently." }
            }

            h3 { text "Current Phase" }
            p {
                strong { text "Hywe" }
                text " is an active design sandbox exploring how spatial design can emerge from logic rather than representation. It is not inspired by, nor comparable to, existing design software—because it is not software in the traditional sense. It is a system, a hypothesis, and an ongoing experiment in the design computation of space."
            }

            h3 { text "Repository" }
            ul {
                li { strong { text "GitHub: " }; a { attr.href "https://github.com/vykrum/Hywe"; attr.target "_blank"; text "github.com/vykrum/Hywe" } }
                li { strong { text "Author: " }; a { attr.href "https://github.com/vykrum"; attr.target "_blank"; text "Vikram Subbaiah" } }
            }

            h3 { text "Social" }
            ul {
                li { strong { text "LinkedIn: " }; a { attr.href "https://www.linkedin.com/company/hywein/"; attr.target "_blank"; text "linkedin.com/company/hywein" } }
                li { strong { text "X (formerly Twitter): " }; a { attr.href "https://x.com/_hywe_"; attr.target "_blank"; text "@_hywe_" } }
            }

            h3 { text "Direction" }
            p {
                strong { text "Hywe" }
                text " is a deeply personal project—one shaped by a singular vision and the ambition to explore spatial reasoning beyond the boundaries of precedent or tradition. It does not seek to adapt to established workflows or validate itself against known practices. Instead, it charts its own course, grounded in a conviction that spatial design can emerge from new computational logics yet to be fully imagined."
            }
            p { text "There is no defined roadmap. Hywe evolves in response to the ideas it generates, not to predetermined milestones or deliverables. Its development resists conformity, and embraces uncertainty—as a space for discovering possibilities that structured agendas might never permit." }
            p { text "The development of Hywe reflects an ongoing investigation into how design computation reshapes creative processes. It invites collaboration, critique, and interpretation — encouraging others to explore the intersection of logic, geometry, and narrative within spatial design. While Hywe continues to evolve as an active design sandbox, its goal is to provide a cohesive and implementable philosophy for future design tools." }
        }
    }

/// The fullscreen loading screen shown before WASM is ready
let loadingScreen (current: AppScreen) =
    let isHidden = current <> LoadingScreen
    div {
        attr.id "loading-frame"
        attr.style (if isHidden then "opacity: 0; pointer-events: none;" else "")
        
        video {
            attr.``class`` "fullscreen-bg"
            attr.autoplay true
            attr.muted true
            attr.loop true
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

        video {
            attr.id "intro-video"
            attr.``class`` "fullscreen-bg"
            attr.autoplay true
            attr.muted true
            attr.loop true
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
