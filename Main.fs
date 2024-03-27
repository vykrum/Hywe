module Hywe.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Hexel
open Shape
open Bridge

type Model =
    {
        cls0 : int
        cls1 : int
        cls2 : int
        cls3 : int
        cls4 : int
        cls5 : int
        cls6 : int
        lbl0 : string
        lbl1 : string
        lbl2 : string
        lbl3 : string
        lbl4 : string
        lbl5 : string
        lbl6 : string
        shp1 : int
        sqn1 : int
        scl1 : int
    }

// Default Input
let initModel =
    {
        cls0 = 6
        cls1 = 4
        cls2 = 4
        cls3 = 6
        cls4 = 5
        cls5 = 6
        cls6 = 5
        lbl0 = ""
        lbl1 = ""
        lbl2 = ""
        lbl3 = ""
        lbl4 = ""
        lbl5 = ""
        lbl6 = ""
        shp1 = 1
        sqn1 = 1
        scl1 = 10
    }

type Message =
    | SetCls0 of int
    | SetCls1 of int
    | SetCls2 of int
    | SetCls3 of int
    | SetCls4 of int
    | SetCls5 of int
    | SetCls6 of int
    | LblCls0 of string
    | LblCls1 of string
    | LblCls2 of string
    | LblCls3 of string
    | LblCls4 of string
    | LblCls5 of string
    | LblCls6 of string
    | SetShp1 of int
    | SetSqn1 of int
    | SetScl1 of int

let update message model =
    match message with
    | SetCls0 value -> { model with cls0 = value }
    | SetCls1 value -> { model with cls1 = value }
    | SetCls2 value -> { model with cls2 = value }
    | SetCls3 value -> { model with cls3 = value }
    | SetCls4 value -> { model with cls4 = value }
    | SetCls5 value -> { model with cls5 = value }
    | SetCls6 value -> { model with cls6 = value }
    | LblCls0 value -> { model with lbl0 = value }
    | LblCls1 value -> { model with lbl1 = value }
    | LblCls2 value -> { model with lbl2 = value }
    | LblCls3 value -> { model with lbl3 = value }
    | LblCls4 value -> { model with lbl4 = value }
    | LblCls5 value -> { model with lbl5 = value }
    | LblCls6 value -> { model with lbl6 = value }
    | SetSqn1 value -> { model with sqn1 = value }
    | SetShp1 value -> { model with shp1 = value }
    | SetScl1 value -> { model with scl1 = value }

// Interface
let view model dispatch =      
    concat {
        // Header
        div{
            attr.``class`` "flex-container-1"
            // Name and Logo
            div{
                attr.``class`` "flex-container-2"
                attr.id "header"
                // Logo
                a{
                    attr.href "https://github.com/vykrum/Hywe"
                    attr.target "blank"
                    img{
                        attr.width "30"
                        attr.height "30"
                        attr.src "https://vykrum.github.io/Hywe/favicon-32x32.png"
                    }
                }
                // Title
                div{
                    attr.id "title"
                    " H Y W E"
                    }
                // Acronym
                div{
                    img{
                    attr.width "200"
                    attr.height "45"
                    attr.src "https://vykrum.github.io/Hywe/hyweLogoAcronym.png"
                    }
                }
                }
            // Introduction
            div{
                attr.id "description"
                p{
                    "Hywe is a space layout planning concept currently undergoing its formative stages of development being developed as an early stage design interface."
                    br
                    "Manipulating the color coded sliders controls the scale of corresponding clusters."
                }
            }
        }

        // Shape Sequence and Scale Controls
        div{
            // Shape
            div{
                attr.``class`` "flex-container-1"
                // Slider Shape
                input{
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.id "shp1"
                    attr.min "1"
                    attr.max "7"
                    bind.input.int model.shp1 (fun a -> dispatch (SetShp1 a))
                }
            }
            // Sequence
            div{
                attr.``class`` "flex-container-1"
                // Slider Sequence
                input{
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.id "sqn1"
                    attr.min "1"
                    attr.max "12"
                    bind.input.int model.sqn1 (fun a -> dispatch (SetSqn1 a))
                }
            }
            // Scale
            div{
                attr.``class`` "flex-container-1"
                // Slider Scale
                input{
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.id "scl1"
                    attr.min "5"
                    attr.max "30"
                    bind.input.int model.scl1 (fun a -> dispatch (SetScl1 a))
                }
            }
        }

        // Cluster Controls
        div{
            // Cluster 0
            div{
                attr.``class`` "flex-container-2"
                // Slider 0
                input{
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.id "cls0"
                    attr.min "6"
                    attr.max "25"
                    bind.input.int model.cls0 (fun a -> dispatch (SetCls0 a))
                }
                // Cluster 0 Size Percentage
                label{
                    attr.``class`` "label"
                    attr.id "cls0"
                    attr.``type`` "text"
                    $" {model.cls0 * 4}%%"
                }
                // Cluster 0 Name Input
                input{
                    attr.``class`` "input"
                    attr.``type`` "text"
                    attr.id "cls0"
                    bind.input.string model.lbl0 (fun a -> dispatch (LblCls0 a))
                }
            }
            // Cluster 1
            div{ 
                attr.``class`` "flex-container-2"
                // Slider 1
                input{
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.id "cls1"
                    attr.min "0"
                    attr.max "25"
                    bind.input.int model.cls1 (fun b -> dispatch (SetCls1 b))
                }
                // Cluster 1 Size Percentage
                label{
                    attr.``class`` "label"
                    attr.id "cls1"
                    attr.``type`` "text"
                    $" {model.cls1 * 4}%%"
                }
                // Cluster 1 Name Input
                input{
                    attr.``class`` "input"
                    attr.``type`` "text"
                    attr.id "cls1"
                    bind.input.string model.lbl1 (fun a -> dispatch (LblCls1 a))
                }
            }
            // Cluster 2
            div{
                attr.``class`` "flex-container-2"
                // Slider 2
                input{
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.id "cls2"
                    attr.min "0"
                    attr.max "25"
                    bind.input.int model.cls2 (fun c -> dispatch (SetCls2 c))
                }
                // Cluster 2 Size Percentage
                label{
                    attr.``class`` "label"
                    attr.id "cls2"
                    attr.``type`` "text"
                    $" {model.cls2 * 4}%%"
                }
                // Cluster 2 Name Input
                input{
                    attr.``class`` "input"
                    attr.``type`` "text"
                    attr.id "cls2"
                    bind.input.string model.lbl2 (fun a -> dispatch (LblCls2 a))
                }
            }
            // Cluster 3
            div{
                attr.``class`` "flex-container-2"
                // Slider 3
                input{
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.id "cls3"
                    attr.min "0"
                    attr.max "25"
                    bind.input.int model.cls3 (fun d -> dispatch (SetCls3 d))
                }
                // Cluster 3 Size Percentage
                label{
                    attr.``class`` "label"
                    attr.id "cls3"
                    attr.``type`` "text"
                    $" {model.cls3 * 4}%%"
                }
                // Cluster 3 Name Input
                input{
                    attr.``class`` "input"
                    attr.``type`` "text"
                    attr.id "cls3"
                    bind.input.string model.lbl3 (fun a -> dispatch (LblCls3 a))
                }
            }
            // Cluster 4
            div{
                attr.``class`` "flex-container-2"
                // Slider 4
                input{
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.id "cls4"
                    attr.min "0"
                    attr.max "25"
                    bind.input.int model.cls4 (fun e -> dispatch (SetCls4 e))
                }
                // Cluster 4 Size Percentage
                label{
                    attr.``class`` "label"
                    attr.id "cls4"
                    attr.``type`` "text"
                    $" {model.cls4 * 4}%%"
                }
                // Cluster 4 Name Input
                input{
                    attr.``class`` "input"
                    attr.``type`` "text"
                    attr.id "cls4"
                    bind.input.string model.lbl4 (fun a -> dispatch (LblCls4 a))
                }
            }
            // Cluster 5
            div{
                attr.``class`` "flex-container-2"
                // Slider 5
                input{
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.id "cls5"
                    attr.min "0"
                    attr.max "25"
                    bind.input.int model.cls5 (fun e -> dispatch (SetCls5 e))
                }
                // Cluster 5 Size Percentage
                label{
                    attr.``class`` "label"
                    attr.id "cls5"
                    attr.``type`` "text"
                    $" {model.cls5 * 4}%%"
                }
                // Cluster 5 Name Input
                input{
                    attr.``class`` "input"
                    attr.``type`` "text"
                    attr.id "cls5"
                    bind.input.string model.lbl5 (fun a -> dispatch (LblCls5 a))
                }
            }
            // Cluster 6
            div{
                attr.``class`` "flex-container-2"
                // Slider 6
                input{
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.id "cls6"
                    attr.min "0"
                    attr.max "25"
                    bind.input.int model.cls6 (fun g -> dispatch (SetCls6 g))
                }
                // Cluster 6 Size Percentage
                label{
                    attr.``class`` "label"
                    attr.id "cls6"
                    attr.``type`` "text"
                    $"{model.cls6 * 4}%%"
                }
                // Cluster 6 Name Input
                input{
                    attr.``class`` "input"
                    attr.``type`` "text"
                    attr.id "cls6"
                    bind.input.string model.lbl6 (fun a -> dispatch (LblCls6 a))
                }
            }   
            }

        // Scale
        let scl = model.scl1
        
        // Shape
        let shp = match model.shp1 with 
                    | 1 -> HxFl
                    | 2 -> HxPt
                    | 3 -> PrAn
                    | 4 -> PrFl
                    | 5 -> RhHr
                    | 6 -> RhVr
                    | _ -> QdSq

        // Sequence
        let sqn = match model.sqn1 with 
                    | 1 -> match shp with
                           |HxPt|PrFl|RhVr|QdSq -> VRCWEE
                           |HxFl|PrAn|RhHr -> HRCWNN
                    | 2 -> match shp with
                           |HxPt|PrFl|RhVr|QdSq -> VRCCEE
                           |HxFl|PrAn|RhHr -> HRCCNN
                    | 3 -> match shp with
                           |HxPt|PrFl|RhVr|QdSq -> VRCWSE
                           |HxFl|PrAn|RhHr -> HRCWNE
                    | 4 -> match shp with
                           |HxPt|PrFl|RhVr|QdSq -> VRCCSE
                           |HxFl|PrAn|RhHr -> HRCCNE
                    | 5 -> match shp with
                           |HxPt|PrFl|RhVr|QdSq -> VRCWSW
                           |HxFl|PrAn|RhHr -> HRCWSE
                    | 6 -> match shp with
                           |HxPt|PrFl|RhVr|QdSq -> VRCCSW
                           |HxFl|PrAn|RhHr -> HRCCSE
                    | 7 -> match shp with
                           |HxPt|PrFl|RhVr|QdSq -> VRCWWW
                           |HxFl|PrAn|RhHr -> HRCWSS
                    | 8 -> match shp with
                           |HxPt|PrFl|RhVr|QdSq -> VRCCWW
                           |HxFl|PrAn|RhHr -> HRCCSS
                    | 9 -> match shp with
                           |HxPt|PrFl|RhVr|QdSq -> VRCWNW
                           |HxFl|PrAn|RhHr -> HRCWSW
                    | 10 -> match shp with
                            |HxPt|PrFl|RhVr|QdSq -> VRCCNW
                            |HxFl|PrAn|RhHr -> HRCCSW
                    | 11 -> match shp with
                            |HxPt|PrFl|RhVr|QdSq -> VRCWNE
                            |HxFl|PrAn|RhHr -> HRCWNW
                    | 12 -> match shp with
                            |HxPt|PrFl|RhVr|QdSq -> VRCCNE
                            |HxFl|PrAn|RhHr -> HRCCNW
                    | _ -> VRCWEE
                   
        let (loc,wdt,hgt) = cls 
                                scl 
                                sqn 
                                ([| model.cls0
                                    model.cls1
                                    model.cls2
                                    model.cls3
                                    model.cls4
                                    model.cls5
                                    model.cls6 |])
        
        // Cluster Names
        let lbls = [| model.lbl0
                      model.lbl1
                      model.lbl2
                      model.lbl3
                      model.lbl4
                      model.lbl5
                      model.lbl6|]
        
        // Cluster Colors
        let clrs = [|"#ececce"
                     "#bccfd3"
                     "#867869"
                     "#687b7f"
                     "#c6bdb4"
                     "#3496a3"
                     "#eabdb5"|]
        
        // The Clusters SVG
        div{
            attr.``class`` "center"
            cluster (Array.zip3 loc lbls clrs) shp sqn scl wdt hgt
            }
        
        // Footer
        footer{
            a{
                attr.id "footer"
                attr.href "https://twitter.com/_hywe_"
                attr.target "blank"
                img{
                    attr.width "30"
                    attr.height "30"
                    attr.align "right"
                    attr.src "https://vykrum.github.io/Hywe/X.svg"
                    
                    }
            }
            
        }
    }

// Bolero component handling state updates and rendering the user interface
type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view