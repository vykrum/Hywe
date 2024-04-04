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
            attr.``style`` "margin-top: 0px;background: #d3d3d1; color: #363636; flex-direction: column;"
            // Name and Logo
            div{
                attr.``class`` "flex-container"
                attr.``style`` "width: 100%;height: 37px;opacity: 1;background: #363636;padding-left: 5px;padding-top: 5px;"
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
                    attr.``style`` "color: white;font-family: 'Segoe UI';font-size: 20px;font-weight: normal;padding-left: 10px;padding-right: 10px;padding-bottom: 7px;"
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
                attr.``style`` "font-family: Segoe UI; font-size: 18px; color: #363636; padding-left: 12px;padding-right: 10px;padding-bottom: 5px;"
                p{
                    "Hywe is a space layout planning concept currently undergoing its formative stages of development being developed as an early stage design interface."
                    br
                    "Manipulating the color coded sliders controls the scale of corresponding clusters."
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

        // Shape Sequence and Scale Controls
        div{
            // Shape
            div{
                attr.``class`` "flex-container"
                
                // Slider Shape
                input{
                    attr.``style`` $"background:#d3d3d1;border:none;"
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.min "1"
                    attr.max "7"
                    bind.input.int model.shp1 (fun a -> dispatch (SetShp1 a))
                }
                // Shape Label
                label{
                    attr.``class`` "label"
                    attr.``style`` $"background:#d3d3d1;border:none;"
                    attr.``type`` "text"
                    $" shape "
                }
                // Shape Value
                label{
                    attr.``class`` "label"
                    attr.``style`` $"background:#d3d3d1;border:none;width:15%%;"
                    attr.``type`` "text"
                    $"{shp}" 
                }
            }
            // Sequence
            div{
                attr.``class`` "flex-container"
                // Slider Sequence
                input{
                    attr.``style`` $"background: #d3d3d1;border:none;"
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.min "1"
                    attr.max "12"
                    bind.input.int model.sqn1 (fun a -> dispatch (SetSqn1 a))
                }
                // Sequence Label
                label{
                    attr.``class`` "label"
                    attr.``style`` $"background:#d3d3d1;border:none;"
                    attr.``type`` "text"
                    $" sqnce "
                }
                // Sequence Value
                label{
                    attr.``class`` "label"
                    attr.``style`` $"background: #d3d3d1; border: none; width: 15%%;"
                    attr.``type`` "text"
                    $"{sqn}"
                }
            }
            // Scale
            div{
                attr.``class`` "flex-container"
                // Slider Scale
                input{
                    attr.``style`` $"background:#d3d3d1;border:none;"
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.min "5"
                    attr.max "30"
                    bind.input.int model.scl1 (fun a -> dispatch (SetScl1 a))
                }
                // Scale Label
                label{
                    attr.``class`` "label"
                    attr.``style`` $"background:#d3d3d1;border:none;"
                    attr.``type`` "text"
                    $" scale "
                }
                // Scale Value
                label{
                    attr.``class`` "label"
                    attr.``style`` $"background:#d3d3d1;border: none;width:15%%;"
                    attr.``type`` "text"
                    $"{model.scl1}" 
                }
            }
        }

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
        
        // Cluster Controls
        div{
            // Cluster 0
            div{
                attr.``class`` "flex-container"
                // Slider 0
                input{
                    attr.``style`` $"background: {clrs[0]}; border: none;"
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.min "6"
                    attr.max "25"
                    bind.input.int model.cls0 (fun a -> dispatch (SetCls0 a))
                }
                // Cluster 0 Size Percentage
                label{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` $"background: {clrs[0]}; border: none;"
                    $" {model.cls0 * 4}%%"
                }
                // Cluster 0 Name Input
                input{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` $"background: {clrs[0]}; border: none; width: 15%%;"
                    bind.input.string model.lbl0 (fun a -> dispatch (LblCls0 a))
                }
            }
            // Cluster 1
            div{ 
                attr.``class`` "flex-container"
                // Slider 1
                input{
                    attr.``style`` $"background: {clrs[1]}; border: none;"
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.min "0"
                    attr.max "25"
                    bind.input.int model.cls1 (fun b -> dispatch (SetCls1 b))
                }
                // Cluster 1 Size Percentage
                label{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` $"background: {clrs[1]}; border: none;"
                    $" {model.cls1 * 4}%%"
                }
                // Cluster 1 Name Input
                input{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` $"background: {clrs[1]}; border: none; width: 15%%;"
                    bind.input.string model.lbl1 (fun a -> dispatch (LblCls1 a))
                }
            }
            // Cluster 2
            div{
                attr.``class`` "flex-container"
                // Slider 2
                input{
                    attr.``style`` $"background: {clrs[2]}; border: none;"
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.min "0"
                    attr.max "25"
                    bind.input.int model.cls2 (fun c -> dispatch (SetCls2 c))
                }
                // Cluster 2 Size Percentage
                label{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` $"background: {clrs[2]}; border: none;"
                    $" {model.cls2 * 4}%%"
                }
                // Cluster 2 Name Input
                input{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` $"background: {clrs[2]}; border: none; width: 15%%;"
                    bind.input.string model.lbl2 (fun a -> dispatch (LblCls2 a))
                }
            }
            // Cluster 3
            div{
                attr.``class`` "flex-container"
                // Slider 3
                input{
                    attr.``style`` $"background: {clrs[3]}; border: none;"
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.min "0"
                    attr.max "25"
                    bind.input.int model.cls3 (fun d -> dispatch (SetCls3 d))
                }
                // Cluster 3 Size Percentage
                label{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` $"background: {clrs[3]}; border: none;"
                    $" {model.cls3 * 4}%%"
                }
                // Cluster 3 Name Input
                input{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` $"background: {clrs[3]}; border: none; width: 15%%;"
                    bind.input.string model.lbl3 (fun a -> dispatch (LblCls3 a))
                }
            }
            // Cluster 4
            div{
                attr.``class`` "flex-container"
                // Slider 4
                input{
                    attr.``style`` $"background: {clrs[4]}; border: none;"
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.min "0"
                    attr.max "25"
                    bind.input.int model.cls4 (fun e -> dispatch (SetCls4 e))
                }
                // Cluster 4 Size Percentage
                label{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` $"background: {clrs[4]}; border: none;"
                    $" {model.cls4 * 4}%%"
                }
                // Cluster 4 Name Input
                input{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` $"background: {clrs[4]}; border: none; width: 15%%;"
                    bind.input.string model.lbl4 (fun a -> dispatch (LblCls4 a))
                }
            }
            // Cluster 5
            div{
                attr.``class`` "flex-container"
                // Slider 5
                input{
                    attr.``style`` $"background: {clrs[5]}; border: none;"
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.min "0"
                    attr.max "25"
                    bind.input.int model.cls5 (fun e -> dispatch (SetCls5 e))
                }
                // Cluster 5 Size Percentage
                label{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` $"background: {clrs[5]}; border: none;"
                    $" {model.cls5 * 4}%%"
                }
                // Cluster 5 Name Input
                input{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` $"background: {clrs[5]}; border: none; width: 15%%;"
                    bind.input.string model.lbl5 (fun a -> dispatch (LblCls5 a))
                }
            }
            // Cluster 6
            div{
                attr.``class`` "flex-container"
                // Slider 6
                input{
                    attr.``style`` $"background: {clrs[6]}; border: none;"
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.min "0"
                    attr.max "25"
                    bind.input.int model.cls6 (fun g -> dispatch (SetCls6 g))
                }
                // Cluster 6 Size Percentage
                label{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` $"background: {clrs[6]}; border: none;"
                    $"{model.cls6 * 4}%%"
                }
                // Cluster 6 Name Input
                input{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` $"background: {clrs[6]}; border: none; width: 15%%;"
                    bind.input.string model.lbl6 (fun a -> dispatch (LblCls6 a))
                }
            }   
            }
                  
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
        
        // The Clusters SVG
        div{
            attr.``class`` "center"
            cluster (Array.zip3 loc lbls clrs) shp sqn scl wdt hgt
            }
        
        // Nested Clusters
        div{
            attr.``class`` "label1"
            attr.``style`` $"background:#d3d3d1;border:none;width:100%%; margin-left:0px;padding-left:10px;"
            label{
                    attr.``type`` "text"
                    $" Foyer "
                }
            
            div{
                attr.``class`` "label1"
                attr.``style`` "background:#85C1E9;border:none;"
                label{
                    attr.``type`` "text"
                    $" Living "
                    }
                div{
                    attr.``class`` "label1"
                    attr.``style`` $"background:#D6DBDF;border:none;"
                    label{
                        attr.``type`` "text"
                        $" Dining "
                    }
                    div{
                        attr.``class`` "label1"
                        attr.``style`` $"background:#FCF3CF;border:none;"
                        label{
                            attr.``type`` "text"
                            $" Kitchen "
                        }
                        div{
                            attr.``class`` "label1"
                            attr.``style`` $"background:#FADBD8;border:none;"
                            label{
                                attr.``type`` "text"
                                $" Utility "
                        }
                        }
                    }
                    div{
                        attr.``class`` "label1"
                        attr.``style`` $"background:#FEF5E7;border:none;"
                        label{
                            attr.``type`` "text"
                            $" Bed-1 "
                        }
                        div{
                            attr.``class`` "label1"
                            attr.``style`` $"background:#D4EFDF;border:none;"
                            label{
                                attr.``type`` "text"
                                $" Bath-1 "
                        }
                        }
                    }
                    div{
                        attr.``class`` "label1"
                        attr.``style`` "background:#F5CBA7;border:none;"
                        label{
                            attr.``type`` "text"
                            $" Bed-2 "
                        }
                        div{
                            attr.``class`` "label1"
                            attr.``style`` "background:#EBDEF0;border:none;"
                            label{
                                attr.``type`` "text"
                                $" Closet-2 "
                            }
                            div{
                                attr.``class`` "label1"
                                attr.``style`` "background:#AED6F1;border:none;"
                                label{
                                    attr.``type`` "text"
                                    $" Bath-2 "
                                }
                            }
                        }
                    }
                    div{
                        attr.``class`` "label1"
                        attr.``style`` "background:#FCF3CF;border:none;"
                        label{
                            attr.``type`` "text"
                            $" Bed-3 "
                        }
                        div{
                            attr.``class`` "label1"
                            attr.``style`` "background:#F0B27A;border:none;"
                            label{
                                attr.``type`` "text"
                                $" Closet-3 "
                            }
                        }
                        div{
                            attr.``class`` "label1"
                            attr.``style`` "background:#F5CBA7;border:none;"
                            label{
                                attr.``type`` "text"
                                $" Bath-3 "
                            }
                        }
                    }
                }
                div{
                    attr.``class`` "label1"
                    attr.``style`` "background:#D0ECE7;border:none;"
                    label{
                        attr.``type`` "text"
                        $" Staircase "
                    }
                }
            }
            div{
                attr.``class`` "label1"
                attr.``style`` "background:#D6EAF8;border:none;"
                label{
                    attr.``type`` "text"
                    $" Study "
                }
                    
            }    
            
        }   

        // Footer
        footer{
            a{
                attr.``style`` "position: fixed;left: 0;bottom: 0;width: 99%;padding-bottom: 10px;"
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