module Hywe.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Hexel
open Coxel
open Shape
open Bridge
open Parse

type Model =
    {
        shp1 : int
        sqn1 : int
        scl1 : int
        spcStr1 : string
        spcStr2 : string
    }

// Default Input
let initModel =
    {
        shp1 = 4
        sqn1 = 13
        scl1 = 10
        spcStr1 = "(1/7/Foyer),(2/12/Living),(3/12/Dining),(1.1/10/Study),(2.1/10/Staircase),(3.1/12/Kitchen),(3.2/14/Bed-1),(3.3/18/Bed-2),(3.4/18/Bed-3),(3.1.1/6/Utility),(3.2.1/8/Bath-1),(3.3.1/10/Closet-2),(3.4.1/10/Closet-3),(3.4.2/10/Bath-3),(3.3.1.1/10/Bath-2)"
        spcStr2 = "(1/7/Foyer),(2/12/Living),(3/16/Dining),(3.1/12/Kitchen),(3.2/16/Bed-1),(3.3/18/Bed-2),(3.1.1/7/Utility),(3.2.1/10/Closet-1),(3.3.1/10/Closet-2),(3.3.2/10/Bath-2),(3.2.1.1/10/Bath-1)"
    }

type Message =
    | SetShp1 of int
    | SetSqn1 of int
    | SetScl1 of int
    | SetSpcStr1 of string
    | SetSpcStr2

let update message model =
    match message with
    | SetSqn1 value -> { model with sqn1 = value }
    | SetShp1 value -> { model with shp1 = value }
    | SetScl1 value -> { model with scl1 = value }
    | SetSpcStr1 value -> { model with spcStr1 = value }
    | SetSpcStr2 -> { model with spcStr2 = model.spcStr1}

// Interface
let view model dispatch =      
    concat {
        // Header
        div{
            attr.``style`` "margin-top: 0px;
                            background: #d3d3d1; 
                            color: #363636; 
                            flex-direction: column;"
            // Name and Logo
            div{
                attr.``class`` "flex-container"
                attr.``style`` "width: 100%;
                                height: 37px;
                                opacity: 1;
                                background: #363636;
                                padding-left: 5px;
                                padding-top: 5px;"
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
                    attr.``style`` "color: white;
                                    font-family: 'Optima', Candara, Calibri;
                                    font-size: 20px;
                                    font-weight: normal;
                                    padding-left: 10px;
                                    padding-right: 10px;
                                    padding-bottom: 7px;"
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
                attr.``style`` "font-family: 'Optima', Candara, Calibri; 
                                font-size: 18px; 
                                color: #363636; 
                                padding-left: 12px;
                                padding-right: 10px;
                                padding-bottom: 5px;"
                p{
                    "Hywe is an endogenous space planning concept currently undergoing its formative stages of development as an early stage design interface."
                    br
                    "Weave spatial layouts at a high level of abstraction using properly formatted (heirarchy/size/label) instructions."
                }
            }
        }

        // Scale
        let scl = model.scl1
        
        // Shape
        let shp = match model.shp1 with 
                    | 1 -> Sqr
                    | 2 -> Arw
                    | 3 -> Prl
                    | _ -> Hxg

        // Sequence
        let sqn = match model.sqn1 with 
                    | 1 -> VRCWEE 
                    | 2 -> VRCCEE    
                    | 3 -> VRCWSE
                    | 4 -> VRCCSE
                    | 5 -> VRCWSW
                    | 6 -> VRCCSW
                    | 7 -> VRCWWW
                    | 8 -> VRCCWW
                    | 9 -> VRCWNW
                    |10 -> VRCCNW
                    |11 -> VRCWNE
                    |12 -> VRCCNE
                    |13 -> HRCWNN
                    |14 -> HRCCNN
                    |15 -> HRCWNE
                    |16 -> HRCCNE
                    |17 -> HRCWSE
                    |18 -> HRCCSE
                    |19 -> HRCWSS
                    |20 -> HRCCSS
                    |21 -> HRCWSW
                    |22 -> HRCCSW
                    |23 -> HRCWNW
                    |24 -> HRCCNW
                    | _ -> VRCWEE

        // Nested Coxels Basic Framework
        let bsNs = hxlVld sqn (AV(1,4,0))
        let bsOc = 
            match sqn with 
            | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE 
                -> let a,b,c = hxlCrd bsNs
                   Array.append (hxlOrt sqn (hxlVld sqn (AV(a-100,b-2,c))) 200 false) (adjacent sqn (hxlVld sqn (AV(0,0,0))))
                   |> allAV true
            | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW 
                -> let a,b,c = hxlCrd bsNs
                   Array.append (hxlOrt sqn (hxlVld sqn (AV(a-104,b-2,c))) 200 false) (adjacent sqn (hxlVld sqn (AV(0,0,0))))
                   |> allAV true

        // Universal Controls
        div{
            attr.``class`` "flex-container"
            attr.``style`` "flex-wrap: wrap; 
                            justify-content: center;
                            display: flex;
                            flex-direction: column;"
            
            // Scale
            div{
                attr.``class`` "flex-container"
                attr.``style`` "flex-wrap: wrap;
                                display: flex;
                                justify-content: center;"
                // Scale Label
                div{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` "background:#d3d3d1;"
                    " Scale "
                }
                // Scale Value
                div{
                    attr.``class`` "label"
                    attr.``style`` "background:#d3d3d1;"
                    attr.``type`` "text"
                    $"{model.scl1}" 
                }
                // Slider Scale
                input{
                    attr.``style`` "background:#d3d3d1;"
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.min "1"
                    attr.max "25"
                    bind.input.int model.scl1 (fun a -> dispatch (SetScl1 a))
                }
            }
            // Shape
            div{
                attr.``class`` "flex-container"
                attr.``style`` "flex-wrap: wrap;
                                display: flex;
                                justify-content: center;"
                // Shape Label
                div{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` "background:#d3d3d1;"
                    "Shape"
                }
                // Shape Value
                div{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` "background:#d3d3d1;"
                    $"{shp}" 
                }
                // Slider Shape
                input{
                    attr.``style`` $"background:#d3d3d1;"
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.min "1"
                    attr.max "4"
                    bind.input.int model.shp1 (fun a -> dispatch (SetShp1 a))
                }
            }
            // Sequence
            div{
                attr.``class`` "flex-container"
                attr.``style`` "flex-wrap: wrap;
                                display: flex;
                                justify-content: center;"
                // Sequence Label
                div{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` "background:#d3d3d1;"
                    " Sequence "
                }
                // Sequence Value
                div{
                    attr.``class`` "label"
                    attr.``type`` "text"
                    attr.``style`` "background: #d3d3d1;"
                    $"{sqn}"
                }
                // Slider Sequence
                input{
                    attr.``style`` "background: #d3d3d1;"
                    attr.``class`` "slider"
                    attr.``type`` "range"
                    attr.min "1"
                    attr.max "24"
                    bind.input.int model.sqn1 (fun a -> dispatch (SetSqn1 a))
                }  
            }
        }  

        // Formatting Instructions
        div{
            attr.``class`` "flex-container"
            attr.``style`` "flex-wrap: wrap; 
                            justify-content: right;
                            display: flex;
                            flex-direction: row;
                            width: 90%;
                            margin-left: 20px;
                            margin-right: 20px;"
            a{
                attr.href "https://github.com/vykrum/Hywe/wiki/Hywe-Syntax"
                attr.target "blank"
                img{
                    attr.width "20"
                    attr.height "20"
                    attr.src "https://vykrum.github.io/Hywe/help.png"
                }
            }
        }
        
        // Dropdown
        //div{
        //    label{
        //        attr.``for`` "presets"
        //        "Select a preset"
        //    }
        //    select{
        //        attr.name "presets"
        //        attr.id "presets"
        //        option {"Option A"}
        //        option {"Option B"}
        //        option {"Option C"}
        //    }
        //}

        // Input String
        div{
            attr.``class`` "flex-container"
            attr.``style`` "flex-wrap: wrap; 
                            justify-content: center;
                            display: flex;
                            flex-direction: row;"
            
            textarea {
                attr.``type`` "textarea"
                attr.``class`` "textarea"
                attr.``style`` "width: 95%;
                                margin-left: 20px;
                                margin-right: 20px;
                                height:100px;
                                font-size: 14px;"
                bind.change.string model.spcStr1 (fun a -> dispatch (SetSpcStr1 a))
                "Default"
            }
            button {
                attr.``class`` "button1"
                attr.``style`` "
                                width: 95%;
                                margin-left: 20px;
                                margin-right: 20px;
                                margin-top: 5px;"
                on.click (fun _ -> dispatch (SetSpcStr2))
                "h y W E A V E"
            }
            div{
                attr.``class`` "flex-container"
                attr.``style`` "flex-wrap: wrap; justify-content: center;"
                let cxCxl1 = spaceCxl sqn bsNs bsOc model.spcStr2
                let cxClr1 = pastels (Array.length cxCxl1)
                nstdCxls cxCxl1 cxClr1 scl shp 1200 
            }  
        } 
    }

// Bolero component handling state updates and rendering the user interface
type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view