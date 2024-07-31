module Hywe.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Hexel
open Shape
open Bridge
open Parse

type Beeset = 
    | Beeline
    | Beeyond
    | Beedroom
    | Beespoke

let hyweInstructions = 
    "Depending on your selection, the Hywe Syntax will be displayed here. Choose from the following options:\n\n" +
    "• Bee-line: Basic syntax for the simplest layout. Includes index/size/label for straightforward organization.\n" +
    "• Bee-yond: Syntax for a branched layout, where the index format becomes x.x, allowing for slightly more complex structures.\n" +
    "• Bee-droom: Syntax for a typical residential layout with multiple levels of branching, ideal for more detailed layouts.\n" +
    "• Bee-spoke: Custom layout option providing a clean slate for exploring hypothetical layout configurations.\n\n" +
    "Click the hyWEAVE button below to update your selection."

type Model =
    {
        shp1 : int
        sqn1 : int
        scl1 : int
        opt1 : Beeset option
        stx1 : string
        stx2 : string
    }

// Default Input
let initModel =
    {
        shp1 = 4
        sqn1 = 13
        scl1 = 10
        opt1 = None
        stx1 = hyweInstructions
        stx2 = "(1/3/.),(2/3/.)"
    }

type Message =
    | SetShp1 of int
    | SetSqn1 of int
    | SetScl1 of int
    | SetOpt1 of Beeset
    | SetStx1 of string
    | SetStx2

let update message model =
    match message with
    | SetSqn1 value -> { model with sqn1 = value }
    | SetShp1 value -> { model with shp1 = value }
    | SetScl1 value -> { model with scl1 = value }
    | SetOpt1 value -> 
                            let content = 
                                match value with 
                                | Beeline -> "(1/10/Start),(2/10/End)"
                                | Beeyond -> "(1/10/Docking),(1.1/10/Logistics),(1.2/10/Laboratory),(1.3/10/Habitation),(1.4/10/Power)"
                                | Beedroom -> "(1/7/Foyer),(2/12/Living),(3/12/Dining),(1.1/10/Study),(2.1/10/Staircase),(3.1/12/Kitchen),(3.2/14/Bed-1),(3.3/18/Bed-2),(3.4/18/Bed-3),(3.1.1/6/Utility),(3.2.1/8/Bath-1),(3.3.1/10/Closet-2),(3.4.1/10/Closet-3),(3.4.2/10/Bath-3),(3.3.1.1/10/Bath-2)"
                                | Beespoke -> ""
                            {model with opt1 = Some value; stx1 = content}

    | SetStx1 value -> { model with stx1 = value }
    | SetStx2 -> { model with stx2 = model.stx1}

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
                    "Weave spatial layouts at a high level of abstraction using properly formatted hywe syntax."
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

        // Hywe
        div{
            attr.``style`` "flex-wrap: wrap;
                                justify-content: center;
                                display: flex;
                                flex-direction: row;"
            // Dropdown
            label{
                attr.``for`` "options"   
            }
            select{
                attr.name "options"
                attr.``style`` "width: 80%;
                                margin-left: 20px;
                                margin-right: 20px;
                                margin-top: 10px;
                                margin-bottom: 10px;
                                height:25px;
                                font-size: 14px;
                                font-family: 'Optima', Candara, Calibri;"
                attr.id "options"
                on.change (fun e -> 
                                    let value = (e.Value :?> string)
                                    let beeset = 
                                        match value with
                                        | "Bee-line" -> Beeline
                                        | "Bee-yond" -> Beeyond
                                        | "Bee-droom" -> Beedroom
                                        | _ -> Beespoke

                                    dispatch (SetOpt1 beeset))
                option {
                        attr.selected "true"
                        attr.value ""
                        "Bee-which •"
                }
                option {
                        attr.value "Bee-line"
                        "Bee-line"
                        }
                option {
                        attr.value "Bee-yond"
                        "Bee-yond"
                        }
                option {
                        attr.value "Bee-droom"
                        "Bee-droom"
                        }
                option {
                        attr.value "Bee-spoke"
                        "Bee-spoke"
                        }
            }
            // Formatting Instructions
            a{
                attr.href "https://github.com/vykrum/Hywe/wiki/Hywe-Syntax"
                attr.target "blank"
                img{
                    attr.width "20"
                    attr.height "20"
                    attr.``style`` "margin-right: 20px;
                                    margin-top: 15px"
                    attr.src "https://vykrum.github.io/Hywe/help.png"
                }
            }
            // Hywe Syntax Input
            textarea {
                attr.name "options"
                attr.id "options"
                attr.``type`` "textarea"
                attr.``class`` "textarea"
                attr.``style`` "width: 95%;
                                margin-left: 20px;
                                margin-right: 20px;
                                height:100px;
                                font-size: 14px;"
                bind.change.string model.stx1 (fun a -> dispatch (SetStx1 a))
                
            }
            // Hyweave
            button {
                attr.``class`` "button1"
                attr.``style`` "
                                width: 95%;
                                margin-left: 20px;
                                margin-right: 20px;
                                margin-top: 5px;"
                on.click (fun _ -> dispatch (SetStx2))
                "h y W E A V E"
            }
            // Hywe SVG
            div{
                attr.``class`` "flex-container"
                attr.``style`` "flex-wrap: wrap; justify-content: center;"
                let cxCxl1 = spaceCxl sqn bsNs bsOc model.stx2
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