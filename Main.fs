module Hywe.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Hexel
open Coxel
open Shape
open Bridge
open Parse

type Beeset = 
    | Beewhich
    | Beeline
    | Beeyond
    | Beedroom

let stxInstr = 
    "Depending on your selection above, Hywe Syntax (index/size/label) will populate here.\n" +
    "Use the populated examples as reference to script your custom layouts.\n" +
    "Click on hyWEAVE to update any selection or alteration.\n\n"+
    "• BEE-line  : Hywe syntax in its simplest form.\n" +
    "• BEE-yond  : Syntax for slightly complex layouts.\n" +
    "• BEE-droom : Syntax with nested branching, for more detailed layouts.\n"
   
type Model =
    {
        shp1 : Shp
        sqn1 : Sqn
        scp1 : int
        scl1 : int
        opt1 : Beeset option
        stx1 : string
        stx2 : string
    }

// Default Input
let initModel =
    {
        shp1 = Hxg
        sqn1 = HRCCNN
        scp1 = 10
        scl1 = 1
        opt1 = None
        stx1 = stxInstr 
        stx2 = "(1/3/.)"
    }

type Message =
    | SetShp1 of Shp
    | SetSqn1 of Sqn
    | SetScp1 of int
    | SetScl1 of int
    | SetOpt1 of Beeset
    | SetStx1 of string
    | SetStx2

let update message model =
    match message with
    | SetSqn1 value -> { model with sqn1 = value }
    | SetShp1 value -> { model with shp1 = value }
    | SetScp1 value -> { model with scp1 = value }
    | SetScl1 value -> { model with scl1 = value }
    | SetOpt1 value -> 
                            let content = 
                                match value with 
                                | Beewhich -> stxInstr
                                | Beeline -> "(1/48/Start),(2/52/End)"
                                | Beeyond -> "(1/25/Dock),(1.1/25/Logistics),(1.2/25/Lab),"+
                                             "(1.3/25/Habitation),(1.4/25/Power)"
                                | Beedroom -> "(1/7/Foyer),(2/12/Living),(3/18/Dining),(1.1/12/Study),"+
                                              "(2.1/12/Staircase),(3.1/14/Kitchen),(3.2/14/Bed-1),"+
                                              "(3.3/18/Bed-2),(3.4/18/Bed-3),(3.1.1/6/Utility),"+
                                              "(3.2.1/8/Bath-1),(3.3.1/10/Closet-2),(3.4.1/10/Closet-3),"+
                                              "(3.4.2/10/Bath-3),(3.3.1.1/10/Bath-2)"
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
                    "Weave spatial layouts at a high level of abstraction using properly " +
                    "formatted syntax in Hywe, an endogenous space planning concept " +
                    "currently undergoing its formative stages of development " + 
                    "as an early stage design interface."
                }    
            }
        }

        // Nested Coxels Data
        let bsNs = hxlVld model.sqn1 (AV(1,4,0))
        let bsOc = 
            match model.sqn1 with 
            | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE 
                -> let a,b,c = hxlCrd bsNs
                   Array.append (hxlOrt model.sqn1 (hxlVld model.sqn1 (AV(a-100,b-2,c))) 200 false) (adjacent model.sqn1 (hxlVld model.sqn1 (AV(0,0,0))))
                   |> allAV true
            | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW 
                -> let a,b,c = hxlCrd bsNs
                   Array.append (hxlOrt model.sqn1 (hxlVld model.sqn1 (AV(a-104,b-2,c))) 200 false) (adjacent model.sqn1 (hxlVld model.sqn1 (AV(0,0,0))))
                   |> allAV true
        let cxCxl1 = spaceCxl model.scl1 model.sqn1 bsNs bsOc model.stx2
        let cxlAvl = cxlExp cxCxl1 model.sqn1
        let cxClr1 = pastels (Array.length cxCxl1)

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
                attr.``style`` "width: 75%;
                                display: block;
                                margin-left: 20px;
                                margin-right: 20px;
                                margin-top: 10px;
                                margin-bottom: 20px;
                                height: 36px;
                                font-size: 14px;
                                border: none;
                                padding: 10px 10px;
                                color: #646464;
                                border-radius: 10px;
                                text-align: center;
                                background-color: #ececec;
                                box-shadow: 0px 8px 16px 0px rgba(0,0,0,0.2);
                                font-family: 'Optima', Candara, Calibri"
                attr.id "options"
                on.change (fun e -> 
                                    let value = (e.Value :?> string)
                                    let beeset = 
                                        match value with
                                        | "Bee-line" -> Beeline
                                        | "Bee-yond" -> Beeyond
                                        | "Bee-droom" -> Beedroom
                                        | _ -> Beewhich

                                    dispatch (SetOpt1 beeset))
                option {
                        attr.selected "true"
                        attr.value "Bee-which"
                        "SELECT AN OPTION TO BEGIN HYWING"
                }
                option {
                        attr.value "Bee-line"
                        "BEE-line"
                        }
                option {
                        attr.value "Bee-yond"
                        "BEE-yond"
                        }
                option {
                        attr.value "Bee-droom"
                        "BEE-droom"
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
                                font-size: 14px;
                                color: #808080;"
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

                nstdCxls cxCxl1 cxClr1 model.scp1 model.shp1
            }
            
            // Universal Controls
            div{
                attr.width "95%"
                attr.``class`` "flex-container"
                attr.``style`` "flex-wrap: wrap; 
                                justify-content: center;
                                display: flex;
                                flex-direction: row;"
                
                // Shape
                label{
                    attr.``for`` "selectShape"  
                }
                select{
                    attr.name "selectShape"
                    attr.``style`` "width: 20%;
                                    display: block;
                                    height: 26px;
                                    font-size: 12px;
                                    background:#f9f9f9;
                                    margin-left: 10px;
                                    margin-right: 10px;
                                    color: #808080;
                                    padding: 5px 10px;
                                    border-radius: 5px;
                                    border: none;
                                    text-align: center;
                                    font-family: 'Optima', Candara, Calibri;"
                    attr.id "shapeOptions"
                    on.change (fun e -> 
                                        let value = (e.Value :?> string)
                                        let shp01 = 
                                            match value with
                                            | "Hexagon" -> Hxg
                                            | "Square" -> Sqr
                                            | "Arrow" -> Arw
                                            | "Rhombus" -> Prl
                                            | _ -> Hxg

                                        dispatch (SetShp1 shp01))
                    option {"Shape"}
                    option {"Hexagon"}
                    option {"Square"}
                    option {"Arrow"}
                    option {"Rhombus"}
                }
                
                // Sequence
                label{
                    attr.``for`` "selectSequence"  
                }
                select{
                    attr.name "selectSequence"
                    attr.``style`` "width: 20%;
                                    display: block;
                                    height: 26px;
                                    font-size: 12px;
                                    background:#f9f9f9;
                                    margin-left: 10px;
                                    margin-right: 10px;
                                    color: #808080;
                                    padding: 5px 10px;
                                    border-radius: 5px;
                                    border: none;
                                    text-align: center;
                                    font-family: 'Optima', Candara, Calibri;"
                    attr.id "sequenceOptions"
                    on.change (fun e -> 
                                        let value = (e.Value :?> string)
                                        let sqn01 = 
                                            match value with
                                            |"VRCWEE" -> VRCWEE 
                                            |"VRCCEE" -> VRCCEE    
                                            |"VRCWSE" -> VRCWSE
                                            |"VRCCSE" -> VRCCSE
                                            |"VRCWSW" -> VRCWSW
                                            |"VRCCSW" -> VRCCSW
                                            |"VRCWWW" -> VRCWWW
                                            |"VRCCWW" -> VRCCWW
                                            |"VRCWNW" -> VRCWNW
                                            |"VRCCNW" -> VRCCNW
                                            |"VRCWNE" -> VRCWNE
                                            |"VRCCNE" -> VRCCNE
                                            |"HRCWNN" -> HRCWNN
                                            |"HRCCNN" -> HRCCNN
                                            |"HRCWNE" -> HRCWNE
                                            |"HRCCNE" -> HRCCNE
                                            |"HRCWSE" -> HRCWSE
                                            |"HRCCSE" -> HRCCSE
                                            |"HRCWSS" -> HRCWSS
                                            |"HRCCSS" -> HRCCSS
                                            |"HRCWSW" -> HRCWSW
                                            |"HRCCSW" -> HRCCSW
                                            |"HRCWNW" -> HRCWNW
                                            |"HRCCNW" -> HRCCNW
                                            | _ -> VRCWEE

                                        dispatch (SetSqn1 sqn01))
                    option {"Sequence"}
                    option {"VRCWEE"}
                    option {"VRCCEE"}
                    option {"VRCWSE"}
                    option {"VRCCSE"}
                    option {"VRCWSW"}
                    option {"VRCCSW"}
                    option {"VRCWWW"}
                    option {"VRCCWW"}
                    option {"VRCWNW"}
                    option {"VRCCNW"}
                    option {"VRCWNE"}
                    option {"VRCCNE"}
                    option {"HRCWNN"}
                    option {"HRCCNN"}
                    option {"HRCWNE"}
                    option {"HRCCNE"}
                    option {"HRCWSE"}
                    option {"HRCCSE"}
                    option {"HRCWSS"}
                    option {"HRCCSS"}
                    option {"HRCWSW"}
                    option {"HRCCSW"}
                    option {"HRCWNW"}
                    option {"HRCCNW"}
                }  
                                
                // Scale
                label{
                    attr.``for`` "selectResolution"  
                }
                select{
                    attr.name "selectResolution"
                    attr.``style`` "width: 20%;
                                    display: flex;
                                    height: 26px;
                                    font-size: 12px;
                                    background:#f9f9f9;
                                    margin-left: 10px;
                                    margin-right: 10px;
                                    padding: 5px 10px;
                                    border-radius: 5px;
                                    border: none;
                                    text-align: center;
                                    color: #808080;
                                    font-family: 'Optima', Candara, Calibri;"
                    attr.id "scaleOptions"
                    on.change (fun e -> 
                                        let value = (e.Value :?> string)
                                        let scl01 = 
                                            match value with
                                            | "1x" -> 1
                                            | "2x" -> 2
                                            | "3x" -> 3
                                            | "4x" -> 4
                                            | _ -> 1

                                        dispatch (SetScl1 scl01))
                    option {"Scale"}
                    option {"1x"}
                    option {"2x"}
                    option {"3x"}
                    option {"4x"}
                }

                // Scope
                label{
                    attr.``for`` "selectScope"  
                }
                select{
                    attr.name "selectScope"
                    attr.``style`` "width: 20%;
                                    display: flex;
                                    height: 26px;
                                    font-size: 12px;
                                    background:#f9f9f9;
                                    margin-left: 10px;
                                    margin-right: 10px;
                                    padding: 5px 10px;
                                    border-radius: 5px;
                                    border: none;
                                    text-align: center;
                                    color: #808080;
                                    font-family: 'Optima', Candara, Calibri;"
                    attr.id "scopeOptions"
                    on.change (fun e -> 
                                        let value = (e.Value :?> string)
                                        let scp01 = 
                                            match value with
                                            | "1x" -> 1
                                            | "5x" -> 5
                                            | "10x" -> 10
                                            | "15x" -> 15
                                            | _ -> 10

                                        dispatch (SetScp1 scp01))
                    option {"Scope"}
                    option {"1x"}
                    option {"5x"}
                    option {"10x"}
                    option {"15x"}
                }

            }  
            
            // Hywe Table
            div{
                attr.``class`` "flex-container"
                attr.``style`` "flex-wrap: wrap; justify-content: center;"
                table{
                    attr.width "95%"
                    attr.``style`` "
                                font-size: 14px;
                                opacity:75%;
                                border: none;
                                padding: 10px 10px;
                                color: #646464;
                                text-align: center;
                                font-family: 'Optima', Candara, Calibri"
                    thead{
                            tr{
                                th{"Index"}
                                th{"Label"}
                                th{"Required"}
                                th{"Achieved"}
                                th{"Open"}
                            }
                    }
                    tbody{
                        for cxl in (Array.zip3 cxCxl1 cxClr1 cxlAvl) do
                            tr{
                                let (fs,sn,th) = cxl
                                attr.``style`` $"background-color:{sn}"
                                
                                let reqSz = 
                                    match (Coxel.prpVlu fs.Rfid) with
                                    | "1" -> ((Coxel.prpVlu fs.Size |> int) + 1).ToString()
                                    | _ -> Coxel.prpVlu fs.Size

                                let achSz = Array.length fs.Hxls

                                let achCl =
                                    match achSz < (reqSz|>int) with
                                    | true -> "red"
                                    | false -> "#646464"

                                let achVtCl =                                     
                                        match th < 1 with
                                        | true -> "red"
                                        | false -> "#646464"

                                td{
                                    attr.width "15%"
                                    attr.``style`` "
                                        padding: 5px 10px;
                                        text-align: center;"
                                    Coxel.prpVlu fs.Rfid
                                    }
                                td{
                                    attr.width "35%"
                                    attr.``style`` "
                                        padding: 5px 10px;
                                        text-align: center;"
                                    Coxel.prpVlu fs.Name
                                    }
                                td{
                                    attr.width "15%"
                                    attr.``style`` "
                                        padding: 5px 10px;
                                        text-align: center;"
                                    reqSz
                                    }
                                td{
                                attr.width "15%"
                                attr.``style`` $"
                                    padding: 5px 10px;
                                    text-align: center;
                                    color:{achCl};"
                                $"{achSz}"
                                }
                                td{
                                attr.width "15%"
                                attr.``style`` $"
                                    padding: 5px 10px;
                                    text-align: center;
                                    color:{achVtCl};"
                                $"{th}"
                                }
                            }
                    }
                }
            }
        } 

    }

// Bolero component handling state updates and rendering the user interface
type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view