module Hywe.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Hexel
open Coxel
open Shape
open Bridge
open Parse
open Page

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
        sqn1 = HRCWNE
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
                                | Beeline -> beeline
                                | Beeyond -> beeyond
                                | Beedroom -> beedroom
                            {model with opt1 = Some value; stx1 = content}

    | SetStx1 value -> { model with stx1 = value }
    | SetStx2 -> { model with stx2 = model.stx1}

// Interface
let view model dispatch =      
    concat {
        // Header
        div{
            attr.``style`` styleHeader
            // Name and Logo
            div{
                attr.``class`` "flex-container"
                attr.``style`` styleLogo
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
                    attr.``style`` styleTitle
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
                attr.``style`` styleIntro
                p{
                    introduction
                }    
            }
        }

        // Nested Coxels Data
        //let bsNs = hxlVld model.sqn1 (AV(1,4,0))
        //let bsOc = hxlBnd model.sqn1 bsNs
        let bsOc = [||]
        let cxCxl1 = spaceCxl model.scl1 model.sqn1 bsOc  model.stx2
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
                attr.``style`` styleDrop1
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
            let fntSz = match model.opt1 with 
                        | Some Beeline -> 28
                        | Some Beeyond -> 24
                        | Some Beedroom -> 18
                        | _ -> 14
            textarea {
                attr.name "options"
                attr.id "options"
                attr.``type`` "textarea"
                attr.``class`` "textarea"
                attr.``style`` $"width : 95%%;
                                margin-left: 20px;
                                margin-right: 20px;
                                height:100px;
                                font-size: {fntSz}px;
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
                    attr.``style`` styleDrop2
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
                    attr.``style`` styleDrop2
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
                    attr.``style`` styleDrop2
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
                    attr.``style`` styleDrop2
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
                    attr.``style`` styleTable
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
                                    match (prpVlu fs.Rfid) with
                                    | "1" -> ((prpVlu fs.Size |> int) + 1).ToString()
                                    | _ -> prpVlu fs.Size

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
                                    prpVlu fs.Rfid
                                    }
                                td{
                                    attr.width "35%"
                                    attr.``style`` "
                                        padding: 5px 10px;
                                        text-align: center;"
                                    prpVlu fs.Name
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