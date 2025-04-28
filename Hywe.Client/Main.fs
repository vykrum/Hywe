module Hywe.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Coxel
open Shape
open Bridge
open Parse
open Page

type Model =
    {
        shp1 : Shp
        scp1 : int
        opt1 : Beeset option
        stx1 : string
        stx2 : string
    }

// Default Input
let initModel =
    {
        shp1 = Hxg
        scp1 = 8
        opt1 = None
        stx1 = stxInstr 
        stx2 = "(1/3/.)"
    }

type Message =
    | SetShp1 of Shp
    | SetScp1 of int
    | ScpInc
    | ScpDec
    | SetOpt1 of Beeset
    | SetStx1 of string
    | SetStx2

let update message model =
    match message with
    | SetShp1 value -> { model with shp1 = value }
    | SetScp1 value -> { model with scp1 = value }
    | ScpInc -> { model with scp1 = model.scp1 + 1 }
    | ScpDec -> { model with scp1 = model.scp1 - 1 }
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
        // Nested Coxels Data
        let bsOc = [||]
        let cxCxl1 = spaceCxl bsOc model.stx2
        let cxlAvl = cxlExp cxCxl1 (Array.head cxCxl1).Seqn
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
                attr.``class`` "dropdown1"
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
            a {
                attr.``style`` "padding: 17px 5px;"
                attr.``class`` "button3"
                attr.href "https://github.com/vykrum/Hywe/wiki/Hywe-Syntax"
                attr.target "blank"
                "?"
            }
            
            // Font size setting
            let fntSz = match model.opt1 with 
                        | Some Beeline -> 28
                        | Some Beeyond -> 24
                        | Some Beedroom -> 18
                        | _ -> 14
            
            // Hywe Syntax Input
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

            // Zoom Out
            button {
                attr.``class`` "button3"
                attr.``style`` "width: 5%;"
                on.click (fun _ -> dispatch (ScpDec))
                "--"
            }

            // Hyweave
            button {
                attr.``class`` "button1"
                attr.``style`` "
                                width: 88%;
                                margin-left: 1%;
                                margin-right: 1%;
                                margin-top: 5px;"
                on.click (fun _ -> dispatch (SetStx2))
                "h y W E A V E"
            }

            // Zoom In
            button {
                attr.``class`` "button3"
                attr.``style`` "width: 5%;"
                on.click (fun _ -> dispatch (ScpInc))
                "+"
            }

            // Hywe SVG
            div{
                attr.``class`` "flex-container"
                attr.``style`` "flex-wrap: wrap; justify-content: center;"

                //nstdCxls cxCxl1 cxClr1 model.scp1 model.shp1
                nstdCxlsWrp cxCxl1 cxClr1 model.scp1
            }
            
            // Hywe Table
            div{
                attr.``class`` "styleTable"
                table{
                    attr.width "95%"
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