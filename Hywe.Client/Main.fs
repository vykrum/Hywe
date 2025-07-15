module Hywe.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Coxel
open Shape
open Bridge
open Parse
open Page
open Tree

type DerivedData = {
    cxCxl1: Cxl[]
    cxlAvl: int[]
    cxClr1: string[]
}

let deriveData (stx: string) : DerivedData =
    let bsOc = [||]
    let cxCxl1 = spaceCxl bsOc stx
    let cxlAvl = cxlExp cxCxl1 (Array.head cxCxl1).Seqn
    let cxClr1 = pastels (Array.length cxCxl1)
    {
        cxCxl1 = cxCxl1
        cxlAvl = cxlAvl
        cxClr1 = cxClr1
    }

type Model =
    {
        shp1 : Shp
        scp1 : int
        opt1 : Beeset option
        stx1 : string
        stx2 : string
        Tree : SubModel
        Derived : DerivedData
        IsHyweaving: bool
    }

type Message =
    | SetShp1 of Shp
    | SetScp1 of int
    | ScpInc
    | ScpDec
    | SetOpt1 of Beeset
    | SetStx1 of string
    | TreeMsg of SubMsg
    | StartHyweave
    | RunHyweave
    | FinishHyweave

// Default Input
let initModel =
    {
        shp1 = Hxg
        scp1 = 6
        opt1 = None
        stx1 = stxInstr 
        stx2 = stx2Ini
        Tree = Tree.initModel ()
        Derived = deriveData stx2Ini
        IsHyweaving = false
    }

let update message model =
    match message with
    | SetShp1 value -> 
        { model with shp1 = value }, Cmd.none

    | SetScp1 value -> 
        { model with scp1 = value }, Cmd.none

    | ScpInc -> 
        { model with scp1 = model.scp1 + 1 }, Cmd.none

    | ScpDec -> 
        { model with scp1 = model.scp1 - 1 }, Cmd.none

    | SetOpt1 value -> 
        let newStx =
            match value with
            | Beewhich -> stxInstr
            | Beegin -> Tree.getOutput model.Tree
            | Beespoke -> beedroom

        let newModel = {
            model with
                opt1 = Some value
                stx1 = newStx
                stx2 = newStx
        }

        // Reset Hyweave if Bee-which is selected
        let finalModel =
            match value with
            | Beewhich -> { newModel with IsHyweaving = false }
            | _ -> newModel

        finalModel, Cmd.none

    | SetStx1 value -> 
        { model with stx1 = value }, Cmd.none

    | StartHyweave ->
        { model with IsHyweaving = true },
        Cmd.OfAsync.perform
            (fun () -> async {
                do! Async.Sleep 50
                return ()
            }) () (fun _ -> RunHyweave)

    | RunHyweave ->
        let updatedStx1 =
            match model.opt1 with
            | Some Beegin -> Tree.getOutput model.Tree
            | _ -> model.stx1

        let newModel = {
            model with
                stx1 = updatedStx1
                stx2 = updatedStx1
                Derived = deriveData updatedStx1
        }

        newModel,
        Cmd.OfAsync.perform
            (fun () -> async {
                do! Async.Sleep 100
                return ()
            }) () (fun _ -> FinishHyweave)

    | FinishHyweave ->
        { model with IsHyweaving = false }, Cmd.none

    | TreeMsg subMsg ->
        let updatedTree = updateSub subMsg model.Tree
        match model.opt1 with
        | Some Beegin ->
            let newOutput = Tree.getOutput updatedTree
            {
                model with 
                    Tree = updatedTree
                    stx1 = newOutput
                    stx2 = newOutput 
            }, Cmd.none
        | _ ->
            { model with Tree = updatedTree }, Cmd.none

// Interface
let view model dispatch =      
    concat {
        // Nested Coxels Data
        let cxCxl1 = model.Derived.cxCxl1
        let cxlAvl = model.Derived.cxlAvl
        let cxClr1 = model.Derived.cxClr1

        // Hywe
        div{
            attr.``style`` "flex-wrap: wrap;justify-content: center;display: flex;flex-direction: row;"
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
                                        | "Bee-gin" -> Beegin
                                        | "Bee-spoke" -> Beespoke
                                        | _ -> Beewhich

                                    dispatch (SetOpt1 beeset))
                option {
                        attr.selected "true"
                        attr.value "Bee-which"
                        "To  Bee or To Bee . . ."
                }
                option {
                        attr.value "Bee-gin"
                        "Bee-gin : Space Flow Chart"
                        }
                option {
                        attr.value "Bee-spoke"
                        "Bee-spoke : Space Flow Script"
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

            // Space Flow Chart
            div {
                attr.``style`` "width: 100%; flex-basis: 100%; margin-top: 10px;"

                match model.opt1 with
                | Some Beegin ->
                    div {
                        attr.``style`` "width: 95%; margin: auto; padding: 10px;"
                        viewTreeEditor model.Tree (TreeMsg >> dispatch)
                    }
                | _ -> empty()
            }

            // Font size setting
            let fntSz = match model.opt1 with 
                        | Some Beegin -> 12
                        | Some Beespoke -> 12
                        | _ -> 12
            
            // Hywe Syntax Input
            textarea {
                attr.id "syntax"
                attr.``class`` "textarea"
                attr.``style`` $"width : 95%%;
                                margin-left: 20px;
                                margin-right: 20px;
                                height:50px;
                                font-size: {fntSz}px;
                                color: #808080;"

                match model.opt1 with
                | Some Beegin ->
                    attr.readonly true
                    text (Tree.getOutput model.Tree)
                | _ ->
                    bind.change.string model.stx1 (fun a -> dispatch (SetStx1 a))
                    text model.stx1
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
                let hyweaveDisabled =
                    model.IsHyweaving || model.opt1 = None || model.opt1 = Some Beewhich
                attr.``class`` "button1"
                attr.disabled hyweaveDisabled
                attr.``style`` "
                    width: 88%;
                    margin-left: 1%;
                    margin-right: 1%;
                    margin-top: 5px;"
                on.click (fun _ -> dispatch StartHyweave)

                match model.IsHyweaving with
                | true ->
                    span { attr.``class`` "spinner" }
                    text " h y W E A V E i n g . . ."
                | false ->
                    text "h y W E A V E"
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
            viewHyweTable cxCxl1 cxClr1 cxlAvl
        } 
    }

// Bolero component handling state updates and rendering the user interface
type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkProgram
            (fun _ -> initModel, Cmd.none)
            update
            view