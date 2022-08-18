module Hyve.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Bridge

type Model =
    {
        host : int
        cls1 : int
        cls2 : int
        cls3 : int
    }

let initModel =
    {
        host = 13
        cls1 = 10
        cls2 = 10
        cls3 = 10
    }

type Message =
    | SetHost of int
    | SetCls1 of int
    | SetCls2 of int
    | SetCls3 of int

let update message model =
    match message with
    | SetHost value -> { model with host = value }
    | SetCls1 value -> { model with cls1 = value }
    | SetCls2 value -> { model with cls2 = value }
    | SetCls3 value -> { model with cls3 = value }

let view model dispatch =

        concat {
        div{
            input{
                attr.``class`` "slider"
                attr.``type`` "range"
                attr.id "host"
                attr.min "3"
                attr.max "23"
                bind.input.int model.host (fun a -> dispatch (SetHost a))
            }
            input{
                attr.``class`` "slider"
                attr.``type`` "range"
                attr.id "cls1"
                attr.min "0"
                attr.max "20"
                bind.input.int model.cls1 (fun b -> dispatch (SetCls1 b))
            }
            input{
                attr.``class`` "slider"
                attr.``type`` "range"
                attr.id "cls2"
                attr.min "0"
                attr.max "20"
                bind.input.int model.cls2 (fun c -> dispatch (SetCls2 c))
            }
            input{
                attr.``class`` "slider"
                attr.``type`` "range"
                attr.id "cls3"
                attr.min "0"
                attr.max "20"
                bind.input.int model.cls3 (fun d -> dispatch (SetCls3 d))
            }
        }
       
        cluster (List.zip (cls ([model.host;model.cls1;model.cls2;model.cls3])) (["#bccfd3";"#867869";"#687b7f";"#c6bdb4" ]))
    }

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view
