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
        cls4 : int
        cls5 : int
        cls6 : int
    }

let initModel =
    {
        host = 7
        cls1 = 12
        cls2 = 8
        cls3 = 16
        cls4 = 4
        cls5 = 14
        cls6 = 6
    }

type Message =
    | SetHost of int
    | SetCls1 of int
    | SetCls2 of int
    | SetCls3 of int
    | SetCls4 of int
    | SetCls5 of int
    | SetCls6 of int

let update message model =
    match message with
    | SetHost value -> { model with host = value }
    | SetCls1 value -> { model with cls1 = value }
    | SetCls2 value -> { model with cls2 = value }
    | SetCls3 value -> { model with cls3 = value }
    | SetCls4 value -> { model with cls4 = value }
    | SetCls5 value -> { model with cls5 = value }
    | SetCls6 value -> { model with cls6 = value }

let view model dispatch =
        concat {
        div{
            p{
                "An experimental space layout planning concept being developed as a plain WebAssembly application that runs dynamic content on the client side directly in the user's browser."
            }   
            
            p{
                "In the current state, seven sliders control the magnitude of clusters of a corresponding color. The first cluster serves as the host to the other six"
            }

            input{
                attr.``class`` "slider"
                attr.``type`` "range"
                attr.id "host"
                attr.min "7"
                attr.max "18"
                bind.input.int model.host (fun a -> dispatch (SetHost a))
            }
            input{
                attr.``class`` "slider"
                attr.``type`` "range"
                attr.id "cls1"
                attr.min "0"
                attr.max "18"
                bind.input.int model.cls1 (fun b -> dispatch (SetCls1 b))
            }
            input{
                attr.``class`` "slider"
                attr.``type`` "range"
                attr.id "cls2"
                attr.min "0"
                attr.max "18"
                bind.input.int model.cls2 (fun c -> dispatch (SetCls2 c))
            }
            input{
                attr.``class`` "slider"
                attr.``type`` "range"
                attr.id "cls3"
                attr.min "0"
                attr.max "18"
                bind.input.int model.cls3 (fun d -> dispatch (SetCls3 d))
            }
            input{
                attr.``class`` "slider"
                attr.``type`` "range"
                attr.id "cls4"
                attr.min "0"
                attr.max "18"
                bind.input.int model.cls4 (fun e -> dispatch (SetCls4 e))
            }
            input{
                attr.``class`` "slider"
                attr.``type`` "range"
                attr.id "cls5"
                attr.min "0"
                attr.max "17"
                bind.input.int model.cls5 (fun f -> dispatch (SetCls5 f))
            }
            input{
                attr.``class`` "slider"
                attr.``type`` "range"
                attr.id "cls6"
                attr.min "0"
                attr.max "18"
                bind.input.int model.cls6 (fun g -> dispatch (SetCls6 g))
            }
        }
        
        cluster (List.zip (cls ([model.host;model.cls1;model.cls2;model.cls3;model.cls4;model.cls5;model.cls6])) (["#363636";"#bccfd3";"#867869";"#687b7f";"#c6bdb4";"#3496a3";"#eabdb5"]))

    }

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view
