module Hyve.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Bridge

type Model =
    {
        cluster1 : int
        cluster2 : int
    }

let initModel =
    {
        cluster1 = 10
        cluster2 = 10
    }

type Message =
    | SetCluster1 of int
    | SetCluster2 of int

let update message model =
    match message with
    | SetCluster1 value -> { model with cluster1 = value }
    | SetCluster2 value -> { model with cluster2 = value }

let view model dispatch =

        concat {
        $"Base : {model.cluster1}"
        div{
            attr.``class`` "slidecontainer"
            input{
                attr.``class`` "slider"
                attr.``type`` "range"
                attr.min "3"
                attr.max "61"
                attr.value "20"
                bind.input.int model.cluster1 (fun v -> dispatch (SetCluster1 v))
            }
        }
        

        cluster (bas model.cluster1) "rgb(54,54,54)" 
    }

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view
