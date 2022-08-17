module Hyve.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Hexel

type svC = Template<
      """ <polygon 
      points="0,0,-10,10,-10,20,0,30,10,20,10,10" 
      fill="${cl}"
      stroke="${cl}"
      transform="translate${tr}"
      />""">

let cluster (cd: (int*int) list) (cr:string) : Node =
    svg {
         attr.width (400)
         attr.height (400)
         for c in cd do
                svC() 
                    .tr($"{c}")
                    .cl($"{cr}")
                    .Elt()  
    }

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

        let hsHx01 = nui [Host(0,1,0.0),model.cluster1] []
        let hxXY01 = List.map (fun x -> x |> xyz |> vxy) hsHx01.[0]
        let hxShfX = 0 - (hxXY01 |> List.minBy(fun (x,_) -> x) |> fst) + 40
        let hxShfY = 0 - (hxXY01 |> List.minBy(fun (_,x) -> x) |> snd) + 40
        let hxXY02 = List.map (fun (x,y)-> (x + hxShfX), (y + hxShfY)) hxXY01

        concat {
        $"Base : {model.cluster1}"
        div{
            attr.``class`` "slidecontainer"
            input{
                attr.``class`` "slider"
                attr.``type`` "range"
                attr.min "3"
                attr.max "50"
                attr.value "20"
                bind.input.int model.cluster1 (fun v -> dispatch (SetCluster1 v))
            }
        }
        $"Cluster 1 : {model.cluster2}"
        div{
            attr.``class`` "slidecontainer"
            input{
                attr.``class`` "slider"
                attr.``type`` "range"
                attr.min "0"
                attr.max "50"
                attr.value "20"
                bind.input.int model.cluster2 (fun v -> dispatch (SetCluster2 v))
            }
        }

        cluster hxXY02 "rgb(54,54,54)" 
    }

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view
