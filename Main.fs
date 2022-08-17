module Hyve.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Hexel

type svC = Template<
      """ <polygon 
      points="0,0,-10,10,-10,20,0,30,10,20,10,10" 
      fill="lime"
      stroke="lime"
      transform="translate${tr}"
      />""">

let cluster (cd: (int*int) list) : Node =
    svg {
         attr.width (400)
         attr.height (400)
         for c in cd do
                svC() 
                    .tr($"{c}")
                    .Elt()  
    }

type Model =
    {
        cluster1 : int
    }

let initModel =
    {
        cluster1 = 20
    }

type Message =
    | SetCluster1 of int

let update message model =
    match message with
    | SetCluster1 value -> { model with cluster1 = value }

let view model dispatch =

        let hsHx01 = nui [Host(0,1,0.0),model.cluster1] []
        let hxXY01 = List.map (fun x -> x |> xyz |> vxy) hsHx01.[0]
        let hxShfX = 0 - (hxXY01 |> List.minBy(fun (x,_) -> x) |> fst) + 40
        let hxShfY = 0 - (hxXY01 |> List.minBy(fun (_,x) -> x) |> snd) + 40
        let hxXY02 = List.map (fun (x,y)-> (x + hxShfX), (y + hxShfY)) hxXY01

        concat {
        p {
            "Base:"   
            input {
                attr.``type`` "number"
                attr.id "counter"
                attr.``class`` "input"
                bind.input.int model.cluster1 (fun v -> dispatch (SetCluster1 v))
            }
            
        }

        cluster hxXY02   
    }

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view
