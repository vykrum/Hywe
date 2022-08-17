module Bridge

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

let bas (cnt : int) = 
    let hsHx01 = nui [Host(0,1,0.0),cnt] []
    let hxXY01 = List.map (fun x -> x |> xyz |> vxy) hsHx01.[0]
    let hxShfX = 0 - (hxXY01 |> List.minBy(fun (x,_) -> x) |> fst) + 40
    let hxShfY = 0 - (hxXY01 |> List.minBy(fun (_,x) -> x) |> snd) + 40
    let hxXY02 = List.map (fun (x,y)-> (x + hxShfX), (y + hxShfY)) hxXY01
    hxXY02