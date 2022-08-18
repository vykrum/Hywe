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

let cluster (cd: ((int*int) list *string) list) : Node =
    svg {
         attr.width (400)
         attr.height (400)
         for c in cd do
             for xy in (c |> fst) do
                    svC() 
                        .tr($"{xy}")
                        .cl($"{c |> snd}")
                        .Elt()  
    }


let crd (hst : Hxl list list) = 
    let hxXY01 = List.map(fun a -> List.map (fun x -> x |> xyz |> vxy) a) hst
    let hxShfX = 0 - ((List.concat hxXY01) |> List.minBy(fun (x,_) -> x) |> fst) + 40
    let hxShfY = 0 - ((List.concat hxXY01) |> List.minBy(fun (_,x) -> x) |> snd) + 40
    let hxXY02 = List.map(fun a -> List.map (fun (x,y)-> (x + hxShfX), (y + hxShfY))a) hxXY01
    hxXY02

let cls (cnt : int list) =
    let hsHx01 = nui [Host(0,1,0.0),cnt|>List.head] [] |> List.head |> List.rev
    let hsHx02 = List.take ((List.length cnt) - 1) hsHx01
    let hsHx03 = List.zip hsHx02 (List.tail cnt)
    let hsHx04 = hsHx01 :: (List.map (fun x -> List.tail x)(nui hsHx03 hsHx01))
    hsHx04 |> crd
