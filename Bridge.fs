module Bridge

open Bolero
open Bolero.Html
open Hexel

type hxgn = Template<
      """ <polygon 
      points="0,0,-10,10,-10,20,0,30,10,20,10,10" 
      fill="${cl}"
      stroke="${cl}"
      transform="translate${tr}"
      />""">

let cluster (cd: ((int*int) list *string) list) wdt hgt : Node =
    svg {
         "xmlns:xlink" => "http://www.w3.org/1999/xlink"
         "xmlns" => "http://www.w3.org/2000/svg"
         attr.download "hywe.svg"
         attr.width wdt
         attr.height hgt
         for c in cd do
             for xy in (c |> fst) do
                    hxgn() 
                        .tr($"{xy}")
                        .cl($"{c |> snd}")
                        .Elt()  
        }

let crd (hst : Hxl list list) = 
    let hxXY01 = List.map(fun a -> List.map (fun x -> x |> xyz |> vxy) a) hst
    let hxShfX = 0 - ((List.concat hxXY01) |> List.minBy(fun (x,_) -> x) |> fst) + 40
    let hxShfY = 0 - ((List.concat hxXY01) |> List.minBy(fun (_,x) -> x) |> snd) + 40
    let hxMxmX = ((List.concat hxXY01) |> List.maxBy(fun (x,_) -> x) |> fst) + hxShfX + 40
    let hxMxmY = ((List.concat hxXY01) |> List.maxBy(fun (_,x) -> x) |> snd) + hxShfY + 40
    let hxXY02 = List.map(fun a -> List.map (fun (x,y)-> (x + hxShfX), (y + hxShfY))a) hxXY01
    (hxXY02,hxMxmX,hxMxmY)

let cls (cnt : int list) =
    let hsHx01 = nui [Host(0,1,0.0),cnt|>List.head] [] |> List.head |> List.rev
    let hsHx02 = (List.take ((List.length cnt) - 1) hsHx01) |> List.rev
    let hsHx03 = List.zip hsHx02 (List.tail cnt)
    let hsHx04 = hsHx01 :: (List.map (fun x -> List.tail x)(nui hsHx03 hsHx01))
    hsHx04 |> crd
