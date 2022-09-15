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

          
// Scaled Hexel xy
let scl = 10
let vxy (x,y,_) = x * scl, y * scl

let cluster (cd: ((int*int)[] *string)[]) wdt hgt : Node =
    svg {
         attr.width wdt
         attr.height hgt
         for c in cd do
             for xy in (c |> fst) do
                    hxgn() 
                        .tr($"{xy}")
                        .cl($"{c |> snd}")
                        .Elt()
        }

let crd (hst : Hxl[][]) = 
    let hxXY01 = Array.map(fun a -> Array.map (fun x -> x.Loc |> vxy) a) hst
    let hxShfX = 0 - ((Array.concat hxXY01) |> Array.minBy(fun (x,_) -> x) |> fst) + 40
    let hxShfY = 0 - ((Array.concat hxXY01) |> Array.minBy(fun (_,x) -> x) |> snd) + 40
    let hxMxmX = ((Array.concat hxXY01) |> Array.maxBy(fun (x,_) -> x) |> fst) + hxShfX + 40
    let hxMxmY = ((Array.concat hxXY01) |> Array.maxBy(fun (_,x) -> x) |> snd) + hxShfY + 40
    let hxXY02 = Array.map(fun a -> Array.map (fun (x,y)-> (x + hxShfX), (y + hxShfY))a) hxXY01
    (hxXY02,hxMxmX,hxMxmY)

let cls (cnt : int[]) =
    let hsHx01 = nui [|cnt|>Array.head,{Avl=true;Loc=(0,1,0)}|] [||] |> Array.head |> Array.rev
    let hsHx02 = (Array.take ((Array.length cnt) - 1) hsHx01) |> Array.rev
    let hsHx03 = Array.zip (Array.tail cnt) hsHx02
    let hsHx04 = [|[|hsHx01|] ; (Array.map (fun x -> Array.tail x)(nui hsHx03 hsHx01))|] |> Array.concat
    hsHx04 |> crd
