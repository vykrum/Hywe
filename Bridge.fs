module Bridge

open Bolero
open Bolero.Html
open Location

type hxgn = Template<
      """ <polygon 
      points="0,0,-10,10,-10,20,0,30,10,20,10,10" 
      fill="${cl}"
      stroke="${cl}"
      transform="translate${tr}"
      opacity = "0.75"
      stroke-opacity="0.125"
      >""">

          
type svtx = Template<
    """<text 
    x="${xx}" 
    y="${yy}"
    font-size = "10px"
    text-align = "center"
    fill = "#808080"
    opacity = "1"
    >${nm}</text> """>
          
// Scaled Location xy
let scl = 10

// Location to Coordinates
let cdn (loc:Loc[]) (scl:int) =
    Array.map (fun (XY(x,y)) -> ((x*scl),(y*scl))) loc

let cluster (cd: ((int*int)[] * string * string)[]) wdt hgt : Node =
    svg {
         attr.width wdt
         attr.height hgt
         for c in cd do
             let (lc,n,cr) = c
             let x1 = lc |> Array.tryItem ((Array.length lc)/3)
             let x,y = match x1 with 
                         | None -> -10,-10
                         | Some a -> a
             for xy in lc do
                    hxgn() 
                        .tr($"{xy}")
                        .cl($"{cr}")
                        .Elt()
             svtx()
                   .xx($"{x}")
                   .yy($"{y}")
                   .nm($"{n}")
                   .Elt()
        }

let crd (hst : Loc[][]) = 
    let hxXY01 = Array.map (fun x -> cdn x scl) hst
    let hxShfX = 0 - ((Array.concat hxXY01) |> Array.minBy(fun (x,_) -> x) |> fst) + 40
    let hxShfY = 0 - ((Array.concat hxXY01) |> Array.minBy(fun (_,x) -> x) |> snd) + 40
    let hxMxmX = ((Array.concat hxXY01) |> Array.maxBy(fun (x,_) -> x) |> fst) + hxShfX + 40
    let hxMxmY = ((Array.concat hxXY01) |> Array.maxBy(fun (_,x) -> x) |> snd) + hxShfY + 40
    let hxXY02 = Array.map(fun a -> Array.map (fun (x,y)-> (x + hxShfX), (y + hxShfY))a) hxXY01
    (hxXY02,hxMxmX,hxMxmY)

let cls (cnt : int[]) =
    let hsHx01 = 
        (Location.clusters SECW [|identity,cnt|>Array.head|] [||]).Locs
        |> Array.head 
        |> Array.rev
    let hsHx02 = 
        (Array.take ((Array.length cnt) - 1) hsHx01) 
        |> Array.rev
    let hsHx03 = 
        Array.zip hsHx02 (Array.tail cnt) 
    let hsHx04 = 
        [|[|hsHx01|] ; (Array.map (fun x -> Array.tail x)((Location.clusters SECW hsHx03 hsHx01).Locs))|] 
        |> Array.concat
    hsHx04 |> crd
