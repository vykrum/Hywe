module Bridge

open Bolero
open Bolero.Html
open Hexel
open Coxel

type hxg1 = Template<
      """ <polygon 
      points="0,0,-10,10,-10,20,0,30,10,20,10,10" 
      fill="${cl}"
      stroke="${cl}"
      transform="translate${tr}"
      opacity = "0.75"
      stroke-opacity="0.125"
      >""">

type hxgn = Template<
      """ <polygon 
      points="0,0,10,10,20,10,30,0,20,-10,10,-10" 
      fill="${cl}"
      stroke="${cl}"
      transform="translate${tr}"
      opacity = "0.75"
      stroke-opacity="0.175"
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
          
// Hexel Scale
let scl = 10

let cluster (cd: ((int*int*int)[] * string * string)[]) wdt hgt : Node =
    svg {
         attr.width wdt
         attr.height hgt
         for c in cd do
             let (lc1,n,cr) = c
             let lc = Array.map(fun (a,b,_) -> a,b) lc1 
             let x1 = lc |> Array.tryItem ((Array.length lc)/2)
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

let crd (scl : int) (hxo : Hxl[][]) = 
    
    // Location to Coordinates
    let cdn (hxo:Hxl[]) (scl:int) =
        Array.map (fun a -> match a with 
                            |OG(x,y,z) -> ((x*scl),(y*scl),z)
                            |OP(x,y,z) -> ((x*scl),(y*scl),z)) hxo

    let hxXY01 = Array.map (fun x -> cdn x scl) hxo
    let (a,_,_) = (Array.concat hxXY01) |> Array.minBy(fun (x,_,_) -> x)
    let hxShfX = 0 - a + 40
    let (_,b,_) = (Array.concat hxXY01) |> Array.minBy(fun (_,x,_) -> x)
    let hxShfY = 0 - b + 40
    let (c,_,_) = (Array.concat hxXY01) |> Array.maxBy(fun (x,_,_) -> x)
    let hxMxmX = c + hxShfX + 40
    let (_,d,_) = (Array.concat hxXY01) |> Array.maxBy(fun (_,x,_) -> x)
    let hxMxmY = d + hxShfY + 40
    let hxXY02 = Array.map(fun aa -> Array.map (fun (x,y,z)-> (x + hxShfX), (y + hxShfY),z)aa) hxXY01
    (hxXY02,hxMxmX,hxMxmY)

let cls (cnt : int[]) =
    let sqn = HCNN
    // Host Cluster
    let hsHx01 = 
        (Coxel.coxel sqn [|identity,"",cnt|>Array.head,"Host"|] [||]
        |> Array.head).Hxls
        |> Array.rev
    // Base Hexels
    let hsHx02 = 
        (Array.take ((Array.length cnt) - 1) hsHx01) 
        |> Array.rev
    // Size * Base
    let hsHx03 = 
        let a = Array.zip hsHx02 [|"";"";"";"";"";""|]
        let b = Array.zip (Array.tail cnt) [|"";"";"";"";"";""|]
        Array.map2 (fun x y -> (fst x, snd x, fst y, snd y)) a b      
    let hsHx04 = 
        [|[|hsHx01|] ; Array.map (fun x -> Array.tail (x.Hxls))(Coxel.coxel sqn hsHx03 hsHx01)|] 
        |> Array.concat  
    // Scaled Coordinates
    hsHx04 |> crd scl


