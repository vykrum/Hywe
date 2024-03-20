module Bridge

open Bolero
open Bolero.Html
open Hexel
open Coxel

/// <summary> Module shape in tessalated hexagonal grid. </summary>
/// <typeparam name="HxFl"> Hexagon Flat-Top. </typeparam>
/// <typeparam name="HxPt"> Hexagon Pointy-Top. </typeparam>
/// <typeparam name="QdSq"> Square. </typeparam>
/// <typeparam name="PrFl"> Parallelogram Flat. </typeparam>
/// <typeparam name="PrAn"> Parallelogram Angled. </typeparam>
/// <typeparam name="RhHr"> Rhombus Horizontal. </typeparam>
/// <typeparam name="RhVr"> Rhombus Vertical. </typeparam>
type Shp = 
    | HxFl | HxPt | QdSq | PrFl | PrAn | RhHr | RhVr

/// <summary> Vertices based on shape </summary>
/// <param name="shp"></param>
/// <param name="scl"></param>
let vertices 
    (shp : Shp)
    (scl : int) = 
    let vrtx = match shp with 
                | HxFl -> [|0;0;1;1;2;1;3;0;2;-1;1;-1|]
                | HxPt -> [|0;0;1;1;2;0;2;-1;1;-2;0;-1|]
                | QdSq -> [|0;0;2;0;2;-2;0;-2|]
                | PrAn -> [|0;0;2;1;2;-1;0;-2|]
                | PrFl -> [|0;0;2;0;1;-2;-1;-2|]
                | RhHr -> [|0;0;2;1;4;0;2;-1|]
                | RhVr -> [|0;0;1;2;2;0;1;-2|]
                          
    vrtx 
        |> Array.map (fun x -> string (x * scl)) 
        |> String.concat ","

type hxgn = Template<
      """ <polygon 
      points="${pt}" 
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

/// <summary> Coxel SVG composition </summary>
/// <param name="prp"> Coxel Array of Hexel coordinates (array) * Name * Color </param>
/// <param name="shp"> Shape </param>
/// <param name="scl"> Scale </param>
/// <param name="wdt"> Width </param>
/// <param name="hgt"> Height </param>
/// <returns> SVG of Coxel composition </returns>
let cluster (prp: ((int*int*int)[] * string * string)[]) shp scl wdt hgt : Node =
    svg {
         attr.width wdt
         attr.height hgt
         for cmp in prp do
             let (xyz,label,color) = cmp
             let xy = Array.map(fun (a,b,_) -> a,b) xyz 
             let xx = xy |> Array.tryItem ((Array.length xy)/2)
             let x,y = match xx with 
                         | None -> -10,-10
                         | Some a -> a
             for locn in xy do
                    hxgn()
                        .pt($"{vertices shp scl}")
                        .tr($"{locn}")
                        .cl($"{color}")
                        .Elt()
             svtx()
                   .xx($"{x}")
                   .yy($"{y}")
                   .nm($"{label}")
                   .Elt()
        }

let crd (scl : int) (hxo : Hxl[][]) = 
    
    // Location to Coordinates
    let cdn (hxo:Hxl[]) (scl:int) =
        Array.map (fun a -> match a with 
                            |AV(x,y,z) -> ((x*scl),(y*scl),z)
                            |RV(x,y,z) -> ((x*scl),(y*scl),z)) hxo

    let hxXY01 = Array.map (fun x -> cdn x scl) hxo
    let (a,_,_) = (Array.concat hxXY01) |> Array.minBy(fun (x,_,_) -> x)
    let hxShfX = 0 - a + (4*scl)
    let (_,b,_) = (Array.concat hxXY01) |> Array.minBy(fun (_,x,_) -> x)
    let hxShfY = 0 - b + (4*scl)
    let (c,_,_) = (Array.concat hxXY01) |> Array.maxBy(fun (x,_,_) -> x)
    let hxMxmX = c + hxShfX + (4*scl)
    let (_,d,_) = (Array.concat hxXY01) |> Array.maxBy(fun (_,x,_) -> x)
    let hxMxmY = d + hxShfY + (4*scl)
    let hxXY02 = Array.map(fun aa -> Array.map (fun (x,y,z)-> (x + hxShfX), (y + hxShfY),z)aa) hxXY01
    (hxXY02,hxMxmX,hxMxmY)

let cls 
    (scl : int)
    (shp : Shp)
    (cnt : int[]) =
    // Shape matched with appropriate sequence
    let sqn = match shp with
                | HxFl | RhHr | PrAn -> SQ23
                | HxPt | QdSq | RhVr | PrFl -> SQ21
    // Host Cluster
    let hsHx01 = 
        (Coxel.coxel sqn [|identity,Refid "",Count (cnt|>Array.head),Label "Host"|] [||]
        |> Array.head).Hxls
        |> Array.rev
    // Base Hexels
    let hsHx02 = 
        (Array.take ((Array.length cnt) - 1) hsHx01) 
        |> Array.rev
    // Size * Base
    let hsHx03 = 
        let a = Array.zip 
                    hsHx02 
                    [|Refid"";Refid"";Refid"";Refid"";Refid"";Refid""|]
        let b = Array.zip 
                    (Array.map(fun x -> Count x)(Array.tail cnt)) 
                    [|Label"";Label"";Label"";Label"";Label"";Label""|]
        Array.map2 (fun x y -> (fst x, snd x, fst y, snd y)) a b      
    let hsHx04 = 
        [|[|hsHx01|] ; Array.map (fun x -> Array.tail (x.Hxls))(Coxel.coxel sqn hsHx03 hsHx01)|] 
        |> Array.concat  
    // Scaled Coordinates
    hsHx04 |> crd scl