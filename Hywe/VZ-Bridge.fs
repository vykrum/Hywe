module Bridge

open Bolero
open Bolero.Html
open Hexel
open Coxel
open Shape
///

type hxgn = Template<
      """ <polygon 
      points="${pt}" 
      fill="${cl}"
      stroke="${cl}"
      transform="translate${tr}"
      opacity = "0.75"
      stroke-opacity="0.175"
      >""">

type plgn = Template<
        """ <polygon 
        points="${pt}" 
        fill="${cl}"
        opacity = "0.75"
        >""">

type svln = Template<
        """<line
        x1 = "${x1}"
        y1 = "${y1}"
        x2 = "${x2}"
        y2 = "${y2}"
        stroke = "${cl}"
        stroke-with = "2"
        >""">

type crPh = Template<
            """<path
            id = "${pathid}"
            fill = "none"
            d="M ${sx},${sy}
           A ${r},${r} 0 1,1 ${ex},${ey}
           A ${r},${r} 0 1,1 ${sx},${sy}"
            >""">

type crCl = Template<
    """<circle
        cx="${cx}" 
        cy="${cy}" 
        r="5" 
        fill="${cl}" />""">

type crTx = Template<
    """<text
        font-size="10px"
        font-family="Verdana"
        text-anchor="middle"
        style="text-transform: lowercase">
        <textPath
            href="#${pth}"
            letter-spacing="0.5px"
            startOffset="50%">
            ${nm}
        </textPath>
    </text>""">

type svtx = Template<
        """<text 
        x="${xx}" 
        y="${yy}"
        width = "50px"
        font-size = "10px"
        font-family="Verdana"
        text-anchor="middle"
        dominant-baseline="middle"
        fill = "#808080"
        opacity = "1"
        >${nm}</text> """>

///

/// <summary> Scale and Shift origin</summary>
/// <param name="scl"> Scale </param>
/// <param name="hxo"> Array of Hexels arrays </param>
/// <returns> Property array to feed into the Cluster function 
/// Hexel coordinates array * width * height </returns>
let crd 
    (scl : int) 
    (hxo : Hxl[][]) = 
    // Location to Coordinates
    let cdn 
        (hxo:Hxl[]) 
        (scl:int) =
        Array.map (fun a -> match a with 
                            |AV(x,y,z) -> ((x*scl),(y*scl),z)
                            |RV(x,y,z) -> ((x*scl),(y*scl),z)
                            |EX(x,y,z) -> ((x*scl),(y*scl),z)) hxo

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
///

/// <summary> (Unused)Nested Coxels SVG </summary>
/// <param name="cxl"> Array of coxels </param>
/// <param name="clr"> Array of colors </param>
/// <param name="wdt"> Width of SVG (and Height) </param>
/// <returns> Altered Vertices </returns>
let nstdCxls
    (cxl : Cxl[])
    (clr : string[])
    (scl : int)
    (shp : Shp)= 
    let lbl = Array.map (fun x -> prpVlu x.Name) cxl
    let crd = ( Array.map (fun x -> x.Hxls) cxl) 
              |> Array.map (fun x -> Array.map(fun y -> hxlCrd y)x)
    
    // Shift and Scale Vertices
    let vrtx = vertex (Array.head cxl).Seqn shp (AV(0,0,0))
                    |> Array.map (fun (_,x,y) -> [|x;y|])
                    |> Array.concat
                    |> Array.map (fun x -> string (x * scl)) 
                    |> String.concat ","

    // Shift and Scale Vertices
    let padd = 5
    let crdX = crd |> Array.map (fun a -> Array.map(fun (x,_,_) -> x)a) |> Array.concat
    let crdY = crd |> Array.map (fun a -> Array.map(fun (_,y,_) -> y)a) |> Array.concat
    let minX = Array.min crdX
    let maxX = Array.max crdX
    let minY = Array.min crdY
    let maxY = Array.max crdY
    let shfX = (-1 * minX) + padd
    let shfY = (-1 * minY) + padd
    let crd1 = crd |> Array.map (fun a -> Array.map(fun (x,y,_) -> x+shfX,y+shfY)a)
    let crd2 = Array.map (fun x -> Array.map(fun (a,b) -> a*scl,b*scl)x) crd1
    svg{
        attr.width ((maxX-minX+(padd*2))*scl)
        attr.height ((maxY-minY+(padd*2))*scl)
        attr.style $"viewBox: 0 0 {(maxX-minX)*scl} {(maxY-minY)*scl}"
        svg {
             let prp = Array.zip3 crd2 lbl clr
                    
             for cmp in prp do
                 let (xxyy,label,color) = cmp
                 let xy = Array.map(fun (a,b) -> a,b) xxyy
                 let xx = xy |> Array.tryHead
                 let x,y = match xx with 
                             | None -> -10,-10
                             | Some a -> a

                 for locn in xy do
                        hxgn()
                            .pt($"{vrtx}")
                            .tr($"{locn}")
                            .cl($"{color}")
                            .Elt()

                        svtx()
                            .xx($"{x}")
                            .yy($"{y}")
                            .nm($"{label}")
                            .Elt()

            } 
    }
///

/// <summary> Nested Coxels SVG </summary>
/// <param name="cxl"> Array of coxels </param>
/// <param name="clr"> Array of colors </param>
/// <returns> Polygon Vertices </returns>

let nstdCxlsWrp
    (cxl : Cxl[])
    (clr : string[])
    (scl : int)= 
    /// <summary> Coxel Offseted Boundary Wrap </summary>
    /// <param name="cxl"> Coxel. </param>
    /// <returns> Boundary Wrap vertices. </returns>
    let cxlPrm
        (cxl : Cxl) =
        hxlOfs cxl.Seqn cxl.Hxls 
        |> Array.map (fun x -> hxlCrd x) 
        |> Array.map (fun (x,y,_) -> (x,y))
    
    // Vertices
    let crd = Array.map (fun x -> cxlPrm x) cxl
     
    // Shift and Scale Vertices
    let padd = 5*scl
    let crd1 = Array.map (fun x -> Array.map(fun (a,b) -> a*scl,b*scl)x) crd
    let minX1 = fst (Array.minBy (fun (x,_) -> x) (Array.concat crd1))
    let maxX1 = fst (Array.maxBy (fun (x,_) -> x) (Array.concat crd1))
    let minY1 = snd (Array.minBy (fun (_,x) -> x) (Array.concat crd1))
    let maxY1 = snd (Array.maxBy (fun (_,x) -> x) (Array.concat crd1))
    let shfX = (-1 * minX1) + padd
    let shfY = (-1 * minY1) + padd
    let crd2 = Array.map (fun x -> Array.map(fun (a,b) -> a+shfX,b+shfY)x) crd1
    let wdt = (maxX1 - minX1)+(padd*2)
    let hgt = (maxY1 - minY1)+(padd*2)
    
    // Labels
    let lbl = Array.map (fun x -> prpVlu x.Name) cxl

    svg{
        attr.width wdt
        attr.height hgt
        attr.``style`` $"position: relative;"
        attr.``style`` $"viewBox: 0 0 {maxX1-minX1} {maxY1-minY1}"
        svg {
             attr.width wdt
             attr.height hgt
             attr.``style`` $"position: absolute; top: 0; left: 0;"
             attr.``style`` $"viewBox: 0 0 {maxX1-minX1} {maxY1-minY1}"

             let prp = Array.zip crd2 clr
             
             for cmp in prp do
                 let (xxyy,color) = cmp
                 let xy = Array.map(fun (a,b) -> a,b) xxyy
                            |> Array.map (fun (x,y) -> [|x;y|])
                            |> Array.concat
                            |> Array.map (fun x -> string (x)) 
                            |> String.concat ","
                 
                 plgn()
                    .pt($"{xy}")
                    .cl($"{color}")
                    .Elt()
            }
        // Overlay SVG: text labels
        svg {
             attr.width wdt
             attr.height hgt
             attr.``style`` $"position: absolute; top: 0; left: 0;"
             attr.``style`` $"viewBox: 0 0 {maxX1-minX1} {maxY1-minY1}"

             let pth = Array.map(fun x -> ($"path{x}")) [|1..Array.length lbl|]
             let prp1 = Array.zip crd2 clr
             let prp2 = Array.zip lbl pth
             let prp = Array.map2 (fun x y -> fst x, fst y, snd x, snd y) prp1 prp2
             for (xxyy, label, color, path) in prp do
                let x, y =
                    match xxyy with
                    | [||] -> -10, -10
                    | _ ->
                        let a = xxyy |> Array.map(fun (x,_) -> double x) |> Array.average |> int
                        let b = xxyy |> Array.map(fun (_,x) -> double x) |> Array.average |> int
                        a,b

                let r = 10
                crPh()
                    .pathid(path)
                    .sx($"{x}")
                    .sy($"{y+r}")
                    .r($"{r}")
                    .ex($"{x}")
                    .ey($"{y-r}")
                    .Elt()
                
                crTx()
                    .nm($"{label}")
                    .pth(path)
                    .Elt()
                
                crCl()
                    .cx($"{x}")
                    .cy($"{y}")
                    .cl($"{color}")
                    .Elt()

(*                svpt()
                    .cl($"{color}")
                    .xp($"{x}")
                    .yp($"{y}")
                    .Elt()

                svtx()
                    .xx($"{x}")
                    .yy($"{y}")
                    .nm($"{label}")
                    .Elt()*)
        }
    }
///

/// <summary> Hexel Coordinates, Color and Name </summary>
/// <param name="scl"> Scale </param>
/// <param name="sqn"> Sequence </param>
/// <param name="cnt"> Cluster sizes array </param>
/// <returns> Collection of Coxels  
/// Coxel coordinates array * Color * Name </returns>
let cls 
    (scl : int)
    (sqn : Sqn)
    (cnt : int[]) =
    // Host Cluster
    let hsHx01 = 
        (Coxel.coxel sqn [|identity,Refid "",Count (cnt|>Array.head),Label "Host"|] [||]
        |> Array.head).Hxls
        |> Array.append [|RV(0,0,0)|]
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
        [|[|hsHx01|] ; Array.map (fun x -> (x.Hxls))(Coxel.coxel sqn hsHx03 hsHx01)|] 
        |> Array.concat
    let hsHx05 = Array.map(fun x -> (Array.sortBy(fun y -> available sqn y x)x)) hsHx04
    
    // Scaled Coordinates
    hsHx05 |> crd scl
///

/// <summary> Coxel SVG composition </summary>
/// <param name="prp"> Coxel Array of Hexel coordinates (array) * Name * Color </param>
/// <param name="shp"> Shape </param>
/// <param name="scl"> Scale </param>
/// <param name="wdt"> Width </param>
/// <param name="hgt"> Height </param>
/// <returns> SVG of Coxel composition </returns>
let cluster 
    (prp: ((int*int*int)[] * string * string)[]) 
    shp sqn scl wdt hgt : Node =
    svg {
         attr.width wdt
         attr.height hgt
         let vrtx = vertex sqn shp (AV(0,0,0))
                    |> Array.map (fun (_,x,y) -> [|x;y|])
                    |> Array.concat
                    |> Array.map (fun x -> string (x * scl)) 
                    |> String.concat ","
                    
         for cmp in prp do
             let (xyz,label,color) = cmp
             let xy = Array.map(fun (a,b,_) -> a,b) xyz 
             let xx = xy |> Array.rev |> Array.tryHead
             let x,y = match xx with 
                         | None -> -10,-10
                         | Some a -> a
             for locn in xy do
                    hxgn()
                        .pt($"{vrtx}")
                        .tr($"{locn}")
                        .cl($"{color}")
                        .Elt()
             svtx()
                   .xx($"{x+10}")
                   .yy($"{y}")
                   .nm($"{label}")
                   .Elt()
        }
///