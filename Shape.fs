module Shape

open Hexel
///

/// <summary> Module shape in tessalated hexagonal grid. </summary>
/// <typeparam name="HxFl"> Hexagon Flat-Top. </typeparam>
/// <typeparam name="HxPt"> Hexagon Pointy-Top. </typeparam>
/// <typeparam name="QdSq"> Square. </typeparam>
/// <typeparam name="PrFl"> Parallelogram Flat. </typeparam>
/// <typeparam name="PrAn"> Parallelogram Angled. </typeparam>
/// <typeparam name="RhHr"> Rhombus Horizontal. </typeparam>
/// <typeparam name="RhVr"> Rhombus Vertical. </typeparam>
type Shp = 
        | HxFl | HxPt | QdSq | RhHr | RhVr | PrFl | PrAn

///

/// <summary> Ortogonal Hexel Sequence </summary>
/// <param name="sqn"> Sequence to follow. </param>
/// <param name="org"> Start Hexel. </param> 
/// <param name="lgt"> Sequence Length. </param> 
/// <param name="vrt"> Vertical / Horizontal. </param> 
/// <returns> Array of Sequential Reserved Hexels. </returns>
let hxlOrt 
    (sqn : Sqn)
    (org : Hxl)
    (lgt : int)
    (vrt : bool) =
    let hxx,hxy,hxz = org |> hxlVld sqn |> hxlCrd
    let lgt = lgt + (lgt%2)         
    match sqn with
    | SQ11 | SQ12 | SQ13 | SQ14 | SQ15 | SQ16 | SQ17 | SQ18 | SQ19 | SQ20 | SQ21 | SQ22
        -> match vrt with 
            | true -> [|hxy..4..(hxy+lgt+4)|]
                    |> Array.map (fun y -> [|RV(hxx,y,hxz);RV(hxx+1,y+2,hxz)|])
                    |> Array.concat
                    |> Array.take ((lgt/2)+1)
            | false -> Array.map (fun x -> RV (x,hxy,hxz)) [|hxx..2..(hxx+lgt+4)|]
                    |> Array.take ((lgt/2)+1)
    | SQ23 | SQ24 | SQ25 | SQ26 | SQ27 | SQ28 | SQ29 | SQ30 | SQ31 | SQ32 | SQ33 | SQ34
        -> match vrt with
            | true -> Array.map (fun y -> RV (hxx,y,hxz)) [|hxy..2..(hxy+lgt)|]
                    |> Array.take ((lgt/2)+1)
            | false -> [|hxx..4..(hxx+lgt)|] 
                    |> Array.map (fun x -> [|RV(x,hxy,hxz);RV(x+2,hxy+1,hxz)|])
                    |> Array.concat
                    |> Array.take ((lgt/2)+1)
///

// Hexel Vertices
let vertex
    (sqn : Sqn)
    (shp : Shp)
    (hxl : Hxl) = 
    let hxCr = match sqn with 
                | SQ11 | SQ12 | SQ13 | SQ14 | SQ15 | SQ16 | SQ17 | SQ18 | SQ19 | SQ20 | SQ21 | SQ22 
                    -> match shp with
                        | QdSq -> [|0,0; 2,0; 2,-2; 0,-2|]
                        | RhVr -> [|0,0; 1,2; 2,0; 1,-2|]
                        | PrFl -> [|0,0; 2,0; 1,-2; -1,-2|]
                        | HxPt -> [|0,0; 1,1; 2,0; 2,-1; 1,-2; 0,-1|]
                        | _ -> [|0,0; 1,1; 2,0; 2,-1; 1,-2; 0,-1|]
                | SQ23 | SQ24 | SQ25 | SQ26 | SQ27 | SQ28 | SQ29 | SQ30 | SQ31 | SQ32 | SQ33 | SQ34
                    -> match shp with 
                        | RhHr -> [|0,0; 2,1; 4,0; 2,-1|]
                        | PrAn -> [|0,0; 2,1; 2,-1; 0,-2|]
                        | HxFl -> [|0,0; 1,1; 2,1; 3,0; 2,-1; 1,-1|]
                        | _ -> [|0,0; 1,1; 2,1; 3,0; 2,-1; 1,-1|]
    let x, y, _ = hxl |> hxlCrd 
    hxCr |> Array.map(fun (a,b)-> a + x, b + y)