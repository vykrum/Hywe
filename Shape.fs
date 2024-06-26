﻿module Shape

open Hexel
open Coxel
///

/// <summary> Module shape in tessalated hexagonal grid. </summary>
/// <typeparam name="Hxg"> Hexagon. </typeparam>
/// <typeparam name="Sqr"> Square. </typeparam>
/// <typeparam name="Arw"> Arrow. </typeparam>
/// <typeparam name="Prl"> Parallelogram. </typeparam>
type Shp = 
        | Hxg | Sqr | Arw | Prl
///

// Hexel Vertices
let vertex
    (sqn : Sqn)
    (shp : Shp)
    (hxl : Hxl) = 
    let hxCr = 
        match shp with 
        | Hxg 
            -> match sqn with 
                |VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE 
                    -> [|0x0,0x0; 0x1,0x1; 0x2,0x0; 0x2,0xFFFFFFFF; 0x1,0xFFFFFFFE; 0x0,0xFFFFFFFF|]
                | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
                    -> [|0x0,0x0; 0x1,0x0; 0x2,0xFFFFFFFF; 0x1,0xFFFFFFFE; 0x0,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFF|]
        | Sqr 
            -> match sqn with 
                |VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE
                    -> [|0x0,0x0; 0x1,0x0; 0x2,0x0; 0x2,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x0,0xFFFFFFFE|]
                | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
                    -> [|0x0,0x0; 0x2,0x0; 0x2,0xFFFFFFFF; 0x2,0xFFFFFFFE; 0x0,0xFFFFFFFE; 0x0,0xFFFFFFFF|]
        | Arw 
            -> match sqn with 
                |VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE
                    -> [|0x0,0x0; 0x1,0xFFFFFFFF; 0x2,0x0; 0x2,0xFFFFFFFD; 0x1,0xFFFFFFFE; 0x0,0xFFFFFFFD|]
                | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
                    -> [|0x0,0x0; 0x3,0x0; 0x2,0xFFFFFFFF; 0x3,0xFFFFFFFE; 0x0,0xFFFFFFFE; 0x1,0xFFFFFFFF|]
        | Prl 
            -> match sqn with 
                |VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE
                    -> [|0x0,0x0; 0x1,0x0; 0x2,0x0; 0x1,0xFFFFFFFE; 0x0,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE|]
                | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
                    -> [|0x0,0x0; 0x2,0x1; 0x2,0x0; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x0,0xFFFFFFFF|] 
         
    let x, y, _ = hxl |> hxlCrd 
    hxCr 
    |> Array.map(fun (a,b)-> a + x, b + y) 
    |> Array.map2 ( fun inx (vrx,vry) -> string(inx),vrx,vry) [|0..(Array.length hxCr)-1|]
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
    | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE
        -> match vrt with 
            | true -> [|hxy..4..(hxy+lgt+4)|]
                    |> Array.map (fun y -> [|RV(hxx,y,hxz);RV(hxx+1,y+2,hxz)|])
                    |> Array.concat
                    |> Array.take ((lgt/2)+1)
            | false -> Array.map (fun x -> RV (x,hxy,hxz)) [|hxx..2..(hxx+lgt+4)|]
                    |> Array.take ((lgt/2)+1)
    | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
        -> match vrt with
            | true -> Array.map (fun y -> RV (hxx,y,hxz)) [|hxy..2..(hxy+lgt)|]
                    |> Array.take ((lgt/2)+1)
            | false -> [|hxx..4..(hxx+lgt)|] 
                    |> Array.map (fun x -> [|RV(x,hxy,hxz);RV(x+2,hxy+1,hxz)|])
                    |> Array.concat
                    |> Array.take ((lgt/2)+1)
///

// Coxel Perimeter Vertices
let cxlPrm
    (cxl : Cxl) = 
    let sqn = cxl.Seqn
    let hx1 = cxl.Hxls |> allAV false 
    // Boundary Hexels
    let hxBd = (cxlHxl cxl).Prph
            |> allAV false                    
    // All hexel vertices
    let vrHx = Array.map(fun x -> vertex sqn Hxg x) hx1
    // Vertices shared Hexel Count 
    let vrHxCt = vrHx   
                |> Array.concat 
                |> Array.groupBy (fun (_,x,y) -> x,y)
                |> Array.map (fun (x,y) -> x,Array.length y)
                |> Map.ofArray
    // Boundary Hexel Vertices
    let vrBd = hxBd 
            |> Array.map(fun x -> vertex sqn Hxg x) 
    // Vertex Cell Count 
    let vrBdCt = 
                let vrBd1 = Array.map(fun x -> Array.map(fun (a,b,c)->b,c)x)vrBd
                vrBd1
                |> Array.map(fun x 
                                -> Array.map(fun y 
                                                -> Map.find 
                                                    y 
                                                    vrHxCt)x)
    let vrBdCdCt = Array.map2 (fun x y ->Array.map2(fun a b -> a,b)x y) vrBd vrBdCt
    // Break Index in vertex sequence
    let vrBrIn = 
        let a = vrBdCdCt
                |> Array.map(fun x 
                                -> Array.tryFindIndexBack (fun y -> (snd y)<3)x)
                |> Array.map (fun x 
                                -> Option.defaultWith (fun () -> 0)x)
                |> Array.map2 (fun x y 
                                -> match y<5 with
                                    | false -> x
                                    | true ->   let a,b = Array.splitAt (y+1) x
                                                Array.append b a  )vrBdCdCt 
        let b = a
                |> Array.map(fun x 
                                -> Array.tryFindIndexBack (fun y -> (snd y)>2)x)
                |> Array.map (fun x 
                                -> Option.defaultWith (fun () -> 0)x)
                |> Array.map2 (fun x y 
                                -> match y<5 with
                                    | false -> x
                                    | true ->   let a,b = Array.splitAt (y+1) x
                                                Array.append b a  )a      
        b     
    let vrBrIn1 = vrBrIn
                |> Array.map (fun x -> Array.filter(fun (_,y)-> y<3)x)
                |> Array.concat
                |> Array.map (fun ((_,x,y),_) -> x,y)
                |> Array.distinct
    vrBrIn1
///