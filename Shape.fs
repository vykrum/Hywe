module Shape

open Hexel
open Coxel
open System
///

/// <summary> Module shape in tessalated hexagonal grid. </summary>
/// <typeparam name="Hxg"> Hexagon. </typeparam>
/// <typeparam name="Sqr"> Square. </typeparam>
/// <typeparam name="Arw"> Arrow. </typeparam>
/// <typeparam name="Prl"> Parallelogram. </typeparam>
type Shp = 
    | Hxg | Sqr | Arw | Prl

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
    |> Array.map2 ( fun inx (vrx,vry) 
                        -> string(inx),vrx,vry) 
                        [|0..(Array.length hxCr)-1|]
///

/// <summary> Ortogonal Hexel Sequence </summary>
/// <param name="sqn"> Sequence to follow. </param>
/// <param name="org"> Start Hexel. </param> 
/// <param name="lgt"> Sequence Length. </param> 
/// <param name="vrt"> Vertical or Horizontal Orientation. </param> 
/// <param name="rev"> Positive or Negative Direction. </param> 
/// <returns> Array of Sequential Reserved Hexels. </returns>
let hxlOrt
        (sqn : Sqn)
        (org : Hxl)
        (lgt : int)
        (vrt : bool)
        (rev : bool) =
        let hxx,hxy,hxz = org |> hxlVld sqn |> hxlCrd
        let lgt = lgt + (lgt%2)
        let sgn = match rev with 
                    | true -> -1
                    | false -> 1         
        match sqn with
        | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE
            -> match vrt with 
                | true -> [|hxy..4*sgn..(hxy+lgt+4)*sgn|]
                        |> Array.map (fun y -> [|EX(hxx,y,hxz);EX(hxx+1,y+2*sgn,hxz)|])
                        |> Array.concat
                        |> Array.take ((lgt/2)+1)
                | false -> [|hxx..2*sgn..(hxx+lgt+4)*sgn|]
                        |> Array.map (fun x -> EX (x,hxy,hxz)) 
                        |> Array.take ((lgt/2)+1)
        | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW
            -> match vrt with
                | true -> [|hxy..2*sgn..(hxy+lgt)*sgn|]
                        |> Array.map (fun y -> RV (hxx,y,hxz)) 
                        |> Array.take ((lgt/2)+1)
                | false -> [|hxx..4*sgn..(hxx+lgt)*sgn|] 
                        |> Array.map (fun x -> [|EX(x,hxy,hxz);EX(x+2*sgn,hxy+1,hxz)|])
                        |> Array.concat
                        |> Array.take ((lgt/2)+1)

let hxlOff
    (hxl : Hxl[])
    (rev : bool) = 
    let neg = match rev with 
                | true -> -1
                | false -> 1
    let crd = Array.map (fun a -> hxlCrd a) hxl
    let x1,y1,_ = Array.head crd
    let x2,y2,_ = Array.get crd 1
    match (x1 = x2) with 
    | true -> Array.map(fun (x,y,z) -> EX(x+(2*neg),y-1,z)) crd
    | false -> match (y1=y2) with 
                | true -> Array.map(fun (x,y,z) -> EX(x+1,y+(2*neg),z)) crd
                | false -> match (Math.Abs(x2-x1)=1) with 
                            | true -> Array.map(fun (x,y,z) -> EX(x+(2*neg),y,z)) crd
                            | false -> Array.map(fun (x,y,z) -> EX(x,y+(2*neg),z)) crd

let hxlRct
    (sqn : Sqn)
    (wdt : int)
    (hgt : int)
    (str : int) = 
    let sqn = match sqn with
                | HRCWNN | HRCCNN | HRCWNE | HRCCNE | HRCWSE | HRCCSE | HRCWSS | HRCCSS | HRCWSW | HRCCSW | HRCWNW | HRCCNW-> HRCWNN
                | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE -> VRCWEE
    let org = hxlVld sqn (AV(0,0,0))
    let hrz1 = hxlOrt sqn org ((wdt+1)*2) false false
    let vrt1 = hxlOrt sqn (Array.last hrz1) ((hgt+1)*2) true false
    let hrz2 = hxlOrt sqn (Array.last vrt1) ((wdt+1)*2) false true
    let vrt2 = hxlOrt sqn (Array.last hrz2) ((hgt+1)*2) true true

    let bs1 = match str with 
                | 1 -> Array.get hrz1 ((Array.length hrz1)/2)
                | 2 -> vrt1 |> Array.tail |> Array.head
                | 3 -> Array.get vrt1 ((Array.length vrt1)/2)
                | 4 -> hrz2 |> Array.tail |> Array.head
                | 5 -> Array.get hrz2 ((Array.length hrz2)/2)
                | 6 -> vrt2 |> Array.tail |> Array.head
                | 7 -> Array.get vrt2 ((Array.length vrt2)/2)
                | _ -> hrz1 |> Array.tail |> Array.head

    let rct = Array.concat[|hrz1; vrt1; hrz2; vrt2; 
                    hxlOff hrz1 true; 
                    hxlOff vrt1 false; 
                    hxlOff hrz2 false; 
                    hxlOff vrt2 true|] 
                    |> Array.distinct
    let bas = AV(hxlCrd bs1) |> adjacent sqn |> hxlUni 3 |> Array.except rct |> Array.head
    AV(hxlCrd bas),rct

let cxlPrm
    (cxl : Cxl) = 
    let sqn = cxl.Seqn
    let hx1 = cxl.Hxls |> hxlUni 1 
    // Boundary Hexels
    let hxBd = (cxlHxl cxl).Prph
            |> hxlUni 1 
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
                |> Array.map (fun x -> Array.filter(fun (_,y) -> y < 3)x)
                |> Array.concat
                |> Array.map (fun ((_,x,y),_) -> x,y)
                |> Array.distinct
    vrBrIn1
