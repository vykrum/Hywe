module Shapes

open Hexel

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