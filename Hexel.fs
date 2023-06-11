module Hexel
// Hexel
type Hxl = { Avl : int; Loc : int * int * int }

// Adjacent Hexels
let adj (hxl : Hxl) = 
    match hxl.Avl with 
    | x when x<1 -> hxl |> Array.singleton
    | _ -> 
            let (x,y,z) = hxl.Loc
            Array.map (fun x -> {Avl = 6 ; Loc= x}) 
                (Array.map3 (fun a b c -> 
                    ((a + x), (b + y), (c + z))) 
                    [|0; 1; -1; -2; -1; 1; 2|] 
                    [|0; -2; -2; 0; 2; 2; 0|] 
                    [|0; 0; 0; 0; 0; 0; 0|])

// Available Hexels
let avb (occ : Hxl[]) (hxl : Hxl) = 
    match hxl.Avl with
    | x when x<1 -> {hxl with Avl = 0}
    | _ -> 
            let exp = ([|[|hxl|];occ|]|> Array.concat) |> Array.map (fun x -> x.Loc)
            let ajc = (adj hxl) |> Array.map (fun x -> x.Loc)
            let avl = (Array.except exp ajc) |> Array.length
            {hxl with Avl = avl}

// Except Hexels
let exc (exl : Hxl[]) (hxl : Hxl[]) = 
    let exs hx  = ((Array.map (fun x -> x.Loc)exl) |> Array.contains hx.Loc) = false
    let hx1 = hxl |> Array.filter exs |> Array.distinct
    let hx2 = hx1 |> Array.groupBy (fun x -> x.Loc)
    let hx3 = hx2 |> Array.map (fun (x,y) -> 
                    match (y |> Array.length) with 
                    | 1 -> y |> Array.head
                    | _ -> {Avl = 0 ; Loc= x} )
    hx3

// Incremental Hexels
let inc (hxl : (int * Hxl)[]) (occ : Hxl[]) = 
    let (ct0,hx0) = hxl |> Array.unzip
    let hx1 = hx0 |> Array.map (fun x -> avb  ([|occ;hx0|] |> Array.concat) x)
    let hx2 = Array.zip ct0 hx1
    let hx3 = hx2 |> Array.map ( fun (x,y) -> 
            match x with
            | x when x < 1 -> 0, (y |> Array.singleton)
            | x -> (x-1), (exc ([|hx0;hx1;occ|] |> Array.concat)(adj y )))
    let (ct1,hx4) = hx3 |> Array.unzip
    let hx5 = (hx4 |> Array.scan (fun ac st -> ([|(exc ac st) |> Array.head|]) ) [||]) |> Array.tail 
    let hx6 = hx5 |> Array.map (fun x -> match x with 
                                                                |[||] -> None
                                                                |x -> Some (Array.head x))
    let hx7 = Array.map2 (fun x y -> match y with 
                                                | None -> x 
                                                | Some y -> y) hx1 hx6
    let hx8 = hx7 |> Array.map (fun x -> avb ([|occ;hx0;hx1;hx7|] |> Array.concat) x)
    Array.zip ct1 hx8

// Non Uniform Clusters
let nui (hxc : (int * Hxl)[]) (occ : Hxl[]) = 
    let (ct1,hx0) = hxc|> Array.unzip
    let hx1 = hx0 |> Array.map (fun x -> avb  ([|occ;hx0|] |> Array.concat) x)
    let mxc = (Array.max ct1) + 1
    let acc = (Array.map(fun x -> Array.singleton x)hx1)
    let rec nux (hxc : (int * Hxl)[]) (occ : Hxl[]) (mxc : int) (acc : Hxl[][]) = 
        match mxc with 
        | mxc when (mxc <= 1) -> acc
        | _ ->
                        let h01 = inc hxc occ
                        let (c02,h02) = h01 |> Array.unzip
                        let ac1 = Array.map2 (fun x y -> [x ; [|y|]]|> Array.concat) acc h02
                        let oc1 = [|(Array.concat ac1) ; occ|] |> Array.concat |> Array.distinct
                        let ac2 = ac1 |> Array.map(fun x ->Array.map(fun y -> avb oc1 y)x)
                        let acc = ac2 |> Array.map(fun x -> Array.distinct x)
                        let ac3 = acc |> Array.map(fun x -> Array.filter (fun y -> y.Avl > 0)x)
                        let h00 = (Array.map (fun x -> Array.tryHead x ) ac3)
                        let h02 = Array.map2 (fun x y -> match x with 
                                                                    | Some x -> x
                                                                    | None -> y) h00 h02
                        let hxc = Array.zip c02 h02
                        let occ = [occ ; (acc |> Array.concat) ; h02] |> Array.concat |> Array.distinct
                        nux hxc occ (mxc-1) acc
    (nux hxc occ mxc acc) |> Array.map(fun x -> Array.distinct x) 
