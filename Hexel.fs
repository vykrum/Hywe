module Hexel

    type Hxl = {
        Avl : bool
        Loc : int * int * int
    }

    let adj (hxl : Hxl) = 
        match hxl.Avl with 
        | false -> hxl |> List.singleton
        | true -> 
                let (x,y,z) = hxl.Loc
                List.map (fun x -> {Avl = true ; Loc= x}) 
                    (List.map3 (fun a b c -> 
                        ((a + x), (b + y), (c + z))) 
                        [0; 1; 2; 1; -1; -2; -1] 
                        [0; 2; 0; -2; -2; 0; 2] 
                        [0; 0; 0; 0; 0; 0; 0])

    let exc (exl : Hxl List) (hxl : Hxl list) = 
        let cts hx  = ((List.map (fun x -> x.Loc)exl) 
            |> List.contains hx.Loc) = false
        let hx1 = hxl |> List.filter cts |> List.distinct
        let hx2 = hx1 |> List.groupBy (fun x -> x.Loc)
        hx2 |> List.map (fun (x,y) -> 
                match (y.Length) with 
                | 1 -> y |> List.head
                | _ -> {Avl = false ; Loc= x} )

    let chk (hxl : Hxl) (occ : Hxl list) = 
        match hxl.Avl with
        | true -> match ((exc (hxl::occ) (adj hxl)) |> List.length) with 
                        | 0 -> {hxl with Avl = false}
                        | _ -> hxl
        | false -> hxl

    let inc (hxl : (int * Hxl) list) (occ : Hxl list) = 
        let hxc = hxl |> List.map ( fun (x,y) -> 
            match x with
            | x when x < 1 -> 0, (y |> List.singleton)
            | x -> (x-1), (adj y))
        let (_,hx1) = hxl |> List.unzip
        let (ct2 , hx2) = hxc |> List.unzip
        let oc1 = occ @ hx1
        let hx3 = hx2 |> List.scan  (fun ac st-> 
                let oc1 = ac @ oc1
                let h01 = (exc oc1 st)
                let h02 = h01 |> List.tryHead
                match h02 with 
                | Some h02 -> [h02]
                | None -> [st |> List.head]) []
                |> List.tail 
                |> List.map (fun x -> List.head x)
        List.zip ct2 hx3

    let nui (hxc : (int * Hxl) list) (occ : Hxl list) = 
            let (ct1,hx1) = hxc|> List.unzip
            let mxc = (List.max ct1) + 1
            let acc = (List.map(fun x -> List.singleton x)hx1)
            let rec nux (hxc : (int * Hxl) list) (occ : Hxl list) (mxc : int) (acc : Hxl list list) = 
                match mxc with 
                | mxc when (mxc <= 1) -> acc
                | _ ->
                        let h01 = inc hxc occ
                        let (c02,h02) = h01 |> List.unzip
                        let ac1 = List.map2 (fun x y -> x@[y]) acc h02
                        let oc1 = (List.concat ac1) @ occ
                        let ac2 = ac1 |> List.map(fun x ->List.map(fun y -> chk y oc1)x)
                        let acc = ac2 |> List.map(fun x -> List.distinct x)
                        let ac3 = acc |> List.map(fun x -> List.filter (fun y -> y.Avl=true)x)
                        let h00 = (List.map (fun x -> List.tryHead x ) ac3)
                        let h02 = List.map2 (fun x y -> match x with 
                                                        | Some x -> x
                                                        | None -> y) h00 h02
                        let hxc = List.zip c02 h02
                        let occ = [occ @ (acc |> List.concat) @ h02] |> List.concat 
                        nux hxc occ (mxc-1) acc
            (nux hxc occ mxc acc) |> List.map(fun x -> List.distinct x)