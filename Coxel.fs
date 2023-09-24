module Coxel

open Hexel

type Cxl = 
    {
        Name : string
        Size : int
        Seqn : Sqn
        Base : Hxl
        Hxls : Hxl[]
    }  

// Coxel
let coxel 
    (sqn : Sqn)
    (ini : (Hxl*int*string)[])
    (occ : Hxl[]) = 
    
    let bas = Array.map(fun (x,y,z) -> x,y) ini
    let szn = Array.map(fun (x,y,z) -> y,z) ini

    let cnt = 
            bas
            |> Array.map (fun x -> snd x)
            |> Array.max
    let acc = Array.chunkBySize 1 bas
    let occ = (Array.append occ (getHxls bas)) |> allOG 
    
    let rec clsts 
        (hxo: (Hxl*int)[])
        (occ : Hxl[])
        (acc:(Hxl*int)[][])
        (cnt : int) = 
        
        match cnt with 
        | c when c < 0x1 -> acc
        | _ -> 
                let occ = 
                    acc 
                    |> Array.concat 
                    |> getHxls
                    |> Array.append occ
                    |> Array.append (getHxls hxo)
                    |> Array.append [|identity|]
                    |> Array.distinct
                    |> allOG

                let rpt = Array.map (fun x 
                                        -> (snd x) - 0x1) hxo
                let Hxl =  
                    acc
                    |> Array.map (fun x
                                    -> Array.filter (fun a 
                                                        -> (available sqn a occ) > 0x0) x)
                    |> Array.map (fun x 
                                    -> Array.tryHead x)
                    |> Array.map (fun x 
                                    -> match x with
                                        | Some a -> a 
                                        | None -> (identity,0xFFFFFFFF))                
                    |> Array.map2 (fun x y 
                                    -> fst y, x) rpt
                
                let inc = increments sqn Hxl occ
                          
                let acc = Array.map2  (fun x y
                                        -> Array.append x y) 
                            acc
                            (Array.chunkBySize 1 inc)

                let occ = Array.concat[|getHxls (Array.concat [|Array.concat acc; inc;Hxl|]);occ|] |> allOG

                (clsts Hxl occ acc (cnt - 0x1))


    let cls = 
        clsts bas occ acc cnt
            |> Array.map(fun x 
                            -> Array.filter(fun (_,z) -> z >= 0) x)

    let cl1 = 
        cls
        |> Array.map(fun x -> getHxls x)
    
    let bs1 =  (getHxls bas)
  
    let cxl = Array.map3 (fun x y z -> 
                                            {
                                                Name = snd x
                                                Size = fst x
                                                Seqn = sqn
                                                Base = y
                                                Hxls = z
                                            })szn bs1 cl1
    cxl

// Coxel Hexel Groups
let cxlHxl
    (cxl : Cxl) 
    (occ : Hxl[]) = 

    // Boundry Hexels Ring
    let bndSqn 
        (sqn : Sqn) 
        (hxl : Hxl[]) = 
    
        let rec arr 
            (sqn : Sqn) 
            (hxl : Hxl[]) 
            (acc : Hxl[]) 
            (cnt : int)
            (opt : bool) = 
            match cnt with 
            | a when cnt <= 0x1 -> acc
            | _ -> 
                let hxl = Array.except acc hxl
                let hx1 = ((Array.filter (fun x -> Array.contains x hxl) 
                                (adjacent sqn (Array.last acc))))                
                let hx2 = match opt with 
                                | false -> Array.tryHead hx1
                                | true -> Array.tryLast hx1
                let hx3 = match hx2 with 
                                | Some a -> [|a|]
                                | None -> [||]
                let acc = Array.append acc  hx3
                arr sqn hxl acc (cnt-1) opt
        
        let a1 = 
            match hxl with 
            | [||] -> [||]
            | _ -> arr sqn hxl [|Array.last hxl|] (Array.length hxl) true

        let b1 = Array.length a1 = Array.length hxl
        
        match b1 with 
        | true -> a1
        | false -> arr sqn hxl [|Array.last hxl|] (Array.length hxl) false

    // Hexel Ring Segment Sequence
    let cntSqn
        (sqn : Sqn)
        (hxl : Hxl[]) = 
    
        let rec ctSq 
            (sqn : Sqn)
            (hxl : Hxl[])
            (acc : Hxl[])
            (cnt : int) = 
            match cnt with 
            | a when a<=1 -> acc
            | _ -> 
                    let b = Array.last acc
                    let hxl = Array.except [|b|] hxl
                    let d = (adjacent sqn b) |> Array.tail
                    let e = d |> Array.filter(fun x -> Array.contains x hxl) |> Array.tryHead
                    let f = match e with 
                                | Some a -> [|a|]
                                | None -> [||]
                    let acc = Array.append acc f
                    ctSq sqn hxl acc (cnt-1)
        let cnt = Array.length(hxl)
        let arr =  ctSq sqn hxl ([|Array.head hxl|]) cnt
        let bln = cnt = Array.length(arr)
        match bln with 
        | true -> arr
        | false -> ctSq sqn (Array.rev hxl) ([|Array.last hxl|]) cnt

    let cl1 = cxl.Hxls
    // Bounding Hexels
    let cl2 =  Array.tail cl1
    let cl3 = Array.partition(fun x-> (available cxl.Seqn x occ) > 0) cl2
    let bd1 = fst  cl3
    let bd2 = bndSqn cxl.Seqn bd1
    
    // Core Hexels
    let cr1 = snd cl3
    
    let oc1 = Array.concat
                [|
                    occ 
                    cxl.Hxls
                |] |> allOG
    
    let cl4 = Array.partition(fun x-> (available cxl.Seqn x oc1) > 0) bd2
    
    // Available Hexels
    let av1= fst cl4
    let av2 = cntSqn cxl.Seqn av1
    
    // Border Hexels
    let br1= snd cl4

    // Output : Base, Hxls, Core, Prph, Brdr, Avbl
    
    {|
        Base = cxl.Base
        Hxls = cl1
        Core = cr1
        Prph = bd2
        Brdr = br1
        Avbl = av2
    |}    
    