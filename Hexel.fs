module Hexel

type Hxl = 
    | OG of int * int

type Sqn = 
    | EECW
    | EECC
    | SECW
    | SECC
    | SWCW
    | SWCC
    | WWCW
    | WWCC
    | NWCW
    | NWCC
    | NECW
    | NECC

// Sequences     
let sequence (sqn:Sqn) =  
    match sqn with 
    | EECW -> [|0,0; 2,0; 1,-2; -1,-2; -2,0; -1,2; 1,2|]
    | EECC -> [|0,0; 2,0; 1,2; -1,2; -2,0; -1,-2; 1,-2|]
    | SECW -> [|0,0; 1,-2; -1,-2; -2,0; -1,2; 1,2; 2,0|]
    | SECC -> [|0,0; 1,-2; 2,0; 1,2; -1,2; -2,0; -1,-2|]
    | SWCW -> [|0,0; -1,-2; -2,0; -1,2; 1,2; 2,0; 1,-2|]
    | SWCC -> [|0,0; -1,-2; 1,-2; 2,0; 1,2; -1,2; -2,0|]
    | WWCW -> [|0,0; -2,0; -1,2; 1,2; 2,0; 1,-2; -1,-2|]
    | WWCC -> [|0,0; -2,0; -1,-2; 1,-2; 2,0; 1,2; -1,2|]
    | NWCW -> [|0,0; -1,2; 1,2; 2,0; 1,-2; -1,-2; -2,0|]
    | NWCC -> [|0,0; -1,2; -2,0; -1,-2; 1,-2; 2,0; 1,2|]
    | NECW -> [|0,0; 1,2; 2,0; 1,-2; -1,-2; -2,0; -1,2|]
    | NECC -> [|0,0; 1,2; -1,2; -2,0; -1,-2; 1,-2; 2,0|]

// Identity Hexel
let identity = 
    OG(0,0)

// Adjacent Hexels
let adjacent 
    (sqn: Sqn)
    (hxo: Hxl) = 
    Array.map (fun (a,b) -> 
    let (OG (x,y)) = hxo
    OG(x+a,y+b) )(sequence sqn)

// Increment Hexel
let increment 
    (sqn : Sqn)
    (hxo : Hxl * int) 
    (occ : Hxl[]) = 
    let occ = Array.concat 
                [|
                    occ
                    [|(fst hxo)|]
                    [|identity|]
                |]
    match hxo with 
    | x,y when y >= 0 -> 
        let inc1 = x 
                |> adjacent sqn
                |> Array.except occ
        let inc2 = match (Array.tryItem  1 inc1) with 
                        | Some a -> 
                                        let bl1 = Array.contains (Array.head inc1) (adjacent sqn a)
                                        match bl1 with 
                                        | true -> Array.tryHead inc1
                                        | false -> x
                                                |>  adjacent sqn 
                                                |> Array.except occ
                                                |> Array.tryLast

                        | None -> Array.tryHead inc1
        match inc2 with 
        | Some a -> a, y
        | None -> (identity,-1)
    | _ -> (identity,-1)

// Get Hexel from tuple 
let getHxls 
    (hxo : (Hxl*int)[]) = 
    hxo
    |> Array.map(fun x 
                    -> fst x)

// Available Adjacent Hexels
let available 
    (sqn : Sqn)
    (hxo : (Hxl*int))
    (occ : Hxl[]) = 
    hxo 
    |> fst 
    |> adjacent sqn
    |> Array.except (Array.append occ [|(fst hxo)|])
    |> Array.length

// Increment Hexels
let increments 
    (sqn : Sqn)
    (hxo : (Hxl*int)[]) 
    (occ : Hxl[]) = 
    
    let occ = Array.append occ (getHxls hxo)
    let inc = 
        Array.scan (fun ac st -> 
        let occ = (Array.concat [|occ;[|fst st|];[|fst ac|];[|identity|]|] )
        increment sqn st (Array.append[|fst ac|] occ )) 
            hxo[0] hxo
            |> Array.tail
    
    let replaceDuplicate 
        (sqn : Sqn)
        (hxo : (Hxl*int)[]) 
        (inc : (Hxl*int)[]) 
        (occ : Hxl[]) =
        let in1 = Array.map (fun x -> snd x)inc
        let lc1 = getHxls hxo 
        let ic1 = getHxls inc 
        let oc1 = Array.concat[|occ;lc1;ic1|]
        let id1 = Array.map(fun y -> Array.findIndex (fun x -> x = y)ic1)ic1
        let bl1 = Array.map2 (fun x y -> x=y) [|0..(Array.length ic1)-1|] id1   
        let tp1 = Array.zip3 bl1 ic1 hxo  
        tp1 |> Array.map2 (fun d (a,b,c) 
                            -> match a with 
                                | true -> b,d
                                | false -> 
                                        match ((available sqn c oc1)>0) with 
                                        | false -> (fst c),-1
                                        | true -> fst(increment sqn c oc1),d) in1

    replaceDuplicate sqn hxo inc occ

// Clusters
let clusters 
    (sqn : Sqn)
    (bas : (Hxl*int)[])
    (occ : Hxl[]) = 
    
    let cnt = 
            bas
            |> Array.map (fun x -> snd x)
            |> Array.max
    let acc = Array.chunkBySize 1 bas
    let occ = Array.append occ (getHxls bas)  
    
    let rec clsts 
        (hxo: (Hxl*int)[])
        (occ : Hxl[])
        (acc:(Hxl*int)[][])
        (cnt : int) = 
        match cnt with 
        | c when c < 1 -> acc
        | _ -> 
                let occ = 
                    acc 
                    |> Array.concat 
                    |> getHxls
                    |> Array.append occ
                    |> Array.append (getHxls hxo)
                    |> Array.append [|identity|]
                    |> Array.distinct

                let rpt = Array.map (fun x 
                                        -> (snd x)-1) hxo
                let Hxl =  
                    acc
                    |> Array.map (fun x
                                    -> Array.filter (fun a 
                                                        -> (available sqn a occ) > 0) x)
                    |> Array.map (fun x 
                                    -> Array.tryHead x)
                    |> Array.map (fun x 
                                    -> match x with
                                        | Some a -> a 
                                        | None -> (identity,-1))                
                    |> Array.map2 (fun x y 
                                    -> fst y,  x) rpt
                
                let inc = increments sqn Hxl occ
                
                            
                let acc = Array.map2  (fun x y
                                        -> Array.append x y) 
                            acc
                            (Array.chunkBySize 1 inc)

                let occ = Array.concat[|getHxls (Array.concat [|Array.concat acc; inc;Hxl|]);occ|]

                (clsts Hxl occ acc (cnt-1))
                
    let cls = clsts bas occ acc cnt
            |> Array.map(fun x 
                            -> Array.filter(fun (_,z) -> z >= 0) x)
    let cl0 = 
        cls
        |> Array.map(fun x -> getHxls x)
        
    // Hxlations to Clusters
    let bs1 = getHxls bas
    let oc1 = Array.concat
                [|
                    occ 
                    (getHxls(Array.concat cls))
                |] 
    let cl1 = 
        cls 
        |> Array.map (fun x -> Array.partition(fun y -> (available sqn y oc1) = 0)x)
    let cr1 = 
        Array.map (fun x -> getHxls (fst x)) cl1
            |> Array.map(fun x -> Array.except (getHxls bas)x)
    let ed1 = 
        Array.map (fun x -> getHxls (snd x)) cl1
            |> Array.map(fun x -> Array.except (getHxls bas)x)
    
    {|
        Base = bs1
        Hxls = cl0
        Core = cr1
        Edge = ed1
    |}
