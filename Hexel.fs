module Hexel

[<Struct>]
type Hxl = 
    | OG of int * int

[<Struct>]
type Sqn = 
    // Vertical,Horizontal,Clockwise,Anticlockwise,North,South,East,West
    | VCEE | VAEE | VCSE | VASE | VCSW | VASW | VCWW | VAWW | VCNW | VANW | VCNE | VANE
    | HCNN | HANN | HCNE | HANE | HCSE | HASE | HCSS | HASS | HCSW | HASW | HCNW | HANW

// Sequence Variation
let sequence (sqn:Sqn) =  
    match sqn with 
    | VCEE -> [|0x0,0x0; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2|]
    | VAEE -> [|0x0,0x0; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE|]
    | VCSE -> [|0x0,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0|]
    | VASE -> [|0x0,0x0; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE|]
    | VCSW -> [|0x0,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE|]
    | VASW -> [|0x0,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0|]
    | VCWW -> [|0x0,0x0; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE|]
    | VAWW -> [|0x0,0x0; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2; 0xFFFFFFFF,0x2|]
    | VCNW -> [|0x0,0x0; 0xFFFFFFFF,0x2; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0|]
    | VANW -> [|0x0,0x0; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0; 0x1,0x2|]
    | VCNE -> [|0x0,0x0; 0x1,0x2; 0x2,0x0; 0x1,0xFFFFFFFE; 0xFFFFFFFF,0xFFFFFFFE; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0x2|]
    | VANE -> [|0x0,0x0; 0x1,0x2; 0xFFFFFFFF,0x2; 0xFFFFFFFE,0x0; 0xFFFFFFFF,0xFFFFFFFE; 0x1,0xFFFFFFFE; 0x2,0x0|]
    | HCNN -> [|0x0,0x0; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1|]
    | HANN -> [|0x0,0x0; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1|]
    | HCNE -> [|0x0,0x0; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2|]
    | HANE -> [|0x0,0x0; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF|]
    | HCSE -> [|0x0,0x0; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1|]
    | HASE -> [|0x0,0x0; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE|]
    | HCSS -> [|0x0,0x0; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF|]
    | HASS -> [|0x0,0x0; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF|]
    | HCSW -> [|0x0,0x0; 0xFFFFFFFE,0xFFFFFFFF; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE|]
    | HASW -> [|0x0,0x0; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2; 0xFFFFFFFE,0x1|]
    | HCNW -> [|0x0,0x0; 0xFFFFFFFE,0x1; 0x0,0x2; 0x2,0x1; 0x2,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0xFFFFFFFE,0xFFFFFFFF|]
    | HANW -> [|0x0,0x0; 0xFFFFFFFE,0x1; 0xFFFFFFFE,0xFFFFFFFF; 0x0,0xFFFFFFFE; 0x2,0xFFFFFFFF; 0x2,0x1; 0x0,0x2|]

// Identity Hexel
let identity = 
    OG(0x0,0x0)

// Adjacent Hexels
let adjacent 
    (sqn: Sqn)
    (hxo: Hxl) = 
    
    Array.map (fun (a,b) -> 
    let (OG (x,y)) = hxo
    OG(x+a, y+b))(sequence sqn)

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
    | x,y when y >= 0x0 -> 
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
        | None -> (identity,0xFFFFFFFF)
    | _ -> (identity,0xFFFFFFFF)

// Get Hexel from Tuple
let getHxls 
    (hxo : (Hxl*int)[]) = 
    
    hxo
    |> Array.map(fun x 
                    -> fst x)

// Available Adjacent Hexels
let available 
    (sqn : Sqn)
    (hxo : obj)
    (occ : Hxl[]) = 
    
    let hx1 = match hxo with 
                | :? (Hxl*int) as (a,_) -> a
                | :? Hxl as b ->  b
                | _ -> identity
    hx1 
    |> adjacent sqn
    |> Array.except 
        (Array.append occ [|hx1|])
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
        let bl1 = Array.map2 (fun x y -> x=y) [|(0x0)..(Array.length ic1)-(0x1)|] id1   
        let tp1 = Array.zip3 bl1 ic1 hxo  
        tp1 |> Array.map2 (fun d (a,b,c) 
                            -> match a with 
                                | true -> b,d
                                | false -> 
                                        match ((available sqn c oc1) > 0x0) with 
                                        | false -> (fst c),0xFFFFFFFF
                                        | true -> fst(increment sqn c oc1),d) in1
    
    replaceDuplicate sqn hxo inc occ

// Clusters 
let clusters 
    (sqn : Sqn)
    (bas : (Hxl*int)[])
    (occ : Hxl[]) = 
    
    // Output : Base, Hxls, Core, Brdr, Avbl
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

                let occ = Array.concat[|getHxls (Array.concat [|Array.concat acc; inc;Hxl|]);occ|]

                (clsts Hxl occ acc (cnt - 0x1))

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

    let cls = clsts bas occ acc cnt
            |> Array.map(fun x 
                            -> Array.filter(fun (_,z) -> z >= 0) x)
    let cl1 = 
        cls
        |> Array.map(fun x -> getHxls x)
    let bs1 = getHxls bas
    
    // Bounding Hexels
    let cl2 = Array.map (fun x -> Array.tail x) cl1
    let cl3 = 
        cl2
        |> Array.map(fun y 
                        -> Array.partition(fun x 
                                            -> (available sqn x y) > 0)y)
    let bd1 = Array.map(fun x -> fst x) cl3
    let bd2 = Array.map (fun x -> bndSqn sqn x) bd1
    
    // Core Hexels
    let cr1 = Array.map(fun x -> snd x) cl3
    
    let oc1 = Array.concat
                [|
                    occ 
                    (getHxls(Array.concat cls))
                |] 
    
    let cl4 = 
        bd2
        |> Array.map(Array.partition(fun x-> (available sqn x oc1) > 0))
    
    // Available Hexels
    let av1= Array.map(fun x -> fst x) cl4
    
    // Border Hexels
    let br1= Array.map(fun x -> snd x) cl4
    
    {|
        Base = bs1
        Hxls = cl1
        Core = cr1
        Prph = bd2
        Brdr = br1
        Avbl = av1
    |}