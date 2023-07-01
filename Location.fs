module Location

type Loc = 
    { 
        x : int
        y : int
        z : int
    }

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
    | EECW -> [|0,0,0; 2,0,0; 1,-2,0; -1,-2,0; -2,0,0; -1,2,0; 1,2,0|]
    | EECC -> [|0,0,0; 2,0,0; 1,2,0; -1,2,0; -2,0,0; -1,-2,0; 1,-2,0|]
    | SECW -> [|0,0,0; 1,-2,0; -1,-2,0; -2,0,0; -1,2,0; 1,2,0; 2,0,0|]
    | SECC -> [|0,0,0; 1,-2,0; 2,0,0; 1,2,0; -1,2,0; -2,0,0; -1,-2,0|]
    | SWCW -> [|0,0,0; -1,-2,0; -2,0,0; -1,2,0; 1,2,0; 2,0,0; 1,-2,0|]
    | SWCC -> [|0,0,0; -1,-2,0; 1,-2,0; 2,0,0; 1,2,0; -1,2,0; -2,0,0|]
    | WWCW -> [|0,0,0; -2,0,0; -1,2,0; 1,2,0; 2,0,0; 1,-2,0; -1,-2,0|]
    | WWCC -> [|0,0,0; -2,0,0; -1,-2,0; 1,-2,0; 2,0,0; 1,2,0; -1,2,0|]
    | NWCW -> [|0,0,0; -1,2,0; 1,2,0; 2,0,0; 1,-2,0; -1,-2,0; -2,0,0|]
    | NWCC -> [|0,0,0; -1,2,0; -2,0,0; -1,-2,0; 1,-2,0; 2,0,0; 1,2,0|]
    | NECW -> [|0,0,0; 1,2,0; 2,0,0; 1,-2,0; -1,-2,0; -2,0,0; -1,2,0|]
    | NECC -> [|0,0,0; 1,2,0; -1,2,0; -2,0,0; -1,-2,0; 1,-2,0; 2,0,0|]

// Identity Location
let identity = 
    {
        x = 0
        y = 0
        z = 0
    }

// Adjacent Location
let adjacent 
    (sqn: Sqn)
    (loc: Loc) = 
    Array.map (fun (a,b,c) ->
            {
                x = a + loc.x 
                y = b + loc.y
                z = c + loc.z
            })
            (sequence sqn)

// Increment Location
let increment 
    (sqn : Sqn)
    (loc : Loc * int) 
    (occ : Loc[]) = 
    let occ = Array.concat [|occ; [|(fst loc)|]; [|identity|]|]
    match loc with 
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

// Get Location from tuple 
let getLocs 
    (loc : (Loc*int)[]) = 
    loc
    |> Array.map(fun x 
                    -> fst x)

// Available Adjacent Locations
let available 
    (sqn : Sqn)
    (loc : (Loc*int))
    (occ : Loc[]) = 
    loc 
    |> fst 
    |> adjacent sqn
    |> Array.except (Array.append occ [|(fst loc)|] )
    |> Array.length

// Increment Locations
let increments 
    (sqn : Sqn)
    (loc : (Loc*int)[]) 
    (occ : Loc[]) = 
    let occ = Array.append occ (getLocs loc)
    let inc = 
        Array.scan (fun ac st -> 
        let occ = (Array.concat [|occ;[|fst st|];[|fst ac|];[|identity|]|] )
        increment sqn st (Array.append[|fst ac|] occ )) 
            loc[0] loc
            |> Array.tail
    
    let replaceDuplicate 
        (sqn : Sqn)
        (loc : (Loc*int)[]) 
        (inc : (Loc*int)[]) 
        (occ : Loc[]) =
        let in1 = Array.map (fun x -> snd x)inc
        let lc1 = getLocs loc 
        let ic1 = getLocs inc 
        let oc1 = Array.concat[|occ;lc1;ic1|]
        let id1 = Array.map(fun y -> Array.findIndex (fun x -> x = y)ic1)ic1
        let bl1 = Array.map2 (fun x y -> x=y) [|0..(Array.length ic1)-1|] id1   
        let tp1 = Array.zip3 bl1 ic1 loc  
        tp1 |> Array.map2 (fun d (a,b,c) 
                            -> match a with 
                                | true -> b,d
                                | false -> 
                                        match ((available sqn c oc1)>0) with 
                                        | false -> (fst c),-1
                                        | true -> fst(increment sqn c oc1),d) in1

    replaceDuplicate sqn loc inc occ

// Cluster Locations
let clusters 
    (sqn : Sqn)
    (loc : (Loc*int)[])
    (occ : Loc[]) = 
    
    let cnt = 
            loc
            |> Array.map (fun x -> snd x)
            |> Array.max
    let acc = Array.chunkBySize 1 loc
    let occ = Array.append occ (getLocs loc)  
    
    let rec clsts 
        (loc: (Loc*int)[])
        (occ : Loc[])
        (acc:(Loc*int)[][])
        (cnt : int) = 
        match cnt with 
        | c when c < 1 -> acc
        | _ -> 
                let occ = 
                    acc 
                    |> Array.concat 
                    |> getLocs
                    |> Array.append occ
                    |> Array.append (getLocs loc)
                    |> Array.append [|identity|]
                    |> Array.distinct

                let rpt = Array.map (fun x 
                                        -> (snd x)-1) loc
                let loc =  
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
                
                let inc = increments sqn loc occ
                
                            
                let acc = Array.map2  (fun x y
                                        -> Array.append x y) 
                            acc
                            (Array.chunkBySize 1 inc)

                let occ = Array.concat[|getLocs (Array.concat [|Array.concat acc; inc;loc|]);occ|]

                (clsts loc occ acc (cnt-1))
                
    let cls = clsts loc occ acc cnt
            |> Array.map(fun x 
                            -> Array.filter(fun (_,z) -> z >= 0) x)
    cls

