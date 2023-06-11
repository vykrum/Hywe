module Location
type Loc = 
    { 
        x : int
        y : int
        z : int
    }
    
// Identity Location
let identity = 
    {
        x = 0
        y = 0
        z = 0
    }
    
// Adjacent Location
let adjacent 
    (loc: Loc) = 
    Array.map3 (fun a b c ->
        {
            x = a + loc.x 
            y = b + loc.y
            z = c + loc.z
        })
        [|0; 1; -1; -2; -1; 1; 2|]
        [|0; -2; -2; 0; 2; 2; 0|]
        [|0; 0; 0; 0; 0; 0; 0|]
    
// Increment Location
let increment 
    (loc: Loc * int) 
    (occ: Loc[]) = 
    match loc with 
    | x,y when y >= 0 -> 
        let inc = x 
                |> adjacent 
                |> Array.tail 
                |> Array.except occ 
                |> Array.tryHead
        match inc with 
        | Some a -> a, y
        | None -> loc
    | _ -> loc
   
// Get Location from tuple 
let getLocs 
    (loc : (Loc*int)[]) = 
    loc
    |> Array.map(fun x 
                    -> fst x)

// Increment Location
let increments 
    (loc: (Loc*int)[]) 
    (occ: Loc[]) = 
    let inc = 
        Array.scan (fun ac st -> 
        increment st (Array.append[|fst ac|] occ )) 
            loc[0] loc
            |> Array.tail
    // Avoid identical increments
    let bln = Array.map2 (fun x y 
                            -> x=y) (Array.map(fun y 
                                                ->(inc 
                                                |> Array.findIndex(fun x 
                                                                    -> x = y)))inc) 
                                                                    ([|0..(Array.length inc)-1|])
                
    let rpt = Array.zip3 loc inc bln
    rpt |> Array.map (fun x -> match x with 
                                | a,b,true -> b
                                | a,b,false -> increment a (Array.append occ (getLocs inc)))

// Available Adjacent Locations
let available 
    (loc: (Loc*int))
    (occ: Loc[]) = 
    loc 
    |> fst 
    |> adjacent 
    |> Array.except occ 
    |> Array.length

// Cluster Locations
let clusters 
    (loc: (Loc*int)[])
    (occ : Loc[]) = 
    let cnt = 
            loc
            |> Array.map (fun x -> snd x)
            |> Array.max
    let acc = Array.chunkBySize 1 loc
        
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
                    |> Array.distinct
                let rpt = Array.map (fun x 
                                        -> (snd x)-1) loc
                let loc =  
                    acc
                    |> Array.map (fun x
                                    -> Array.filter (fun a 
                                                        -> (available a occ) > 0) x)
                    |> Array.map (fun x 
                                    -> Array.tryHead x)
                    |> Array.map (fun x 
                                    -> match x with
                                        | Some a -> a 
                                        | None -> (identity,-1))                
                    |> Array.map2 (fun x y 
                                    -> fst y,  x) rpt
                let inc = increments loc occ
                            
                let acc = Array.map2  (fun x y
                                        -> Array.append x y) 
                            acc
                            (Array.chunkBySize 1 inc)
                (clsts loc occ acc (cnt-1))
                
    clsts loc occ acc cnt
    |> Array.map(fun x 
                    -> Array.filter(fun (y,z) -> z >= 0) x)
        
   

