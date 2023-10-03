module Parse

let spaceStr =
     "(1/5/Foyer),(2/20/Living),(3/20/Dining),
    (4/20/Staircase),(1.1/10/Study),(3.1/15/Bed-1),
    (3.2/15/Bed-2),(3.3/15/Bed-3),(3.4/15/Kitchen),
    (3.1.1/5/Bath-1),(3.2.1/5/Dress-2),(3.3.1/5/Dress-3),
    (3.3.2/5/Bath-3),(3.4.1/5/Utility),(3.2.1.1/5/Bath-2)"

let spaceSeq (spaceStr:string) = 
    let spaceMap = 
        ((spaceStr.Replace ("\n",""))
            .Replace(" ",""))
            .Split ","
            |> Array.map(fun x -> x.Remove(0,1)) 
            |> Array.map(fun x -> x.Remove(x.Length-1,1))
            |> Array.map (fun x -> x.Split "/") 
            |> Array.map (fun x -> (x[0],(int x[1],x[2]))) 
            |> Array.sortBy (fun (x,y) -> x)
            |> Map.ofArray

    let spcKy01 = 
        spaceMap 
        |> Map.keys 
        |> Array.ofSeq 
        |> Array.groupBy(fun x 
                            -> match (x.Length <= 1) with 
                                |true -> "0"
                                |false -> x.Substring (0, x.LastIndexOf(".")))

    let spcKy02 = 
        spcKy01 
        |> Array.head 
        |> snd 
        |> Array.windowed 2 
        |> Array.map(fun x -> x[0],[|x[1]|])
    
    let spcKy03 = 
        spcKy01 
        |> Array.tail 
        |> Array.partition (fun (x,y) -> x.Length = 1)

    let spcKy04 = 
        (Array.append spcKy02 (fst spcKy03)) 
        |> Array.groupBy (fun (x,y) -> x)
        |> Array.map (fun x -> snd x)
        |> Array.map (fun x 
                        -> (Array.map(fun (y,z)
                                        -> Array.append[|y|] z))x)
        |> Array.map (fun x -> Array.concat x)
        |> Array.map (fun x -> Array.distinct x)
        |> Array.map (fun x -> Array.sort x)

    let spcKy05 = 
        (snd spcKy03)
        |> Array.map (fun (x,y) 
                        -> Array.append [|x|] y)
        |> Array.append spcKy04
        |> Array.sortBy (fun x -> Array.head x)
    
    let spcKy06 = 
        spcKy05 
        |> Array.map(fun x 
                        -> (Array.map (fun y 
                                        -> y, spaceMap 
                                        |> Map.find y))x)
    
    let spcKey = 
        spcKy06 
        |> Array.map (fun z 
                        -> (Array.map (fun (x,y) 
                                        -> x, fst y, snd y))z)
    spcKey