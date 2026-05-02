
open System
open System.Collections.Generic

type Sqn =  
    | VRCWEE | VRCCEE | VRCWSE | VRCCSE | VRCWSW | VRCCSW | VRCWWW | VRCCWW | VRCWNW | VRCCNW | VRCWNE | VRCCNE 

let sequence (sqn:Sqn) =  
    match sqn with 
    | VRCWEE -> [| 0,0;  2, 0;  1,-2; -1,-2; -2, 0; -1, 2;  1, 2|]
    | VRCCEE -> [| 0,0;  2, 0;  1, 2; -1, 2; -2, 0; -1,-2;  1,-2|]
    | VRCWSE -> [| 0,0;  1,-2; -1,-2; -2, 0; -1, 2;  1, 2;  2, 0|]
    | VRCCSE -> [| 0,0;  1,-2;  2, 0;  1, 2; -1, 2; -2, 0; -1,-2|]
    | VRCWSW -> [| 0,0; -1,-2; -2, 0; -1, 2;  1, 2;  2, 0;  1,-2|]
    | VRCCSW -> [| 0,0; -1,-2;  1,-2;  2, 0;  1, 2; -1, 2; -2, 0|]
    | VRCWWW -> [| 0,0; -2, 0; -1, 2;  1, 2;  2, 0;  1,-2; -1,-2|]
    | VRCCWW -> [| 0,0; -2, 0; -1,-2;  1,-2;  2, 0;  1, 2; -1, 2|]
    | VRCWNW -> [| 0,0; -1, 2;  1, 2;  2, 0;  1,-2; -1,-2; -2, 0|]
    | VRCCNW -> [| 0,0; -1, 2; -2, 0; -1,-2;  1,-2;  2, 0;  1, 2|]
    | VRCWNE -> [| 0,0;  1, 2;  2, 0;  1,-2; -1,-2; -2, 0; -1, 2|]
    | VRCCNE -> [| 0,0;  1, 2; -1, 2; -2, 0; -1,-2;  1,-2;  2, 0|]

let adjacent (sqn : Sqn) (x, y) =
    (sequence sqn) |> Array.map (fun (a, b) -> (x + a, y + b))

let availableSet (sqn : Sqn) (x, y) (occ : HashSet<int * int>) =
    let adj = adjacent sqn (x, y)
    let mutable count = 0
    for i = 1 to adj.Length - 1 do
        if not (occ.Contains(adj.[i])) then count <- count + 1
    count

let incrementSet (sqn : Sqn) (x, y) (occ : HashSet<int * int>) = 
    let adj = adjacent sqn (x, y)
    let inc1 = ResizeArray<int * int>()
    for i = 1 to adj.Length - 1 do
        let n = adj.[i]
        if not (occ.Contains(n)) then inc1.Add(n)
    
    if inc1.Count >= 2 then
        let head = inc1.[0]
        let next = inc1.[1]
        let adjNext = adjacent sqn next
        let mutable isAdj = false
        for k = 0 to adjNext.Length - 1 do 
            if adjNext.[k] = head then isAdj <- true
        if isAdj then head else inc1.[inc1.Count - 1]
    elif inc1.Count = 1 then inc1.[0]
    else (0, 0) // Should not happen in this simplified logic if availableSet > 0

let toBase36 (value: int64) =
    let chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let rec convert v acc =
        if v = 0L then if acc = "" then "0" else acc
        else convert (v / 36L) (string chars.[int (v % 36L)] + acc)
    let prefix = if value < 0L then "-" else ""
    prefix + convert (abs value) ""

let generatePoints (sqn: Sqn) (count: int) =
    let acc = List<int * int>()
    let occ = HashSet<int * int>()
    
    let start = (0, 0)
    acc.Add(start)
    occ.Add(start) |> ignore
    
    for i = 1 to count - 1 do
        let mutable foundPoint = None
        let mutable j = 0
        while j < acc.Count && foundPoint.IsNone do
            let p = acc.[j]
            if (availableSet sqn p occ) > 0 then
                foundPoint <- Some p
            j <- j + 1
        
        match foundPoint with
        | Some p ->
            let next = incrementSet sqn p occ
            acc.Add(next)
            occ.Add(next) |> ignore
        | None -> ()
    
    acc.ToArray()

let sqn = VRCCNE
let points = generatePoints sqn 72
let pointsStrings = points |> Array.map (fun (x, y) -> sprintf "%s.%s" (toBase36 (int64 x)) (toBase36 (int64 y)))
printfn "%s" (String.concat " " pointsStrings)
