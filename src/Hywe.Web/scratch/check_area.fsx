let pts = [| (0,0); (30,0); (30,30); (0,30) |]
let area2 = 
    pts |> Array.pairwise |> Array.sumBy (fun ((x1, y1), (x2, y2)) -> 
        int64 (x1 * y2 - x2 * y1))
printfn "Sum: %d" area2
printfn "Area: %d" (abs area2 / 2L)

let pts2 = [| (0,0); (30,0); (30,30); (0,30); (0,0) |]
let area2_2 = 
    pts2 |> Array.pairwise |> Array.sumBy (fun ((x1, y1), (x2, y2)) -> 
        int64 (x1 * y2 - x2 * y1))
printfn "Sum2: %d" area2_2
printfn "Area2: %d" (abs area2_2 / 2L)
