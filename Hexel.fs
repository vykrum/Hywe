module Hexel
// Hexel
type Hxl = { loc : Location.Loc }

let identity = {loc = Location.identity}

let nui (hxc : (int * Hxl)[]) (occ : Hxl[]) =
    let a = hxc |> Array.map(fun (x,y)-> y.loc,x)
    let b = occ |> Array.map (fun x -> x.loc)
    let c = Location.clusters a b
    Array.map(fun a ->(Array.map(fun (x,y) -> {loc=x}))a)c
    
