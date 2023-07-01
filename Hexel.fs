module Hexel

open Location

// Hexel
type Hxl = { loc : Loc }

let identity = {loc = identity}

let nui (hxc : (int * Hxl)[]) (occ : Hxl[]) =
    let a = hxc |> Array.map(fun (x,y)-> y.loc,x)
    let b = occ |> Array.map (fun x -> x.loc)
    let c = clusters SECW a b
    Array.map(fun a ->(Array.map(fun (x,y) -> {loc=x}))a)c
    
