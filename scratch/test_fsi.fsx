#r "../src/Hywe.Core/bin/Debug/net8.0/Hywe.Core.dll"
open Hywe.Core

let stacked = "L0(Q=VRCCNE/L=0/W=30/H=30/X=1/E=0/B=0/O=/I=)(1/75/Lobby)(1.1/88/Retail)(1.2/54/Toilets)(1.3/67/Retail)(1.4/94/Retail)" +
              "L1(Q=VRCCNE/L=3/E=1/B=1)(1/75/Lobby)(1.1/43/Office)(1.2/123/Office)(1.2.1/34/Toilets)(1.3/52/Office)" + 
              "L2(Q=VRCCNE/L=6/E=1/B=1/T=5)(1/75/Lobby)(1.1/99/Suite)"

let res0 = Lexel.injectSqn stacked 0 "VRCWEE"
let res1 = Lexel.injectSqn res0 1 "VRCWEE"
let res2 = Lexel.injectSqn res1 2 "VRCWEE"

printfn "Original: %s\n" stacked
printfn "After injecting all to VRCWEE: %s" res2
