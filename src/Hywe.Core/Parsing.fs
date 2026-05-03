module Hywe.Core.Parsing

open System.Text.RegularExpressions

let injectSqn (input: string) (newSqn: string) =
    let regex = Regex(@"\(([^)]*)\)")
    let matches = regex.Matches(input)
    if matches.Count = 0 then input
    else
        let firstMatch = matches.[0]
        let attrs = firstMatch.Groups.[1].Value
        let newAttrs = 
            if attrs.Contains("Q=") then
                Regex.Replace(attrs, "Q=[^/)]*", "Q=" + newSqn)
            else
                attrs + "/Q=" + newSqn
        input.Remove(firstMatch.Index, firstMatch.Length).Insert(firstMatch.Index, "(" + newAttrs + ")")
