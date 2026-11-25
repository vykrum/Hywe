namespace Hywe.Core.Tests

open System
open System.IO
open System.Text.RegularExpressions
open System.Xml.Linq
open Microsoft.FSharp.Reflection
open Xunit
open Hywe.Core

module XmlExamplesTests =

    [<Fact>]
    let ``Hywe.Core.xml exists and all example module-member calls resolve to public symbols`` () =
        let asmDir = Path.GetDirectoryName(typeof<Hexel.Hxl>.Assembly.Location)
        let xmlPath = Path.Combine(asmDir, "Hywe.Core.xml")
        Assert.True(File.Exists(xmlPath), $"Expected XML documentation file at {xmlPath}")

        let doc = XDocument.Load(xmlPath)
        let examples =
            doc.Descendants(XName.Get "member")
            |> Seq.choose (fun m ->
                let nameAttr = m.Attribute(XName.Get "name")
                let ex = m.Element(XName.Get "example")
                if isNull nameAttr || isNull ex then None
                else
                    let code = ex.Element(XName.Get "code")
                    if isNull code then None else Some (nameAttr.Value, code.Value.Trim()))
            |> Seq.toList

        Assert.NotEmpty(examples)

        let coreAssembly = typeof<Hexel.Hxl>.Assembly
        let moduleMembers =
            [ "Hexel"; "Coxel"; "Goxel"; "Lexel"; "Xyxel"; "Nexel"; "Zaxel" ]
            |> List.map (fun modName ->
                let ty = coreAssembly.GetType($"Hywe.Core.{modName}")
                Assert.NotNull(ty)
                let names =
                    ty.GetMembers()
                    |> Array.map (fun m -> m.Name)
                    |> Set.ofArray
                let nested =
                    ty.GetNestedTypes()
                    |> Array.map (fun t -> t.Name)
                    |> Set.ofArray
                let unionCases =
                    ty.GetNestedTypes()
                    |> Array.filter (fun t -> FSharpType.IsUnion(t))
                    |> Array.collect (fun t -> FSharpType.GetUnionCases(t))
                    |> Array.map (fun c -> c.Name)
                    |> Set.ofArray
                modName, Set.unionMany [ names; nested; unionCases ])
            |> Map.ofList

        let callPattern = Regex(@"\b(Hexel|Coxel|Goxel|Lexel|Xyxel|Nexel|Zaxel)\.([A-Za-z_][A-Za-z0-9_]*)\b")

        examples
        |> List.iter (fun (memberName, code) ->
            Assert.False(String.IsNullOrWhiteSpace(code), $"Empty <example><code> for {memberName}")
            for m in callPattern.Matches(code) do
                let modName = m.Groups.[1].Value
                let symbol = m.Groups.[2].Value
                let known = moduleMembers.[modName]
                Assert.True(
                    known.Contains(symbol),
                    $"XML <example> for {memberName} references '{modName}.{symbol}', which was not found on Hywe.Core.{modName}")
        )
