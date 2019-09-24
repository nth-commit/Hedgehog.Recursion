module RoseTreeTests

open Xunit
open Swensen.Unquote
open Hedgehog.Recursion
open Extensions

let valueTreeOfZeros = RoseTree (0, [RoseTree (0, [])])
let valueTreeOfOnes = RoseTree (1, [RoseTree (1, [])])
let valueTreeOfAs = RoseTree ('a', [RoseTree ('a', [])])

[<Fact>]
let ``map2 over same type`` () =
    let result =
        valueTreeOfOnes
        |> RoseTree.map2 (fun x y ->
            x
            |> Option.defaultValue 0
            |> (+) y)
    test <@ result = RoseTree (1, [RoseTree (2, [])]) @>

[<Fact>]
let ``map2 over different type`` () =
    let result =
        valueTreeOfAs
        |> RoseTree.map2 (fun x y ->
            x
            |> Option.map string
            |> Option.defaultValue ""
            |> (+) (y |> string))
    test <@ result = RoseTree ("a", [RoseTree ("aa", [])]) @>

[<Fact>]
let ``maplevel`` () =
    valueTreeOfOnes
    |> RoseTree.map2 (fun x y ->
        x
        |> Option.defaultValue 1
        |> (+) y)
    |> RoseTree.maplevel (fun i n -> (i, n))
    |> RoseTree.traverseNodes
    |> List.forall (fun (i, n) -> i = n)

[<Fact>]
let ``traverse`` () =
    valueTreeOfOnes
    |> RoseTree.traverse
    |> (=) [
        RoseTree (1, [RoseTree (1, [])])
        RoseTree (1, [])]

[<Fact>]
let ``traverseNodes`` () =
    valueTreeOfOnes
    |> RoseTree.traverseNodes
    |> (=) [1; 1]

[<Fact>]
let ``traverseSiblingNodes`` () =
    RoseTree ("a", [
        RoseTree ("aa", [
            RoseTree.singleton "aaa"
            RoseTree.singleton "aab" ])
        RoseTree ("ab", [
            RoseTree.singleton "aba"
            RoseTree.singleton "abb" ]) ])
    |> RoseTree.traverseSiblingNodes
    |> (=) [["aa"; "ab"]
            ["aaa"; "aab"]
            ["aba"; "abb"]]
