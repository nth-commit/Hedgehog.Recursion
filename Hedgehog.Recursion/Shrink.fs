namespace Hedgehog.Recursion

open FSharpx.Collections

module Shrink =

    type private VerticallyReducibleRoseTree<'a> = VerticallyReducibleRoseTree of RoseTree<'a> * Option<VerticallyReducibleRoseTree<'a>>

    module private VerticallyReducibleRoseTree =

        let singleton a = VerticallyReducibleRoseTree(a, None)

        let rec append (VerticallyReducibleRoseTree (curr, nextOption)) (candidate : VerticallyReducibleRoseTree<'a>) : VerticallyReducibleRoseTree<'a> =
            match nextOption with
            | None -> VerticallyReducibleRoseTree (curr, candidate |> Some)
            | Some next -> VerticallyReducibleRoseTree (curr, append next candidate |> Some)

        let rec toList (VerticallyReducibleRoseTree (curr, next)) =
            curr :: (next |> Option.map toList |> Option.defaultValue [])

    let private removesLeaves tree : Option<RoseTree<'a>> =
        let height = tree |> RoseTree.height
        tree
        |> RoseTree.filterlevel (fun l _ -> l < height)

    let rec private removesLeavesUntil (f : RoseTree<'a> -> bool) (tree : RoseTree<'a>) : Option<VerticallyReducibleRoseTree<'a>> =
        if f tree
        then
            None
        else
            let curr = VerticallyReducibleRoseTree.singleton tree
            match removesLeaves tree with
            | None -> curr |> Some
            | Some tree2 ->
                match removesLeavesUntil f tree2 with
                | None ->
                    let terminating = VerticallyReducibleRoseTree.singleton tree2
                    VerticallyReducibleRoseTree.append curr terminating |> Some
                | Some next -> VerticallyReducibleRoseTree.append curr next |> Some

    let approachingTrivial (tree : RoseTree<'a>) : List<RoseTree<'a>> =
        match removesLeavesUntil (fun _ -> false) tree with
        | Some verticallyReducibleTree ->
            verticallyReducibleTree
            |> VerticallyReducibleRoseTree.toList
            |> List.tail
        | None -> []