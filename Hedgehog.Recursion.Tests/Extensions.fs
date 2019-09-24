module Extensions

open Hedgehog.Recursion

module RoseTree =

    let traverseSiblingNodes (tree : RoseTree<'a>) : 'a list list =
        tree
        |> RoseTree.traverse
        |> List.map (RoseTree.children >> List.map RoseTree.node)

    let contains a b =
        let setOfA =
            a
            |> RoseTree.traverseNodes
            |> Set
        b
        |> RoseTree.traverseNodes
        |> List.forall (fun bNode -> Set.contains bNode setOfA)

    let rec merge (xs : List<RoseTree<'a>>) : RoseTree<'a> =
        if xs |> List.map RoseTree.node |> List.distinct |> List.length |> (=) 1
        then
            let x =
                xs
                |> List.head
                |> RoseTree.node
            let xs' =
                xs
                |> List.collect RoseTree.children
                |> List.groupBy RoseTree.node
                |> List.map (snd >> merge)
            RoseTree (x, xs')
        else raise (System.Exception "Can only merge RoseTrees if they have the same root node")

    let areEqual (a : RoseTree<'a>) (b : RoseTree<'a>) =
        let normalize x =
            x |> RoseTree.traverseNodes |> List.sort
        (a |> normalize) = (b |> normalize)

    let pairwise tree =
        tree
        |> RoseTree.traverse
        |> List.collect (fun (RoseTree (x, xs)) ->
            xs
            |> List.map RoseTree.node
            |> List.map (fun x' -> (x, x')))

    let rec size ((RoseTree (_, xs)) : RoseTree<'a>) =
        xs
        |> List.map size
        |> List.sum
        |> (+) 1

    let hasChildren tree =
        tree |> RoseTree.children |> List.isEmpty |> not

    let singleton x = RoseTree (x, List.empty)