namespace Hedgehog.Recursion

open Hedgehog
open FSharpx.Collections

module Tree =

    let rec private expandTriviallyInternal
        (expander : RoseTree<'a> -> Option<TriviallyShrinkableRoseTree<'a>>)
        (tree : RoseTree<'a>)
        (reducible : Option<TriviallyShrinkableRoseTree<'a>>) =
            match reducible with
            | Some (TriviallyShrinkableRoseTree (nextTree, nextReducible)) ->
                let nextExpansion = expandTriviallyInternal expander nextTree nextReducible
                TriviallyShrinkableRoseTree (nextTree, nextExpansion) |> Some
            | None -> expander tree

    /// Recursively expands the outcomes and creates trivial shrinks
    let rec expandTrivially
        (expander : RoseTree<'a> -> Option<TriviallyShrinkableRoseTree<'a>>)
        ((Node (tree, shrinks)) : Tree<RoseTree<'a>>) =
            let reductions = {
                Trivial =
                    expandTriviallyInternal expander tree None
                Unstructured =
                    shrinks
                    |> LazyList.map (expandTrivially expander) }
            StructuredShrinkingRoseTree (tree, reductions)

module Gen =

    let rec private invertGens (gens : List<Gen<'a>>) : Gen<List<'a>> = gen {
        match gens with
        | [] -> return []
        | x :: xs ->
            let! x' = x
            let! xs' = invertGens xs
            return x' :: xs' }

    let rec private listDistinctByInternal projection length generator results = gen {
        let! curr = generator
        let newResults =
            curr :: results
            |> List.distinctBy projection
        if (newResults |> List.length) < length
        then return! listDistinctByInternal projection length generator newResults
        else return newResults }

    let private listDistinctBy projection range generator = gen {
        let! length = range |> Gen.integral
        return! listDistinctByInternal projection length generator [] }

    let private listDistinct range generator =
        listDistinctBy id range generator

    let private eval seed size generator =
        generator
        |> Gen.toRandom
        |> Random.run seed size
        |> Tree.outcome

    let private buildLevel parentNode nodeGenerator degree =
        nodeGenerator parentNode
        |> listDistinct degree
        |> Gen.noShrink

    let private evalLevel parentNode nodeGenerator degree seed size =
        buildLevel parentNode nodeGenerator degree
        |> eval seed size

    let private joinTrees root trees =
        let tree = RoseTree (root, trees |> List.map (Tree.outcome))
        let shrinks =
            if trees |> List.length > 1
            then
                trees
                |> List.map (Tree.map (fun tree -> RoseTree (root, [tree])))
            else []
        Node (tree, shrinks |> LazyList.ofList)
 
    let rec private buildTree nodeData nodeGenerator height degree seed size : Tree<RoseTree<'a>> =
        if height < 1 then raise (System.ArgumentException "height")
        else if height = 1 then
            Node (RoseTree (nodeData, []), LazyList.empty)
        else
            evalLevel (nodeData |> Some) nodeGenerator degree seed size
            |> List.map (fun childNode -> buildTree childNode nodeGenerator (height - 1) degree seed size)
            |> List.rev
            |> joinTrees nodeData

    let private shrinkTree (reducible : Tree<RoseTree<'a>>) : StructuredShrinkingRoseTree<'a> =
        let shrinksToTrivialOf tree =
            match tree |> Shrink.approachingTrivial with
            | [] -> None
            | approachingTrivial -> TriviallyReducibleRoseTree.ofList approachingTrivial
        Tree.expandTrivially shrinksToTrivialOf reducible

    let tree
        (node : 'a option -> Gen<'a>)
        (degree : Range<int>)
        (height : Range<int>) : Gen<RoseTree<'a>> = gen {
            let! height = Gen.int height |> Gen.noShrink
            let! root = node None
            return! 
                buildTree root node height degree
                |> Random
                |> Random.map (shrinkTree >> StructuredShrinkingRoseTree.toTree)
                |> Gen.ofRandom } 

    let treeOfSize (node : 'a option -> Gen<'a>) (degree : int) (height : int) =
        tree node (Range.singleton degree) (Range.singleton height)

    let private funDefault (f : 'a option -> 'b) (def : 'a) : ('a option -> 'b) =
        fun a ->
            match a with
            | None -> def |> Some |> f
            | Some _ -> a |> f

    let forest
        (node : 'a option -> Gen<'a>)
        (degree : Range<int>)
        (height : Range<int>) : Gen<List<RoseTree<'a>>> = gen {
            let! roots = buildLevel None node degree
            let! h = Gen.int height
            if h = 1
            then
                return
                    roots
                    |> List.map (fun r -> RoseTree (r, []))
            else
                let height' =
                    height
                    |> Range.map (fun h -> max (h - 1) 1)
                return!
                    roots
                    |> List.map (fun x -> tree (funDefault node x) degree height')
                    |> invertGens }