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

module Random =

    let rec concat (list : List<Random<'a>>) : Random<List<'a>> = random {
        match list with
        | [] -> return []
        | r :: rs ->
            let! r' = r
            let! rs'= concat rs
            return r' :: rs' }

module Gen =

    let rec private concat (gens : List<Gen<'a>>) : Gen<List<'a>> = gen {
        match gens with
        | [] -> return []
        | x :: xs ->
            let! x' = x
            let! xs' = concat xs
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

    let private levelOfNodes parentNode nodeGenerator degree : Gen<List<'a>> =
        nodeGenerator parentNode
        |> listDistinct degree
        |> Gen.noShrink

    let private joinTrees root trees =
        let tree = RoseTree (root, trees |> List.map (Tree.outcome))
        let shrinks =
            if trees |> List.length > 1
            then
                trees
                |> List.map (Tree.map (fun tree -> RoseTree (root, [tree])))
            else []
        Node (tree, shrinks |> LazyList.ofList)
 
    let rec private buildTree nodeGenerator degree height node : Random<Tree<RoseTree<'a>>> =
        if height < 1 then raise (System.ArgumentException "height")
        else if height = 1 then
            Node (RoseTree (node, []), LazyList.empty)
            |> Random.constant
        else
            let nodes =
                levelOfNodes (node |> Some) nodeGenerator degree
                |> Gen.toRandom
                |> Random.map Tree.outcome
            Random.bind nodes (
                List.map (buildTree nodeGenerator degree (height - 1))
                >> List.rev // Don't know why. Doesn't matter in real life but makes tests more digestible.
                >> Random.concat
                >> Random.map (joinTrees node))

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
                buildTree node degree height root
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
            let! roots = levelOfNodes None node degree
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
                    |> concat }

    let forestOfSize (node : 'a option -> Gen<'a>) (degree : int) (height : int) =
        forest node (Range.singleton degree) (Range.singleton height)
