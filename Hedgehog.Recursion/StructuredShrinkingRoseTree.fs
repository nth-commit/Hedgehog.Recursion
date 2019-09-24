namespace Hedgehog.Recursion

open Hedgehog
open FSharpx.Collections

type TriviallyShrinkableRoseTree<'a> =
    TriviallyShrinkableRoseTree of RoseTree<'a> * Option<TriviallyShrinkableRoseTree<'a>>

module TriviallyReducibleRoseTree =

    let rec ofList list =
        match list with
        | x :: xs -> TriviallyShrinkableRoseTree (x, ofList xs) |> Some
        | [] -> None

type StructuredShrinkingRoseTree<'a> =
    StructuredShrinkingRoseTree of RoseTree<'a> * RoseTreeShrinkStructure<'a>

and RoseTreeShrinkStructure<'a> = {
    Trivial : Option<TriviallyShrinkableRoseTree<'a>>
    Unstructured: LazyList<StructuredShrinkingRoseTree<'a>> }

module StructuredShrinkingRoseTree =

    let private ofTrivial (TriviallyShrinkableRoseTree (tree, reduction))  =
        StructuredShrinkingRoseTree (tree, { Trivial = reduction; Unstructured = LazyList.empty })

    let private destructure (reductions : RoseTreeShrinkStructure<'a>) : LazyList<StructuredShrinkingRoseTree<'a>> =
        [   reductions.Trivial |> Option.map ofTrivial |> Option.toList |> LazyList.ofList
            reductions.Unstructured ]
        |> LazyList.ofList
        |> LazyList.concat

    let rec toTree (StructuredShrinkingRoseTree (tree, reductions)) =
        Node (tree, destructure reductions |> LazyList.map toTree)
