module GenTests

open FSharpx.Collections
open Swensen.Unquote
open Hedgehog
open Hedgehog.Recursion
open Xunit
open Extensions

module List =

    let private cartesianPairwiseSequence<'a> (source : 'a seq) : ('a * 'a) seq = seq {
        for (i, x) in (source |> Seq.indexed) do
            for (_, y) in (source |> Seq.indexed |> Seq.filter (fun (j, _) -> j > i)) do
                yield (x, y) }

    // Generates the cartesian product of a list, excluding unordered duplicates
    //  e.g. [x; y] |> List.cartesianPairwise |> (=) [(x, y)]
    let cartesianPairwise (list : 'a list) = list |> cartesianPairwiseSequence |> Seq.toList

    // Generates the cartesian product of a list
    //  e.g. [x; y] |> List.cartesianOrderedPairwise |> (=) [(x, y); (y, x)]
    let cartesianOrderedPairwise (list : 'a list) =
        (list |> cartesianPairwise) @ (list |> List.rev |> cartesianPairwise)

module Tree =

    let rec toRoseTree (Node (x, xs)) =
        RoseTree (x, xs |> LazyList.map toRoseTree |> LazyList.toList)

    let traverse tree =
        tree
        |> toRoseTree
        |> RoseTree.traverse

    let traverseOutcomes tree =
        tree
        |> toRoseTree
        |> RoseTree.traverseNodes

    let exhaust (tree : Tree<'a>) : List<'a> =
        tree
        |> traverse
        |> List.filter (not << RoseTree.hasChildren)
        |> List.map (RoseTree.node)

module Gen =

    let eIntegral lower upper =
        Range.exponential lower upper
        |> Gen.integral

    let meta (g : Gen<'a>) : Gen<Tree<'a>> =
        let seed = Seed.random ()
        Gen.sized <| fun n ->
            Gen.toRandom g
            |> Random.run seed n
            |> Gen.constant
        |> Gen.noShrink

    let control (f : unit -> 'a) : Gen<'a> =
        Random (fun _ _ ->
            let next = f ()
            Node (next, LazyList.empty))
        |> Gen.ofRandom

    let ofSequence (source : 'a seq) : Gen<'a> =
        let enumerator = source.GetEnumerator()
        control (fun _ ->
            enumerator.MoveNext() |> ignore
            enumerator.Current)

    let roundRobin (candidates : 'a list) : Gen<'a> =
        Seq.initInfinite <| fun i ->
            candidates
            |> List.item (i % (candidates |> List.length))
        |> ofSequence

    let child _ (parent : string option) =
        roundRobin ['a' .. 'z']
        |> Gen.map string
        |> Gen.map (fun x -> (parent |> Option.defaultValue "") + x)

let treeOfSize degree height = gen {
    let node = Gen.child ()
    return! Gen.treeOfSize node degree height }

let tree = gen {
    let node = Gen.child ()
    let degree = Range.exponential 1 6
    let height = Range.exponential 1 6
    return! Gen.tree node degree height }

[<Fact>]
let ``degree = N, height = 2 -> generates tree with N + 1 nodes`` () =
    Property.check <| property {
        let! degree = Gen.eIntegral 2 2
        let! tree = treeOfSize degree 2
        let nodes = tree |> RoseTree.traverseNodes
        test <@ nodes |> List.length |> (=) (degree + 1) @> }

[<Fact>]
let ``degree = 1, height = M -> generates tree with M nodes`` () =
    Property.check <| property {
        let! height = Gen.eIntegral 1 10
        let! tree = treeOfSize 1 height
        let nodes = tree |> RoseTree.traverseNodes
        test <@ nodes |> List.length |> (=) height @> }

[<Fact>]
let ``degree = N, height = M -> generates tree with Σ (m = [0 .. (M - 1)]) (N ^ m) nodes`` () =
    Property.check <| property {
        let! degree = Gen.eIntegral 1 5
        let! height = Gen.eIntegral 1 5
        let! tree = treeOfSize degree height
        let nodes = tree |> RoseTree.traverseNodes
        let expectedLength =
            [0 .. (height - 1)]
            |> List.map (fun level -> pown degree level)
            |> List.sum
        test <@ nodes |> List.length |> (=) expectedLength @> }

[<Fact>]
let ``all nodes of all outcomes have a level encoded equal to their level in the tree`` () =
    Property.check <| property {
        let! tree =
            tree
            |> Gen.map (RoseTree.map String.length)
            |> Gen.meta
        test <@ tree
                |> Tree.traverseOutcomes
                |> List.forall (fun valueTree ->
                    valueTree
                    |> RoseTree.maplevel (fun l n -> (l, n))
                    |> RoseTree.traverseNodes
                    |> List.forall (fun (l, n) -> l = n)) @> }

[<Fact>]
let ``all outcomes contain the root`` () =
    Property.check <| property {
        let! tree =
            tree
            |> Gen.map (RoseTree.map String.length)
            |> Gen.meta
        let violations =
            tree
            |> Tree.traverseOutcomes
            |> List.filter (RoseTree.node >> (<>) 1)
        test <@ violations |> List.isEmpty @> }

let traverseSiblingOutcomes (tree : Tree<RoseTree<'a>>) =
    tree
    |> Tree.toRoseTree
    |> RoseTree.traverseSiblingNodes
    |> List.filter (not << List.isEmpty)

[<Fact>]
let ``sibling outcomes are unique`` () =
    Property.check <| property {
        let! tree = tree |> Gen.meta
        let violations =
            tree
            |> traverseSiblingOutcomes
            |> List.map (fun x -> (x, List.cartesianPairwise x))
            |> List.filter (snd >> List.exists (fun (x, y) -> RoseTree.areEqual x y))
            |> List.map fst
        test <@ violations |> List.isEmpty @> }

[<Fact>]
let ``left-most sibling outcome is also the smallest`` () =
    // Implies that we should go to the most trivial shrink first
    Property.check <| property {
        let! tree = tree |> Gen.meta
        let violations =
            tree
            |> traverseSiblingOutcomes
            |> List.filter (fun xs ->
                let sizes = xs |> List.map RoseTree.size
                let leftMostSize = sizes |> List.head
                sizes
                |> List.tail
                |> List.exists (fun size -> leftMostSize > size))
        test <@ violations |> List.isEmpty @> }

[<Fact>]
let ``child outcomes are smaller than their parent`` () =
    Property.check <| property {
        let! tree = tree |> Gen.meta
        let violations =
            tree
            |> Tree.traverse
            |> List.filter (
                RoseTree.pairwise
                >> List.exists (fun (parent, child) ->
                    RoseTree.size child >= RoseTree.size parent))
        test <@ violations |> List.isEmpty @> }

[<Fact>]
let ``child outcomes have a subset of their parent's nodes`` () =
    Property.check <| property {
        let! tree = tree |> Gen.meta
        let violations =
            tree
            |> Tree.traverse
            |> List.filter (
                RoseTree.pairwise
                >> List.exists (fun (parent, child) -> not <| RoseTree.contains parent child))
        test <@ violations |> List.isEmpty @> }

let isHorizontalShrink (RoseTree (x, xs)) =
    let heightX = x |> RoseTree.height
    xs
    |> List.map (RoseTree.node >> RoseTree.height)
    |> List.exists ((=) heightX)

[<Fact>]
let ``outcomes do not lose nodes whilst they are shrinking horizontally`` () =
    Property.check <| property {
        let! tree = tree |> Gen.meta
        let violations =
            tree
            |> Tree.traverse
            |> List.filter isHorizontalShrink
            |> List.filter (fun (RoseTree (x, xs)) ->
                let x' = xs |> List.map RoseTree.node |> RoseTree.merge
                not <| RoseTree.contains x' x)
        test <@ violations |> List.isEmpty @> }

[<Fact>]
let ``always exhausts to the trivial outcome`` () =
    Property.check <| property {
        let! tree = tree |> Gen.meta
        let exhausted = tree |> Tree.exhaust
        test <@ exhausted |> List.forall ((=) (RoseTree ("a", []))) @> }

let testLengthEquals expectedLength outcomes =
    let x =
        outcomes
        |> List.map (RoseTree.traverseNodes >> List.map string  >> List.sort)
        |> List.sortBy List.length
    test <@ x |> List.length |> (=) expectedLength @>

[<Fact>]
let ``degree = 1, height = 1 -> generates single outcome`` () =
    Property.check <| property {
        let! tree = treeOfSize 1 1 |> Gen.meta
        tree
        |> Tree.traverseOutcomes
        |> testLengthEquals 1 }

[<Fact>]
let ``degree = 1, height = M (M > 1) -> generates M unique outcomes`` () =
    // Outcomes:
    //  - The root node
    //  - The root node and its child
    //  - The root node and its child and grandchild
    //  - etc.
    Property.check <| property {
        let! height = Gen.eIntegral 3 10
        let! tree = treeOfSize 1 height |> Gen.meta
        tree
        |> Tree.traverseOutcomes
        |> List.distinct
        |> testLengthEquals height }

[<Fact>]
let ``degree = N (N > 1), height = 2 -> generates N + 2 unique outcomes`` () =
    // Outcomes:
    //  - The initial outcome (all the nodes) = 1
    //  - The trivial outcome = 1
    //  - The root node with each child, N, individually = N
    Property.check <| property {
        let! degree = Gen.eIntegral 2 10
        let! tree = treeOfSize degree 2 |> Gen.meta
        tree
        |> Tree.traverseOutcomes
        |> List.distinct
        |> testLengthEquals (degree + 2) }

let testLengthsEquals (expectedLengths : int list) (outcomes : List<RoseTree<string>>) =
    let x =
        outcomes
        |> List.groupBy (RoseTree.height)
        |> List.sortBy fst
        |> List.map snd
        |> List.map (List.map (RoseTree.traverseNodes >> List.map string >> List.sort))
    test <@ expectedLengths = (x |> List.map List.length) @>

let calculateOutcomeCountOfLevel degree level =
    if degree = 1
    then 1
    else
        [0 .. level - 1]
        |> List.map (pown degree)
        |> List.sum

let calculateOutcomeCounts degree height =
    [1 .. height]
    |> List.map (calculateOutcomeCountOfLevel degree)

[<Fact>]
let ``degree = N, height = M -> generates expected number of outcomes`` () =
    // Outcomes:
    //  -̗͖̮̞T͚̜̝̯h̖͕̜̹͎̗̳e ̣͉̪̠͈i̥̦͓n̹͕̤̘̣it̯̯̲i̝̩̗a͎̦ͅl̘̫͎̭̭͔̙ ͇̻̻ou͔̲̤t̘͓̹͎̹͇c͔̺̪̖o̯ͅm̰͙͕̲̤͓̺e͚̪̘͓̝ͅ ̮(̭̪̺̲al̖͖̭͔̙͙̭l͔̦ ͈͍͖̞̙̳̦t̟̗̹̹͇ͅh͕̯̯̺̘e̠̭ ̗͎̻n̹o̮̪̰͚d̞̲͇͍̰̫͈e̜̰̼̼̩̣̞s̥͇̭͓̦̖̤)̟ ̬̬͖̤̝=̭̩̤̜̰͉ ͓1̩̗͙̹̠
    //̰̟  -T̼̬̜͔̣̮̫h̟e̺̫̩͔̞͔ ͎̝̟͈̬̼͎nth̭ ̥̠̹̻̩l͎̯e̝͖vḛ̹̞̠͙ͅl͚̞̦̬͓̫̝ ̙o̪̙u͉͔̺̲͙͙̝t̤̯͚c̫͓o̩̳̺̲͎̰̯m͓͕͖̹̱͔͙e,̱ ̤̮̥̖̭̰̰Σ̱̣͖͈̮ ͙̜͚̤̹͙̼(̰͍̯̠m̺̰̞ ̳̩̼̩͓̩=͕̥ ̥̥͇͎͙̪̙[͉̯̮̜̞0̻̞̖̮̯ ̭͔ͅ.̳̯̖̥̜.͔͖.̲͚̲̜̪͍ͅ..͖̜̥.͇̥. ̱̫̣̣̪ͅͅ(̣͍̦̗͓̟̞M̰͓ ̦̞͎̤͍̬ͅ-͕̖̝͍͇̲ ̯̭̹̟̯̰1͉)̬]͕̮͎)̤̬͎͙ (̰̰N͖ͅ&&&. ̼̻^̲͚͖̗̯̜ ͔͕͖̜̪π)̗͔͍  // TODO: fix comment
    Property.check <| property {
        let! degree = Gen.eIntegral 1 6
        let! height = Gen.eIntegral 1 6
        let! tree = treeOfSize degree height |> Gen.meta
        tree
        |> Tree.traverseOutcomes
        |> List.distinct
        |> testLengthsEquals (calculateOutcomeCounts degree height) }
