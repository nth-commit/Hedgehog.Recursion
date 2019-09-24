namespace Hedgehog.Recursion

type RoseTree<'a> = | RoseTree of 'a * List<RoseTree<'a>>

module RoseTree =

    let node (RoseTree (x, _)) = x

    let children (RoseTree (_, xs)) = xs

    let rec private traverseForest (forest : List<RoseTree<'a>>) : List<RoseTree<'a>> =
        forest
        |> List.map traverse
        |> List.concat

    and traverse ((RoseTree (_, xs)) as tree : RoseTree<'a>) : List<RoseTree<'a>> =
        tree :: (xs |> traverseForest)

    let private traverseMap f tree =
        tree
        |> traverse
        |> List.map f

    let traverseNodes (tree : RoseTree<'a>) : 'a list =
        traverseMap node tree

    let rec private map2Internal
        (f : 'b option -> 'a -> 'b)
        (parent : 'b option)
        ((RoseTree (x, xs)) : RoseTree<'a>) : RoseTree<'b> =
            let x' = f parent x
            RoseTree (x', xs |> List.map (map2Internal f (x' |> Some)))

    let map2 f tree = map2Internal f None tree

    let rec map (f : 'a -> 'b) ((RoseTree (x, xs)) : RoseTree<'a>) =
        RoseTree (f x, xs |> List.map (map f))

    let rec private maplevelInternal
        (f : int -> 'a -> 'b)
        (level : int)
        ((RoseTree (x, xs)) : RoseTree<'a>) =
            RoseTree (f level x, xs |> List.map (maplevelInternal f (level + 1)))

    let maplevel f tree = maplevelInternal f 1 tree

    let rec private filterInternal
        (f : int -> 'a option -> 'a -> bool)
        (level : int)
        (parent : 'a option)
        ((RoseTree (x, xs)) : RoseTree<'a>) : RoseTree<'a> option =
            if f level parent x
            then
                let xs' =
                    xs
                    |> List.map (filterInternal f (level + 1) (x |> Some))
                    |> List.filter Option.isSome
                    |> List.map Option.get
                RoseTree (x, xs')
                    |> Some
            else
                None

    let filterlevel (f : int -> 'a -> bool) (tree : RoseTree<'a>) =
        filterInternal (fun l _ x -> f l x) 1 None tree

    let filter f tree =
        filterlevel (fun _ x -> f x) tree

    let rec height (RoseTree (_, xs)) =
        if xs |> List.isEmpty
        then 0
        else xs |> List.map height |> List.max
        |> (+) 1
