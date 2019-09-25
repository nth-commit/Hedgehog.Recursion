Hedgehog.Recursion [![Build Status](https://travis-ci.org/nth-commit/Hedgehog.Recursion.svg?branch=master)](https://travis-ci.org/nth-commit/Hedgehog.Recursion)
========

Hedgehog extensions for recursive data-structures:

## Trees

Hedgehog up [rose tree data structures](https://en.wikipedia.org/wiki/Rose_tree). Node generation will always chase uniqueness (via structural equality).

```
val Gen.tree : node:('a option -> Gen<'a>) -> degree:Range<int> -> height:Range<int> -> Gen<RoseTree<'a>>
```

 A shrink always preserves the structure of the tree - the root always remains intact (and shrinking always exhausts to the root node by itself). Shrinks will recursively emit:

  - Half of the tree, if the tree has a breadth greater than 1, and;
  - The tree without its leaf nodes

<img src="https://github.com/nth-commit/Hedgehog.Recursion/raw/master/img/Shrink_2_3.png" />

### Example

```fsharp
open Hedgehog
open Hedgehog.Recursion

type PatriarchalUnion = {
    Man : string
    Wife : string }

let name = Gen.string (Range.linear 1 10) (Gen.char 'a' 'z')

let man fatherName = Gen.choice [
    fatherName |> Gen.constant
    fatherName + "son" |> Gen.constant
    name ]

let child (parents : PatriarchalUnion option) : Gen<PatriarchalUnion> = gen {
    match parents with
    | None -> return { Man = "adam"; Wife = "eve" }
    | Some p ->
        let! man = man p.Man
        let! wife = name
        return { Man = man; Wife = wife } }

let fertilityRate = Range.linear 0 8
let generations = Range.linear 1 10

let patriarchy : Gen<RoseTree<PatriarchalUnion>> = Gen.tree child fertilityRate generations

// For convenience...
let patriarchies : Gen<List<RoseTree<PatriarchalUnion>>> = Gen.forest child fertilityRate generations
```