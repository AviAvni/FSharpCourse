module PropertyBaseTestTree

open FsCheck
open FsCheck.Prop

type Tree = 
    | Leaf of int
    | Branch of Tree * Tree

let tree = 
    let rec treeSub s = 
        match s with
        | 0 -> Gen.map Leaf Arb.generate<int>
        | n when n > 0 -> 
            let subtree = treeSub (n / 2)
            let createBranch x y = Branch(x, y)
            Gen.oneof [ Gen.map Leaf Arb.generate<int>
                        Gen.map2 createBranch subtree subtree ]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized treeSub

tree |> Gen.sample 10 10

type MyGenerators =
  static member Tree() =
      {new Arbitrary<Tree>() with
          override x.Generator = tree
          override x.Shrinker t = Seq.empty }
Arb.register<MyGenerators>()

let rec mapTree f t =
    match t with
    | Leaf x -> Leaf(f x)
    | Branch(t1, t2) -> Branch(mapTree f t1, mapTree f t2)

let add a b = a + b
let p1 = forAll (Arb.from<Tree>) (fun t1 -> let mapAddOneTwice = t1 |> mapTree (add 1) |> mapTree (add 1)
                                            let mapAddTwo = t1 |> mapTree (add 2)
                                            mapAddOneTwice = mapAddTwo)
p1 |> Check.Quick