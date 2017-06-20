module PropertyBaseTestTree

open FsCheck

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