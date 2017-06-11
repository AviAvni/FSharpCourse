open Expecto
open FsCheck
open FsCheck.Prop

let tests = 
    test "A simple test" { 
        let subject = "Hello World"
        Expect.equal subject "Hello World" "The strings should equal"
    }

let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }

let properties = 
    testList "FsCheck samples" 
        [ testProperty "Addition is commutative" <| fun a b -> a + b = b + a
          
          testProperty "Reverse of reverse of a list is the original list" 
          <| fun (xs : list<int>) -> List.rev (List.rev xs) = xs
          
          // you can also override the FsCheck config
          testPropertyWithConfig config "Product is distributive over addition" 
          <| fun a b c -> a * (b + c) = a * b + a * c ]

let p1 = forAll (Arb.Default.Int32()) (fun a b -> a + b = b + a)
let p2 = Gen.oneof [ gen { return true }
                     gen { return false } ]
let p3 = Gen.frequency [ (2, gen { return true })
                         (1, gen { return false }) ]
let p4 = Gen.sized <| fun s -> Gen.choose (0, s)

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

[<EntryPoint>]
let main args = 
    runTestsWithArgs defaultConfig args tests |> ignore
    runTests defaultConfig properties
