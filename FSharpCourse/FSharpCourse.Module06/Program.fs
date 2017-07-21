open Expecto

let tests = 
    test "A simple test" { 
        let subject = "Hello World"
        Expect.equal subject "Hello World" "The strings should equal"
    }

let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }

[<Tests>]
let properties = 
    testList "FsCheck samples" 
        [ testProperty "Addition is commutative" <| fun a b -> a + b = b + a
          
          testProperty "Reverse of reverse of a list is the original list" 
          <| fun (xs : list<int>) -> List.rev (List.rev xs) = xs

          testProperty "Multiple paths" 
          <| fun (xs : list<int>) (i : int) -> if i > 0 && xs.Length > i then List.splitAt i xs = (List.take i xs, List.skip i xs) else true
          
          testProperty "Induction" 
          <| fun (xs : list<int>) (i : int) -> (i :: xs).Length = xs.Length + 1

          testProperty "Invariants 1"
          <| fun (xs : list<int>) -> (xs |> List.map id).Length = xs.Length

          testProperty "Invariants 2"
          <| fun (xs : list<int>) -> (xs |> List.filter (fun _ -> true)).Length = xs.Length

          testProperty "Invariants 3 - Idempotance"
          <| fun (xs : list<int>) -> (xs |> List.distinct |> List.distinct) = (xs |> List.distinct)

          testProperty "Invariants 4 - Idempotance"
          <| fun (xs : list<int>) -> (xs |> List.sort |> List.sort) = (xs |> List.sort)

          testProperty "Invariants 4 - Consistency" 
          <| fun (s1 : string) (s2 : string) ->
                if s2 = null then
                    true
                else
                    let file = System.IO.Path.GetTempFileName()
                    System.IO.File.WriteAllText(file, s1)
                    System.IO.File.WriteAllText(file, s2)
                    System.IO.File.ReadAllText(file) = s2

          // you can also override the FsCheck config
          testPropertyWithConfig config "Product is distributive over addition" 
          <| fun a b c -> a * (b + c) = a * b + a * c ]

[<EntryPoint>]
let main args = 
    runTestsWithArgs defaultConfig args tests |> ignore
    runTestsWithArgs defaultConfig args properties |> ignore
    let history = MissionariesAndCanibals.Run ()
    printfn "%A" history
    MissionariesAndCanibalsProperties.``prove statements`` ()
    0
