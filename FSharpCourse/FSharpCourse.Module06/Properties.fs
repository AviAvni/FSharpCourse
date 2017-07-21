module Properties

open FsCheck
open FsCheck.Prop

let add a b = a + b
let p1 = forAll (Arb.Default.Int32()) (fun a b -> add a b = add b a)
let p2 = forAll (Arb.Default.Int32()) (fun a -> add a 0 = a)
p1 |> Check.Quick 
p1 |> Check.Verbose
Check.One ({ Config.Verbose with MaxTest = 5000 }, p1)
Check.Quick p2

let addStrInt a (b : int) = a + (string b)
addStrInt "One" 1

let arb = Arb.from<string * int>
let p3 = forAll arb (fun (a,b) -> let res = addStrInt a b
                                  System.Char.IsDigit(res.[res.Length - 1]))
Check.Verbose p3