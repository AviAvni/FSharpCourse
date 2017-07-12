module Properties

open FsCheck
open FsCheck.Prop

let p1 = forAll (Arb.Default.Int32()) (fun a b -> a + b = b + a)
p1 |> Check.Quick 
p1 |> Check.Verbose
Check.One ({ Config.Verbose with MaxTest = 5000 }, p1)