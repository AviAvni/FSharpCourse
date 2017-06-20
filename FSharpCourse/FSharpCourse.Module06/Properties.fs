module Properties

open FsCheck
open FsCheck.Prop

let p1 = forAll (Arb.Default.Int32()) (fun a b -> a + b = b + a)
p1 |> Check.Quick