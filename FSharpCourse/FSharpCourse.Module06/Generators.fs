module Generators

open FsCheck

let g0 = gen { return true }
g0 |> Gen.sample 10 10

let g1 = Gen.oneof [ gen { return true }
                     gen { return false } ]
g1 |> Gen.sample 10 10

let g2 = Gen.frequency [ (2, gen { return true })
                         (1, gen { return false }) ]
g2 |> Gen.sample 10 10

let g3 = Gen.sized (fun s -> Gen.choose (0, s))
g3 |> Gen.sample 10 10

let g4 = Gen.sized (fun s -> if s % 2 = 0 then gen { return true } else gen { return false })
g4 |> Gen.sample 1 10