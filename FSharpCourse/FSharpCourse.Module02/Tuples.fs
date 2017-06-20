module Tuples

let value = (1, "one")
let fst (x, y) = x
let snd (x, y) = y
let (x, y) = value
let a = fst value
let b = snd value
let (t1, t2, t3) = 1, 2, 3