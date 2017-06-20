let add a b = a + b
add 1 2

let first (x, y) = x
first ("1", 2)

let map (f : 'a -> 'b) (xs : 'a seq) =
    seq { for x in xs do
              yield f x }

let newList = [1..10] 
              |> map (fun x -> x + 1)

let add1 = add 1
let value = add1 2

type Person() =
    do 
        printfn "Person created"

let num = 1
let str = "Hii"
let person = Person()

let rec factorial n =
    if n = 0 then
        1
    else n * factorial (n - 1)

let tailRecFactorial n =
    let rec fact n acc =
        if n = 0 then
            acc
        else
            fact (n - 1) acc * n
    fact n 1