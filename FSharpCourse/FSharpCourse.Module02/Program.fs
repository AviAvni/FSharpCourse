let value = (1, "one")
let fst (x, y) = x
let snd (x, y) = y
let (x, y) = value
let a = fst value
let b = snd value
let (t1, t2, t3) = 1, 2, 3

// declaration
type Person = { Name : string; Age : int; Height : float }
// assignment
let p       = { Name = "Avi";  Age = 30;  Height = 1.80 }

// let getAge (p:Person) : int = p.Age
let getAge p = p.Age

let age = getAge p

let list1 = [1; 2; 3; 4]
let list2 = [1..4]
let zip = List.zip list1 list2

type Color = Red | Green | Blue | Black | White
type Option<'a> = Some of 'a | None
type BinaryTree<'a> = 
    | Empty
    | Branch of 'a * BinaryTree<'a> * BinaryTree<'a> // Tuple
type JSON =
    | JSString of string
    | JSNumber of float
    | JSObject of (JSON * JSON) list
    | JSArray of JSON list
    | JSBool of bool
    | JSNull

let rec add item tree =
    match tree with
    | Empty -> Branch(item, Empty, Empty)
    | Branch(i, l, r) -> if item < i then
                             Branch(i, add item l, r)
                         else
                             Branch(i, l, add item r)

let rec addWithGuards item tree =
    match tree with
    | Empty -> Branch(item, Empty, Empty)
    | Branch(i, l, r) when item < i -> Branch(i, add item l, r)
    | Branch(i, l, r) -> Branch(i, l, add item r)

let rec contains item tree =
    match tree with
    | Empty -> false
    | Branch(i, l, r) -> if item = i then true
                         elif item < i then contains item l
                         else contains item r

let tree1 =
    Empty
    |> add 5
    |> add 3
    |> add 7
    |> add 9
    |> add 6
    |> add 2
    |> add 4
    |> add 11
    |> add 5

let q1 = contains 4 tree1
let q2 = contains 10 tree1

[<Measure>]
type m
let tenMeter = 10<m>
let twentyMeter : int<m> = tenMeter * 2

