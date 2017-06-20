module Records

// declaration
type Person = { Name : string; Age : int; Height : float }
// assignment
let p1       = { Name = "Avi";  Age = 30;  Height = 1.80 }
let p2       = { Person.Name = "Avi";  Age = 30;  Height = 1.80 }
let p3       = { p2 with Age = 18 }

// let getAge (p:Person) : int = p.Age
let getAge p = p.Age

let age = getAge p1