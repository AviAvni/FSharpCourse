module Lists

let list1 = [1; 2; 3; 4]
let list2 = [1..100]
let list3 = list1 |> List.map (fun x -> [1..x*2])
let list4 = list2 |> List.filter (fun x -> x % 2 = 0)
let list5 = list3 |> List.collect id
let list6 = List.zip [1; 2; 3; 4] ["A"; "B"; "C"; "D"]
let list7 = list2 |> List.fold (+) 0
let list8 = list2 |> List.reduce (+)

let list9 =  [ 1; 2; 3 ]
let list10 = [ 4; 5; 6 ]
let list11 = [ for x in list9 do for y in list10 -> x*y ]