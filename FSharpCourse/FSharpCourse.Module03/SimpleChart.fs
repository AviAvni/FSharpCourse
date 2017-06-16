module SimpleChart

open FSharp.Charting


let list = [1..10]
let squereList = list |> List.map (fun x -> x * x)
Chart.Combine(
   [ Chart.Line(list,Name="list")
     Chart.Line(squereList,Name="squereList") ])
|> Chart.WithLegend true
|> Chart.Show