module SimpleChart

open System
open FSharp.Charting
open System.Reactive.Linq
open FSharp.Charting._ChartStyleExtensions


let list = [1..10]
let squereList = list |> List.map (fun x -> x * x)
Chart.Combine(
   [ Chart.Line(list,Name="list")
     Chart.Line(squereList,Name="squereList") ])
|> Chart.WithLegend true
|> Chart.Show

let obs1 = Observable
            .Interval(TimeSpan.FromMilliseconds(10.0))
            .Select(fun x -> let rad = (float x)*Math.PI/180.0
                             cos rad, sin rad)
            .Take(360)

let obs2 = Observable
            .Interval(TimeSpan.FromMilliseconds(10.0))
            .Select(fun x -> let rad = (float x)*Math.PI/180.0
                             cos (rad*5.0), sin (rad*7.0))
            .Take(360)

Chart.Combine(
    [
        LiveChart.LineIncremental obs1
        LiveChart.LineIncremental obs2
    ])
|> Chart.WithLegend true
|> Chart.WithXAxis(Min = -2.0, Max = 2.0)
|> Chart.WithYAxis(Min = -2.0, Max = 2.0)
|> Chart.Show

let obs3 = Observable
            .Interval(TimeSpan.FromMilliseconds(10.0))
            .Select(fun x -> let rad = (float x)*Math.PI/180.0
                             16.0 * (pown (sin rad) 3), 13.0 * (cos rad) -  5.0 * (cos (2.0 * rad)) -  2.0 * (cos (3.0 * rad)) - cos (4.0 * rad))
            .Take(360)

Chart.Combine(
    [
        LiveChart.LineIncremental obs3
    ])
|> Chart.WithLegend true
|> Chart.WithXAxis(Min = -20.0, Max = 20.0)
|> Chart.WithYAxis(Min = -20.0, Max = 20.0)
|> Chart.Show