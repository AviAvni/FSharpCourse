module UnitOfMeasures

open FSharp.Data.UnitSystems.SI

[<Measure>]
type m
let tenMeter = 10<m>
let twentyMeter : int<m> = tenMeter * 2
let twentyMeterSquare : int<m ^ 2> = tenMeter * tenMeter