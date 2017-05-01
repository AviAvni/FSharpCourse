open FSharp.Data
open FSharp.Charting
open FSharp.Charting._ChartStyleExtensions
open FSharp.Charting

[<Literal>]
let yahooapis = "https://query.yahooapis.com/v1/public/yql"
[<Literal>]
let apiParameters = "&format=json&env=store://datatables.org/alltableswithkeys"
[<Literal>]
let londonLocationSample = 
    yahooapis + "?q=select woeid from geo.places(1) where text = %22london%22" + apiParameters
[<Literal>]
let londonWeatherSample = 
    yahooapis + "?q=select * from weather.forecast where woeid = 44418" + apiParameters

let locationApi (location : string) = 
    yahooapis + "?q=select woeid from geo.places(1) where text = %22" + location + "%22" + apiParameters
let weatherApi (location : string) = 
    yahooapis + "?q=select * from weather.forecast where woeid = "+ location + apiParameters

type Location = JsonProvider<londonLocationSample>
let location = Location.Load(locationApi "tel aviv").Query.Results.Place.Woeid.ToString()
printfn "Woeid: %s" location

type Weather = JsonProvider<londonWeatherSample> 
let query = Weather.Load(weatherApi location).Query
let units = query.Results.Channel.Units.Temperature
printfn "Temperature %s" units

let forecast = query.Results.Channel.Item.Forecast |> Array.map (fun w -> w.Date, w.Text)
printfn "%A" forecast

//#r "bin/Debug/FsharpCourse.Module03.ChessTypeProvider.dll"

type game = ChessGame.Game.Start.``♙D2D4``.``♟C7C5``.``♙E2E3``.``♟C5D4``.``♙E3D4``
let game = new game()
printfn "%s" (game.ToString())

let wb = WorldBankData.GetDataContext()

let birthRate = wb.Countries.Israel.Indicators.``Birth rate, crude (per 1,000 people)``.Values
let mobileSubscription = wb.Countries.Israel.Indicators.``Mobile cellular subscriptions (per 100 people)``.Values

//#load "../packages/FSharp.Charting.0.90.14/FSharp.Charting.fsx"
Chart.Combine(
   [ Chart.Line(birthRate,Name="Birth rate")
     Chart.Line(mobileSubscription,Name="Mobile subscriptions") ])
|> Chart.WithLegend true
|> Chart.Show