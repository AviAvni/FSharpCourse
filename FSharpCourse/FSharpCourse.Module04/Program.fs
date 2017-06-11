open System.Threading
open System.Net
open System.IO

let printThenSleepThenPrint = 
    async { 
        printfn "before sleep" 
        do! Async.Sleep 5000 
        printfn "wake up" 
    } 
Async.StartImmediate printThenSleepThenPrint 
printfn "continuing"

let getWebPage (url:string) =
    async { 
        let req = WebRequest.Create url
        let! resp = req.AsyncGetResponse()
        let stream = resp.GetResponseStream()
        let reader = new StreamReader(stream)
        return! Async.AwaitTask (reader.ReadToEndAsync()) }

let capability = new CancellationTokenSource() 
let tasks = 
    Async.Parallel [ getWebPage "http://www.google.com"
                     getWebPage "http://www.bing.com" ]
    |> Async.Ignore

Async.Start (tasks, cancellationToken=capability.Token)
 
capability.Cancel() 

let parallel2 (job1, job2) =
    async {
        let! task1 = Async.StartChild job1
        let! task2 = Async.StartChild job2
        let! res1 = task1
        let! res2 = task2
        return (res1, res2) }
