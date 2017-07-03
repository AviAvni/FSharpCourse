module AsyncAndParallel

let parallel2 (job1, job2) =
    async {
        let! task1 = Async.StartChild job1
        let! task2 = Async.StartChild job2
        let! res1 = task1
        let! res2 = task2
        return (res1, res2) }

let printThenSleepThenPrint x = 
    async { 
        printfn "before sleep %s" x
        do! Async.Sleep 3000 
        printfn "wake up %s" x
    } 

parallel2 (printThenSleepThenPrint "A", printThenSleepThenPrint "B") 
|> Async.Ignore 
|> Async.Start