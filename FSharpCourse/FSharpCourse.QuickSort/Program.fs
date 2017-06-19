open System
open System.Diagnostics
open Hopac

let rec QuickSort lst = 
    match lst with
    | [] -> []
    | pivot::rest -> 
        let left, right = rest |> List.partition(fun i -> i < pivot)
        let sleft = QuickSort left 
        let sright = QuickSort right
        sleft@pivot::sright

let QuickSortTailRecursive lst = 
    let rec QuickSortCont l cont =
        match l with
        | [] -> cont []
        | pivot::rest -> 
            let left, right = rest |> List.partition(fun i -> i < pivot)
            QuickSortCont left (fun accLeft -> 
            QuickSortCont right (fun accRight -> cont(accLeft@pivot::accRight)))
    QuickSortCont lst (fun x -> x)

let rec QuickSortParallelAsync lst = 
    async {
        match lst with
        | [] -> return []
        | pivot::rest -> 
            let left, right = rest |> List.partition(fun i -> i < pivot)
            let! sleftChild = QuickSortParallelAsync left |> Async.StartChild
            let! srightChild = QuickSortParallelAsync right |> Async.StartChild
            let! sleft = sleftChild
            let! sright = srightChild
            return sleft@pivot::sright
    }

let rec QuickSortParallelJob lst = 
    job {
        match lst with
        | [] -> return []
        | pivot::rest -> 
            let left, right = rest |> List.partition(fun i -> i < pivot)
            let! (sleft, sright) = Infixes.(<*>) (QuickSortParallelJob left) (QuickSortParallelJob right)
            return sleft@pivot::sright
    }

let Measure s f x =
    let sw = Stopwatch.StartNew()
    let r = f x
    sw.Stop()
    printfn "%s %dms" s sw.ElapsedMilliseconds
    r


let rnd = new Random()
let l = [for i in [1 .. 1000000] -> rnd.Next(1, 10000)]

let qs = Measure "QuickSort" QuickSort l
//let qstr = Measure "QuickSortTailRecursive" QuickSortTailRecursive l
let qspa = Measure "QuickSortParallelAsync" (fun l -> QuickSortParallelAsync l |> Async.RunSynchronously) l
let qspj = Measure "QuickSortParallelAsync" (fun l -> QuickSortParallelJob l |> run) l

//printfn "qs = qsp: %A" (qspj = qsp)