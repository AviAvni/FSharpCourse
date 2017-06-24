open System.Collections.Generic
open System.Diagnostics

let fibonacci n =
    let phi = (1.0 + sqrt 5.0) / 2.0
    let psi =  (1.0 - sqrt 5.0) / 2.0
    let fibn = ((pown phi n) - (pown psi n)) / sqrt 5.0
    fibn

fibonacci 1
fibonacci 2
fibonacci 3
fibonacci 4
fibonacci 5
fibonacci 6
fibonacci 7
fibonacci 8
fibonacci 9
fibonacci 10

let rec fibonacciRecursive n =
    if n = 1 || n = 2 then 1L
    else fibonacciRecursive (n-1) + fibonacciRecursive (n - 2)

fibonacciRecursive 1
fibonacciRecursive 2
fibonacciRecursive 3
fibonacciRecursive 4
fibonacciRecursive 5
fibonacciRecursive 6
fibonacciRecursive 7
fibonacciRecursive 8
fibonacciRecursive 9
fibonacciRecursive 10

let memoize f =
    let cache = Dictionary<_,_>()
    fun x ->
        if cache.ContainsKey(x) then cache.[x]
        else let res = f x
             cache.[x] <- res
             res

let rec memoizedFibonacci = memoize (fun n ->
    if n = 1 || n = 2 then 1L
    else memoizedFibonacci (n-1) + memoizedFibonacci (n - 2))

memoizedFibonacci 1
memoizedFibonacci 2
memoizedFibonacci 3
memoizedFibonacci 4
memoizedFibonacci 5
memoizedFibonacci 6
memoizedFibonacci 7
memoizedFibonacci 8
memoizedFibonacci 9
memoizedFibonacci 10

let fibonacciTailRecursive n =
    let rec loop (n1, n2) i =
        if i < n then loop (n1+n2, n1) (i+1)
        else n1
    loop (0L, 1L) 0

fibonacciTailRecursive 1
fibonacciTailRecursive 2
fibonacciTailRecursive 3
fibonacciTailRecursive 4
fibonacciTailRecursive 5
fibonacciTailRecursive 6
fibonacciTailRecursive 7
fibonacciTailRecursive 8
fibonacciTailRecursive 9
fibonacciTailRecursive 10

let Measure s f x =
    let sw1 = Stopwatch.StartNew()
    let r1 = f x
    sw1.Stop()
    let sw2 = Stopwatch.StartNew()
    let r2 = f x
    sw2.Stop()
    printfn "%s %A %dms %A %dms" s r1 sw1.ElapsedMilliseconds r2 sw2.ElapsedMilliseconds
    ()

Measure "fibonacci" fibonacci 1000
Measure "memoizedFibonacci" memoizedFibonacci 1000
Measure "fibonacciTailRecursive" fibonacciTailRecursive 1000
Measure "fibonacciRecursive" fibonacciRecursive 50