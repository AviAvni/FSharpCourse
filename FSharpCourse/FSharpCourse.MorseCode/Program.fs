open System
open System.Threading
open System.Threading.Tasks
open System.Reactive.Linq
open System.Reactive.Disposables
open MorseCodeInterpreter

let morseCodeLiveStream () =
    let observable = 
        Observable.Create<char>(
            fun (observer : IObserver<char>) ->
                let disposable = new BooleanDisposable()
                Task.Factory.StartNew(
                    fun () ->
                        let mutable info = Console.ReadKey()
                        while info.Key <> ConsoleKey.Enter && not disposable.IsDisposed do
                            observer.OnNext(info.KeyChar)
                            info <- Console.ReadKey()
                        Environment.Exit(0)) |>ignore
                disposable :> IDisposable)
    observable
        .Publish()
        .RefCount();

[<EntryPoint>]
let main argv = 
    let xss = morseCodeLiveStream ()
    let mutable index = 0
    (translate xss)
        .Subscribe(
            fun xs -> 
                xs.Subscribe(
                    (fun x ->
                         let oldpos = Console.CursorLeft
                         Console.SetCursorPosition(index, 1)
                         Console.Write(x)
                         Console.SetCursorPosition(oldpos, 0)), 
                    fun () -> index <- index + 1) |> ignore) |> ignore
    Thread.Sleep(-1);
    0
