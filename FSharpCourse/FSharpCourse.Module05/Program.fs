open System
open FsXaml
open System.Windows
open System.Windows.Controls
open System.Reactive.Linq
open System.Reactive.Concurrency
open System.Threading

type MainWindow = XAML<"MainWindow.xaml">

[<STAThread>]
[<EntryPoint>]
let main _ =     
    Gjallarhorn.Wpf.Platform.install true |> ignore

    let app = Application()
    let win = MainWindow()

    let button1 : Button = win.button1;
    let button2 : Button = win.button2;
    
    button1.Click
    |> Event.merge button2.Click
    |> Event.add (fun a -> MessageBox.Show "Hii" 
                           |> ignore)

    //button1.Click
    //|> Observable.merge button2.Click
    //|> Observable.add (fun a -> MessageBox.Show "Hii"
    //                            |> ignore)

    printfn "Interval"
    let observable = Observable.Interval(TimeSpan.FromSeconds(1.0))

    let subscription = observable.Subscribe (printfn "%d")

    Console.ReadLine()

    subscription.Dispose()

    printfn "Buffer" 
    let observable = Observable.Interval(TimeSpan.FromSeconds(1.0)).Buffer(3)

    let subscription = observable.Subscribe (printfn "%A")

    Console.ReadLine()

    subscription.Dispose()

    printfn "Window"
    let observable = Observable.Interval(TimeSpan.FromSeconds(1.0)).Window(3)

    let subscription = observable.Subscribe (fun obs -> obs.Sum().Subscribe (printfn "%d") |> ignore)

    Console.ReadLine()

    subscription.Dispose()

    printfn "GroupBy"
    let observable = Observable.Interval(TimeSpan.FromSeconds(1.0)).GroupBy(fun x -> x % 3L)

    let printKeyValue key value =
        printfn "%s %d" (String('*', key)) value
        ()
        
    let subscription = observable.Subscribe (fun obs -> obs.Subscribe (printKeyValue (int obs.Key)) |> ignore)

    Console.ReadLine()

    subscription.Dispose()

    let txt : TextBox = win.txt
    let console : TextBlock = win.console

    let observable = Observable.FromEventPattern(txt, "TextChanged").Throttle(TimeSpan.FromSeconds(1.0))

    let subscription = observable.ObserveOn(SynchronizationContext.Current).Subscribe (fun text -> console.Text <-console.Text + Environment.NewLine + txt.Text )

    app.Run win
