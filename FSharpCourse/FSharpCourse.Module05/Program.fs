open System
open FsXaml
open System.Windows

type MainWindow = XAML<"MainWindow.xaml">

[<STAThread>]
[<EntryPoint>]
let main _ =     
    Gjallarhorn.Wpf.Platform.install true |> ignore

    let app = Application()
    let win = MainWindow()

    let button1 = win.button1;
    let button2 = win.button2;

    button1.Click
    |> Event.merge button2.Click
    |> Event.add (fun a -> MessageBox.Show "Hii" 
                           |> ignore)

    //button1.Click
    //|> Observable.merge button2.Click
    //|> Observable.add (fun a -> MessageBox.Show "Hii"
    //                            |> ignore)


    app.Run win
