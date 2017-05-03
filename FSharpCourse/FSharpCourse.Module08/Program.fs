let printerAgent = MailboxProcessor.Start(fun inbox-> 
    // the message processing function
    let rec messageLoop() = async{
        // read a message
        let! msg = inbox.Receive()
        // process a message
        printfn "message is: %s" msg
        // loop to top
        return! messageLoop()  
        }
    // start the loop 
    messageLoop() 
    )

printerAgent.Post "hello" 
printerAgent.Post "hello again" 
printerAgent.Post "hello a third time" 

