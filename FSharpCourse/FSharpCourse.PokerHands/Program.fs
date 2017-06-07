open System
open Cards
open Poker

let parseCard (s : string) =
    let parseSuit =
        match s.[1] with
        | 'H' -> Hearts
        | 'D' -> Diamonds
        | 'C' -> Clubs
        | 'S' -> Spades
        | _ -> failwith "Invalid suit"

    let parseRank =
        match s.[0] with
        | 'A' -> Ace
        | 'K' -> King
        | 'Q' -> Queen
        | 'J' -> Jack
        | 'T' -> Ten
        | '9' -> Nine
        | '8' -> Eight
        | '7' -> Seven
        | '6' -> Six
        | '5' -> Five
        | '4' -> Four
        | '3' -> Three
        | '2' -> Two
        | _ -> failwith "Invalid rank"

    parseRank, parseSuit

[<EntryPoint>]
let main argv =
    let hands = Console.ReadLine().Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
    let [| w1; w2; w3; w4; w5|] = hands.[1..5] |> Array.map parseCard
    let blackScore = evaluate (w1, w2, w3, w4, w5)
    let [| b1; b2; b3; b4; b5|] = hands.[7..11] |> Array.map parseCard
    let whiteScore = evaluate (b1, b2, b3, b4, b5)
    match compare blackScore whiteScore with
    | Player1 r -> printfn "Black win %A" r
    | Player2 r -> printfn "White win %A" r
    | Tie r -> printfn "Tie %A" r

    0 // return an integer exit code
