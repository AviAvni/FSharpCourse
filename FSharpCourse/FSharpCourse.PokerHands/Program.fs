open System

type Suit = 
    | Hearts
    | Diamonds
    | Clubs
    | Spades

type CardRank = 
    | Ace
    | King
    | Queen
    | Jack
    | Ten
    | Nine
    | Eight
    | Seven
    | Six
    | Five
    | Four
    | Three
    | Two

type Card = CardRank * Suit

type Winner =
    | Player1
    | Player2
    | Tie

let rank card = 
    match card with
    | Ace -> 14
    | King -> 13
    | Queen -> 12
    | Jack -> 11
    | Ten -> 10
    | Nine -> 9
    | Eight -> 8
    | Seven -> 7
    | Six -> 6
    | Five -> 5
    | Four -> 4
    | Three -> 3
    | Two -> 2

let Deck : Card list = 
    [ for x in [ Hearts; Diamonds; Clubs; Spades ] do
        for y in [ Ace; King; Queen; Jack; Ten; Nine; Eight; Seven; Six; Five; Four; Three; Two ] do
            yield (y, x) ]

type Hand = Card * Card * Card * Card * Card

type Ranks = 
    | StraightFlush of int
    | FourOfAKind   of int
    | FullHouse     of int * int
    | Flush         of int * int * int * int * int
    | Straight      of int
    | ThreeOfAKind  of int
    | TwoPair       of int * int * int
    | OnePair       of int * int * int * int
    | HighCard      of int * int * int * int * int

let evaluate hand = 
    let (c1, c2, c3, c4, c5) = hand
    let listHand = [| c1; c2; c3; c4; c5 |]
    let ranks = listHand |> Array.map (fun c -> fst c |> rank) |> Array.sortBy (fun r -> -r)
    
    let suitGroup = 
        listHand
        |> Seq.groupBy (fun c -> snd c)
        |> Seq.map (fun c -> (fst c, snd c |> Seq.length))
        |> Seq.sortBy (fun g -> snd g)
        |> List.ofSeq
    
    let rankGroup = 
        listHand
        |> Seq.groupBy (fun c -> fst c)
        |> Seq.map (fun c -> (fst c |> rank, snd c |> Seq.length))
        |> Seq.sortBy (fun g -> snd g)
        |> List.ofSeq
    
    let (|Straight|_|) rankGroup = 
        match rankGroup with
        | (a, 1) :: (b, 1) :: (c, 1) :: (d, 1) :: (e, 1) :: [] when a - 1 = b && a - 2 = c && a - 3 = d 
                                                                    && (a - 4 = e || a - 12 = e) -> 
            let high = 
                if a - 12 = e then b
                else a
            Some(high)
        | _ -> None
    
    match suitGroup with
    | (_, 4) :: [] -> 
        match rankGroup with
        | Straight high -> StraightFlush(high)
        | _             -> Flush(ranks.[0], ranks.[1], ranks.[2], ranks.[3], ranks.[4])
    | _ -> 
        match rankGroup with
        | Straight high                              -> Straight(high)
        | (a, 4) :: _                                -> FourOfAKind(a)
        | (a, 3) :: (b, 2) :: _                      -> FullHouse(a, b)
        | (a, 3) :: _                                -> ThreeOfAKind(a)
        | (a, 2) :: (b, 2) :: (c, 1) :: []           -> TwoPair(a, b, c)
        | (a, 2) :: (b, 1) :: (c, 1) :: (d, 1) :: [] -> OnePair(a, b, c, d)
        | _                                          -> HighCard(ranks.[0], ranks.[1], ranks.[2], ranks.[3], ranks.[4])

let compare rank1 rank2 =
    match rank1, rank2 with
    | StraightFlush c1, StraightFlush c2 -> if c1 > c2 then Player1 elif c1 < c2 then Player2 else Tie
    | StraightFlush _, _ -> Player1
    | _, StraightFlush _ -> Player2


[<EntryPoint>]
let main argv =
    let card (s : string) =
        let suit =
            match s.[1] with
            | 'H' -> Hearts
            | 'D' -> Diamonds
            | 'C' -> Clubs
            | 'S' -> Spades
            | _ -> failwith "Invalid suit"

        let rank =
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

        rank, suit

    let hands = Console.ReadLine().Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
    let [| w1; w2; w3; w4; w5|] = hands.[1..5] |> Array.map card
    let whiteScore = evaluate (w1, w2, w3, w4, w5)
    let [| b1; b2; b3; b4; b5|] = hands.[7..11] |> Array.map card
    let blackScore = evaluate (b1, b2, b3, b4, b5)

    printfn "%A" hands
    0 // return an integer exit code
