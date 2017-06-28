module Poker

open Cards

type Hand = Card * Card * Card * Card * Card

type PokerRank = 
    | StraightFlush of int
    | FourOfAKind   of int
    | FullHouse     of int * int
    | Flush         of int
    | Straight      of int
    | ThreeOfAKind  of int
    | TwoPair       of int * int * int
    | OnePair       of int * int * int * int
    | HighCard      of int * int * int * int * int

type Winner =
    | Player1 of PokerRank
    | Player2 of PokerRank
    | Tie     of PokerRank

let rankValue card = 
    match card with
    | Ace   -> 14
    | King  -> 13
    | Queen -> 12
    | Jack  -> 11
    | Ten   -> 10
    | Nine  -> 9
    | Eight -> 8
    | Seven -> 7
    | Six   -> 6
    | Five  -> 5
    | Four  -> 4
    | Three -> 3
    | Two   -> 2

let evaluate (hand : Hand) = 
    let (c1, c2, c3, c4, c5) = hand
    let listHand = [| c1; c2; c3; c4; c5 |] 
                   |> Array.sortBy (fun (rank, _) -> -rankValue rank) 
    let ranks = listHand 
                |> Array.map (fun (rank, _) -> rankValue rank) 
    
    let suitGroup = 
        listHand
        |> Seq.groupBy (fun (rank, suit) -> suit)
        |> Seq.map (fun (suit, cards) -> (suit, cards |> Seq.length))
        |> Seq.sortBy (fun (_, cards) -> -cards)
        |> List.ofSeq
    
    let rankGroup = 
        listHand
        |> Seq.groupBy (fun (rank, _) -> rank)
        |> Seq.map (fun (rank, cards) -> (rankValue rank, cards |> Seq.length))
        |> Seq.sortBy (fun (_, cards) -> -cards)
        |> List.ofSeq

    let (|Straight|_|) rankGroup = 
        match rankGroup with
        | (a, 1) :: (b, 1) :: (c, 1) :: (d, 1) :: (e, 1) :: [] 
            when a - 1 = b && a - 2 = c && a - 3 = d && (a - 4 = e || a - 12 = e) -> 
            let high = 
                if a - 12 = e then b
                else a
            Some(high)
        | _ -> None
    
    match suitGroup with
    | (_, 5) :: [] -> 
        match rankGroup with
        | Straight high -> StraightFlush(high)
        | _             -> Flush(ranks.[0])
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
    let compareRank r1 r2 =
        if r1 > r2 then Player1 rank1 
        elif r1 < r2 then Player2 rank2 
        else Tie rank1
    let compareRanks r1 r2 =
        let cmp = 
            r2 
            |> List.zip r1 
            |> List.map (fun (r1, r2) -> r1 - r2) 
            |> List.tryFind (fun r -> r <> 0)
        match cmp with
        | Some x when x > 0 -> Player1 rank1 
        | Some _ -> Player2 rank2
        | None -> Tie rank1
    match rank1, rank2 with
    | StraightFlush r1, StraightFlush r2 -> compareRank r1 r2
    | StraightFlush _, _ -> Player1 rank1
    | _, StraightFlush _ -> Player2 rank2
    | FourOfAKind r1, FourOfAKind r2 -> compareRank r1 r2
    | FourOfAKind _, _ -> Player1 rank1
    | _, FourOfAKind _ -> Player2 rank2
    | FullHouse(r11, r12), FullHouse(r21, r22) -> compareRanks [r11; r12] [r21; r22]
    | FullHouse _, _ -> Player1 rank1
    | _, FullHouse _ -> Player2 rank2
    | Flush r1, Flush r2-> compareRank r1 r2
    | Flush _, _ -> Player1 rank1
    | _, Flush _ -> Player2 rank2
    | Straight r1, Straight r2 -> compareRank r1 r2
    | Straight _, _ -> Player1 rank1
    | _, Straight _ -> Player2 rank2
    | ThreeOfAKind r1, ThreeOfAKind r2  -> compareRank r1 r2
    | ThreeOfAKind _, _ -> Player1 rank1
    | _, ThreeOfAKind _ -> Player2 rank2
    | TwoPair(r11, r12, r13), TwoPair(r21, r22, r23) -> compareRanks [r11; r12; r13] [r21; r22; r23]
    | TwoPair _, _ -> Player1 rank1
    | _, TwoPair _ -> Player2 rank2
    | OnePair(r11, r12, r13, r14), OnePair(r21, r22, r23, r24) -> compareRanks [r11; r12; r13; r14] [r21; r22; r23; r24]
    | OnePair _, _ -> Player1 rank1
    | _, OnePair _ -> Player2 rank2
    | HighCard(r11, r12, r13, r14, r15), HighCard(r21, r22, r23, r24, r25) -> compareRanks [r11; r12; r13; r14; r15] [r21; r22; r23; r24; r25]
