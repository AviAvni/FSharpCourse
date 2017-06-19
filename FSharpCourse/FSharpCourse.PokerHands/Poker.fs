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

let rank card = 
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

let evaluate hand = 
    let (c1, c2, c3, c4, c5) = hand
    let listHand = [| c1; c2; c3; c4; c5 |]
    let ranks = listHand |> Array.map (fun c -> fst c |> rank) |> Array.sortBy (fun r -> -r)
    
    let suitGroup = 
        listHand
        |> Seq.groupBy (fun c -> snd c)
        |> Seq.map (fun c -> (fst c, snd c |> Seq.length))
        |> Seq.sortBy (fun g -> -snd g)
        |> List.ofSeq
    
    let rankGroup = 
        listHand
        |> Seq.groupBy (fun c -> fst c)
        |> Seq.map (fun c -> (fst c |> rank, snd c |> Seq.length))
        |> Seq.sortBy (fun g -> -snd g)
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
    | (_, 4) :: [] -> 
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
    match rank1, rank2 with
    | StraightFlush r1, StraightFlush r2 when r1 > r2 -> Player1 rank1
    | StraightFlush r1, StraightFlush r2 when r1 < r2 -> Player2 rank2
    | StraightFlush _, _ -> Player1 rank1
    | _, StraightFlush _ -> Player2 rank2
    | FourOfAKind r1, FourOfAKind r2 when r1 > r2 -> Player1 rank1
    | FourOfAKind r1, FourOfAKind r2 when r1 < r2 -> Player2 rank2
    | FourOfAKind _, _ -> Player1 rank1
    | _, FourOfAKind _ -> Player2 rank2
    | FullHouse(r1, _), FullHouse(r2, _) when r1 > r2 -> Player1 rank1
    | FullHouse(r1, _), FullHouse(r2, _) when r1 < r2 -> Player2 rank2
    | FullHouse(_, r1), FullHouse(_, r2) when r1 > r2 -> Player1 rank1
    | FullHouse(_, r1), FullHouse(_, r2) when r1 < r2 -> Player2 rank2
    | FullHouse _, _ -> Player1 rank1
    | _, FullHouse _ -> Player2 rank2
    | Flush r1, Flush r2 when r1 > r2 -> Player1 rank1
    | Flush r1, Flush r2 when r1 < r2 -> Player2 rank2
    | Flush _, _ -> Player1 rank1
    | _, Flush _ -> Player2 rank2
    | Straight r1, Straight r2 when r1 > r2 -> Player1 rank1
    | Straight r1, Straight r2 when r1 < r2 -> Player2 rank2
    | Straight _, _ -> Player1 rank1
    | _, Straight _ -> Player2 rank2
    | ThreeOfAKind r1, ThreeOfAKind r2  when r1 > r2 -> Player1 rank1
    | ThreeOfAKind r1, ThreeOfAKind r2  when r1 < r2 -> Player2 rank2
    | ThreeOfAKind _, _ -> Player1 rank1
    | _, ThreeOfAKind _ -> Player2 rank2
    | TwoPair(r1, _, _), TwoPair(r2, _, _) when r1 > r2 -> Player1 rank1
    | TwoPair(r1, _, _), TwoPair(r2, _, _) when r1 < r2 -> Player2 rank2
    | TwoPair(_, r1, _), TwoPair(_, r2, _) when r1 > r2 -> Player1 rank1
    | TwoPair(_, r1, _), TwoPair(_, r2, _) when r1 < r2 -> Player2 rank2
    | TwoPair(_, _, r1), TwoPair(_, _, r2) when r1 > r2 -> Player1 rank1
    | TwoPair(_, _, r1), TwoPair(_, _, r2) when r1 < r2 -> Player2 rank2
    | TwoPair _, _ -> Player1 rank1
    | _, TwoPair _ -> Player2 rank2
    | OnePair(r1, _, _, _), OnePair(r2, _, _, _) when r1 > r2 -> Player1 rank1
    | OnePair(r1, _, _, _), OnePair(r2, _, _, _) when r1 < r2 -> Player2 rank2
    | OnePair(_, r1, _, _), OnePair(_, r2, _, _) when r1 > r2 -> Player1 rank1
    | OnePair(_, r1, _, _), OnePair(_, r2, _, _) when r1 < r2 -> Player2 rank2
    | OnePair(_, _, r1, _), OnePair(_, _, r2, _) when r1 > r2 -> Player1 rank1
    | OnePair(_, _, r1, _), OnePair(_, _, r2, _) when r1 < r2 -> Player2 rank2
    | OnePair(_, _, _, r1), OnePair(_, _, _, r2) when r1 > r2 -> Player1 rank1
    | OnePair(_, _, _, r1), OnePair(_, _, _, r2) when r1 < r2 -> Player2 rank2
    | OnePair _, _ -> Player1 rank1
    | _, OnePair _ -> Player2 rank2
    | HighCard(r1, _, _, _, _), HighCard(r2, _, _, _, _) when r1 > r2 -> Player1 rank1
    | HighCard(r1, _, _, _, _), HighCard(r2, _, _, _, _) when r1 < r2 -> Player2 rank2
    | HighCard(_, r1, _, _, _), HighCard(_, r2, _, _, _) when r1 > r2 -> Player1 rank1
    | HighCard(_, r1, _, _, _), HighCard(_, r2, _, _, _) when r1 < r2 -> Player2 rank2
    | HighCard(_, _, r1, _, _), HighCard(_, _, r2, _, _) when r1 > r2 -> Player1 rank1
    | HighCard(_, _, r1, _, _), HighCard(_, _, r2, _, _) when r1 < r2 -> Player2 rank2
    | HighCard(_, _, _, r1, _), HighCard(_, _, _, r2, _) when r1 > r2 -> Player1 rank1
    | HighCard(_, _, _, r1, _), HighCard(_, _, _, r2, _) when r1 < r2 -> Player2 rank2
    | HighCard(_, _, _, _, r1), HighCard(_, _, _, _, r2) when r1 > r2 -> Player1 rank1
    | HighCard(_, _, _, _, r1), HighCard(_, _, _, _, r2) when r1 < r2 -> Player2 rank2
    | _ -> Tie rank1
