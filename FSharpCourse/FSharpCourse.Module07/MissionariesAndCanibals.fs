module MissionariesAndCanibals

// A state-transition system is a 4-tuple Σ=(S, A, E, γ)
let nM = 3

// S = {s1, s2, …} is a finite or recursively enumerable set of states
type Characters =
    | Nobody
    | Missionaries of int
    | Canibals of int
    | MissionariesAndCanibals of int * int
    override x.ToString() = 
        match x with 
        | Nobody                        -> "nobody"
        | Missionaries(1)               -> "missionary"
        | Missionaries(m)               -> "missionaries"
        | Canibals(1)                   -> "canibal"
        | Canibals(m)                   -> "canibals"
        | MissionariesAndCanibals(1, 1) -> "missionary and canibal"
        | MissionariesAndCanibals(1, c) -> "missionary and canibals"
        | MissionariesAndCanibals(m, 1) -> "missionaries and canibal"
        | MissionariesAndCanibals(m, c) -> "missionaries and canibals"

type RiverBank =
    | Left
    | Right
        override x.ToString() = 
            match x with 
            | Left  -> "left bank"
            | Right -> "right bank"

let private isSafe(characters: Characters) =
    match characters with 
    | Nobody                        -> true
    | Missionaries(_)               -> true
    | Canibals(_)                   -> true
    | MissionariesAndCanibals(m, c) -> m >= c

type State(bank: RiverBank, characters: Characters) =
    member x.Bank = bank
    member x.Characters = characters

    member x.IsStateAllowed() = isSafe(characters)

    member x.OppositeBank() =
        let oppositeBank =
            match bank with
            | Left -> Right
            | Right -> Left

        let oppositeBankCharacters = 
            match characters with
            | Nobody                        -> MissionariesAndCanibals(nM, nM)
            | Missionaries(m)
                when m < nM                 -> MissionariesAndCanibals(nM - m, nM)
            | Missionaries(m)
                when m = nM                 -> Canibals(nM)
            | Canibals(c)
                when c < nM                 -> MissionariesAndCanibals(nM, nM - c)
            | Canibals(c)
                when c = nM                 -> Missionaries(nM)
            | MissionariesAndCanibals(m, c)
                when m = nM && c = nM       -> Nobody
            | MissionariesAndCanibals(m, c)
                when m = nM && c < nM       -> Canibals(nM - c)
            | MissionariesAndCanibals(m, c)
                when m < nM && c = nM       -> Missionaries(nM - m)
            | MissionariesAndCanibals(m, c)
                when m < nM && c < nM       -> MissionariesAndCanibals(nM - m, nM - c)
            | _                             -> raise (System.ArgumentException("Invalid state"))

        new State(oppositeBank, oppositeBankCharacters)

    override x.ToString() = 
        sprintf "%O with %O" bank characters

    override x.Equals(yobj) =
        match yobj with
        | :? State as y -> (x.Bank = y.Bank) && (x.Characters = y.Characters)
        | _ -> false

    override x.GetHashCode() = (hash x.Bank) ^^^ (hash x.Characters)

    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? State as y -> compare (x.Bank, x.Characters) (y.Bank, y.Characters) 
            | _ -> invalidArg "yobj" "cannot compare values of different types"

let InitialState = new State(Left, MissionariesAndCanibals(nM, nM))

// A = {a1, a2, …} is a finite or recursively enumerable set of actions
type BoatTransfer =
    | Transfer of Characters

let BoatTransfers =
    [
        Transfer(Canibals(1));
        Transfer(Canibals(2));
        Transfer(Missionaries(1));
        Transfer(Missionaries(2));
        Transfer(MissionariesAndCanibals(1, 1));
    ]

let private convert(characters: Characters) =
    match characters with 
    | Nobody                            -> (0, 0)
    | Missionaries(m)                   -> (m, 0)
    | Canibals(c)                       -> (0, c)
    | MissionariesAndCanibals(m, c)     -> (m, c)

let private convertBack(m: int, c: int) =
    match (m, c) with
    | (m, 0) when m > 0                 -> Missionaries(m)
    | (0, c) when c > 0                 -> Canibals(c)
    | (m, c) when m > 0 && c > 0        -> MissionariesAndCanibals(m, c)
    | _                                 -> Nobody

let private sub(characters: Characters, move: BoatTransfer) =
    let (sm, sc) = convert(characters)
    let (tm, tc) = match move with | Transfer(characters) -> convert(characters)

    convertBack(sm - tm, sc - tc)

let private add(characters: Characters, move: BoatTransfer) =
    let (sm, sc) = convert(characters)
    let (tm, tc) = match move with | Transfer(characters) -> convert(characters)

    convertBack(sm + tm, sc + tc)

let private allowed(characters: Characters, move: BoatTransfer) =
    let (sm, sc) = convert(characters)
    let (tm, tc) = match move with | Transfer(characters) -> convert(characters)

    (sm >= tm) && (sc >= tc) && isSafe(sub(characters, move))

let AllowedMoves(state: State) =
    let oppositeSide = state.OppositeBank()

    BoatTransfers |>
        Seq.filter(fun transfer -> allowed(state.Characters, transfer)) |>
        Seq.filter(fun transfer -> isSafe(sub(state.Characters, transfer))) |>
        Seq.filter(fun transfer -> isSafe(add(oppositeSide.Characters, transfer)))

let ApplyTransfer(state: State, transfer: BoatTransfer) =
    let oppositeSide = state.OppositeBank()
    new State(oppositeSide.Bank, add(oppositeSide.Characters, transfer))

let FollowingStates(state: State) =
    let oppositeSide = state.OppositeBank()
    AllowedMoves(state) |>
        Seq.map(fun transfer -> ApplyTransfer(state, transfer))

[<CustomEquality; CustomComparison>]
type History =
    | Start
    | CurrentState of int * State * History

    override x.Equals yobj = 
        match x with
        | Start ->
            match yobj with
                | :? History as y -> 
                    match y with 
                    | Start -> true
                    | CurrentState(_, _, _) -> false
                | _ -> false

        | CurrentState(xGen, xState, _) ->
            match yobj with
                | :? History as y -> 
                    match y with 
                    | Start -> false
                    | CurrentState(yGen, yState, _) -> xGen = yGen && xState = yState
                | _ -> false

    override x.ToString() = 
        match x with
        | Start -> "start"
        | CurrentState(gen, state, _) -> sprintf "generation %d with state %O" gen state

    override x.GetHashCode() =
        match x with
        | Start -> -1
        | CurrentState(gen, state, _) -> gen ^^^ hash state 
        
    interface System.IComparable with
        member x.CompareTo(yobj) =
            match x with
            | Start ->
                match yobj with
                    | :? History as y -> 
                        match y with 
                        | Start -> 0
                        | CurrentState(_, _, _) -> -1
                    | _ -> invalidArg "yobj" "cannot compare values of different types"

            | CurrentState(xGen, xState, _) ->
                match yobj with
                    | :? History as y -> 
                        match y with 
                        | Start -> -1
                        | CurrentState(yGen, yState, _) -> compare xGen yGen
                    | _ -> invalidArg "yobj" "cannot compare values of different types"

// E = {e1, e2, …} is a finite or recursively enumerable set of events
let IsGoalState(state: State) =
    match state.Characters with
    | MissionariesAndCanibals(m, c) when m = nM && c = nM -> state.Bank = Right
    | _ -> false

// γ: S×(A∪E)→2^S is a state transition function
open FSharpx.Collections
open Microsoft.FSharp.Collections
    
let Run() =
    let rec run(priorityQueue, visitedStates: Set<State>) =
        let (history, priorityQueue) = PriorityQueue.pop priorityQueue
        let (CurrentState(gen, currentState, _)) = history

        if IsGoalState(currentState) then
            history
        else 
            let visitedStates = visitedStates.Add(currentState)
            let followingStates = 
                FollowingStates(currentState) |>
                    Seq.filter(fun state -> not(visitedStates.Contains(state))) |>
                    Seq.toList

            let priorityQueue =
                List.fold
                    (fun priorityQueue nextState -> 
                        PriorityQueue.insert (CurrentState(gen + 1, nextState, history)) priorityQueue)
                    priorityQueue followingStates

            run(priorityQueue, visitedStates)
        
    let priorityQueue = PriorityQueue.empty false
    let start = CurrentState(0, InitialState, Start)
    let priorityQueue =  PriorityQueue.insert start priorityQueue            
                
    let visitedStates = new Set<State>([ ])

    run(priorityQueue, visitedStates)