module MissionariesAndCanibalsProperties

open FsCheck
open MissionariesAndCanibals

let ``Initial state is valid`` () =
    InitialState.IsStateAllowed()

let ``Number of missionaries should be greater or equal to canibals `` (bank: RiverBank, characters: Characters) =
    let state = new State(bank, characters)

    let expectedState = 
        match characters with
        | Nobody                        -> true
        | Missionaries(m)               -> true
        | Canibals(_)                   -> true
        | MissionariesAndCanibals(m, c) -> m >= c

    state.IsStateAllowed() = expectedState

let ``Opposite state has opposite bank`` (bank: RiverBank, characters: Characters) =
    let state = new State(bank, characters)
    let oppositeState = state.OppositeBank()

    state.Bank <> oppositeState.Bank

let private numberOfMissionariesOnThisBank(state: State) = 
    match state.Characters with 
    | Nobody                        -> 0
    | Missionaries(m)               -> m
    | Canibals(_)                   -> 0
    | MissionariesAndCanibals(m, _) -> m

let private numberOfCanibalsOnThisBank(state: State) = 
    match state.Characters with 
    | Nobody                        -> 0
    | Missionaries(_)               -> 0
    | Canibals(c)                   -> c
    | MissionariesAndCanibals(_, c) -> c

let private numberOfCharactersOnThisBank(state: State) = 
    numberOfMissionariesOnThisBank(state) + numberOfCanibalsOnThisBank(state)

let ``Number of missionaries on both banks is constant`` (bank: RiverBank, characters: Characters) =
    let state = new State(bank, characters)
    let oppositeState = state.OppositeBank()

    numberOfMissionariesOnThisBank(state) + numberOfMissionariesOnThisBank(oppositeState) = 3

let ``Number of canibals on both banks is constant`` (bank: RiverBank, characters: Characters) =
    let state = new State(bank, characters)
    let oppositeState = state.OppositeBank()

    numberOfCanibalsOnThisBank(state) + numberOfCanibalsOnThisBank(oppositeState) = 3

let ``Number of missionaries and canibals on both banks is constant`` (bank: RiverBank, characters: Characters) =
    let state = new State(bank, characters)
    let oppositeState = state.OppositeBank()

    numberOfCharactersOnThisBank(state) + numberOfCharactersOnThisBank(oppositeState) = 6

let ``Only state with everyone on right bank is goal state`` (bank: RiverBank, characters: Characters) =
    let state = new State(bank, characters)
    let isGoalState = 
        bank = Right &&
        numberOfMissionariesOnThisBank(state) = 3 &&
        numberOfCanibalsOnThisBank(state) = 3

    IsGoalState(state) = isGoalState

let ``Allowed moves results in both banks to be in allowed state`` (bank: RiverBank, characters: Characters) =
    let state = new State(bank, characters)
    let followingStates = FollowingStates(state)

    let allFollowingStatesAreAllowed =
        followingStates |> 
            Seq.forall(fun followingState -> followingState.IsStateAllowed())

    let allOppositeToFollowingStatesAreAllowed =
        followingStates |> 
            Seq.map(fun followingState -> followingState.OppositeBank()) |>
            Seq.forall(fun oppotiteBank -> oppotiteBank.IsStateAllowed())

    allFollowingStatesAreAllowed && allFollowingStatesAreAllowed

let private convertStateToNumberOfCharacters(state: State) = 
    (numberOfMissionariesOnThisBank(state), numberOfCanibalsOnThisBank(state))

let private convertMoveToNumberOfCharacters(transfer: BoatTransfer) = 
    match transfer with 
    | Transfer(Nobody)                          -> (0, 0)
    | Transfer(Missionaries(m))                 -> (m, 0)
    | Transfer(Canibals(c))                     -> (0, c)
    | Transfer(MissionariesAndCanibals(m, c))   -> (m, c)


let ``All skipped moves results in unallowed state on either bank`` (bank: RiverBank, characters: Characters) =
    let state = new State(bank, characters)
    let allowedMoves = AllowedMoves(state)
    let forbiddenMoves =
        BoatTransfers |> 
            Seq.filter(fun transfer -> 
                not(allowedMoves |> Seq.exists(fun allowedTransfer ->
                    transfer = allowedTransfer) ) )

    let (sm, sc) = convertStateToNumberOfCharacters(state)
        
    forbiddenMoves |>
        Seq.map(fun forbidden -> convertMoveToNumberOfCharacters(forbidden)) |>
        Seq.forall(fun (tm, tc) -> 
            ((tm > sm) || (tc > sc)) ||
            (sm - tm < sc - tc) ||
            (3 - sm + tm < 3 - sc + tc))

let private riverBankGen =
    Gen.oneof [
        gen { return Left };
        gen { return Right };
    ]

let private nCharacters = Gen.elements [ 1 .. 3 ]

let private charactersGen =
    Gen.oneof [
        gen { return Nobody };
        Gen.map Missionaries nCharacters;
        Gen.map Canibals nCharacters;    
        Gen.map MissionariesAndCanibals (Gen.two nCharacters);
    ]

let private riverBankStates =
    Gen.map2 (fun rb ch -> (rb, ch)) riverBankGen charactersGen

let ``prove statements`` () =
    Check.VerboseThrowOnFailure ``Initial state is valid``

    Check.VerboseThrowOnFailure (Prop.forAll (Arb.fromGen riverBankStates)
        ``Number of missionaries should be greater or equal to canibals ``)

    Check.VerboseThrowOnFailure (Prop.forAll (Arb.fromGen riverBankStates)
        ``Opposite state has opposite bank``)

    Check.VerboseThrowOnFailure (Prop.forAll (Arb.fromGen riverBankStates)
        ``Number of missionaries on both banks is constant``)

    Check.VerboseThrowOnFailure (Prop.forAll (Arb.fromGen riverBankStates)
        ``Number of canibals on both banks is constant``)

    Check.VerboseThrowOnFailure (Prop.forAll (Arb.fromGen riverBankStates)
        ``Number of missionaries and canibals on both banks is constant``)

    Check.VerboseThrowOnFailure (Prop.forAll (Arb.fromGen riverBankStates)
        ``Only state with everyone on right bank is goal state``)

    Check.VerboseThrowOnFailure (Prop.forAll (Arb.fromGen riverBankStates)
        ``Allowed moves results in both banks to be in allowed state``)

    Check.VerboseThrowOnFailure (Prop.forAll (Arb.fromGen riverBankStates)
        ``All skipped moves results in unallowed state on either bank``)