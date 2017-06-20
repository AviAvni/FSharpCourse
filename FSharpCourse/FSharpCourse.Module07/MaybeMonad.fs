module MaybeMonad

let bind (m, f) = 
    match m with
    | Some x -> f x
    | None -> None

let return' x = Some(x)

type MaybeBuilder() =
    member this.Bind(m, f) = bind(m, f)

    member this.Return(x) = return' x

let maybe = MaybeBuilder()

let divideBy bottom top =
    if bottom = 0
    then None
    else Some(top/bottom)

let divideByExample x y w z = 
    let a = x |> divideBy y 
    match a with
    | None -> None
    | Some a' ->
        let b = a' |> divideBy w
        match b with
        | None -> None
        | Some b' ->
            let c = b' |> divideBy z
            match c with
            | None -> None
            | Some c' ->
                Some c'

let divideByBindExample x y w z = 
    bind (x |> divideBy y, fun a ->
    bind (a |> divideBy w, fun b ->
    bind (b |> divideBy z, fun c ->
    return' c 
    )))

let divideByWorkflowExample x y w z =
    maybe {
        let! a = x |> divideBy y 
        let! b = a |> divideBy w
        let! c = b |> divideBy z
        return c
    }

let good1 = divideByExample 12 3 2 1
let bad1 = divideByExample 12 3 0 1
let good2 = divideByBindExample 12 3 2 1
let bad2 = divideByBindExample 12 3 0 1
let good3 = divideByWorkflowExample 12 3 2 1
let bad3 = divideByWorkflowExample 12 3 0 1