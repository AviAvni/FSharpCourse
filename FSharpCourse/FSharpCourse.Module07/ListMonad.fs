module ListMonad

type ListBuilder() =
    member this.Bind(m, f) =
        m
        |> List.collect f

    member this.Return x = [x]

let list = ListBuilder()

let example = 
    list {
        let! x = [1..10]
        let! y = [1..10]
        return x * y
    }

let example1 =
    [ 
        for x in [1..10] do
            for y in [1..10] do
                yield x * y
    ]