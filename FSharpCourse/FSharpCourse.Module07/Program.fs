﻿type ParserResult<'a> =
    | Success of 'a * list<char>
    | Failure

type Parser<'a> = list<char> -> ParserResult<'a>

let Return (x: 'a): Parser<'a> =
    let p stream = Success(x, stream)
    in p

let Bind (p: Parser<'a>) (f: 'a -> Parser<'b>) : Parser<'b> =
    let q stream =
        match p stream with
        | Success(x, rest) -> (f x) rest
        | Failure -> Failure
    in q

let (>>=) = Bind

/// If parser p succeeds returns x as a result.
let (>>%) p x : Parser<'b> =
    p >>= (fun _ -> Return x)

/// Applies parsers p1 and p2 returning the result of p2.
let (>>.) p1 p2 : Parser<'b> =
    p1 >>= (fun _ -> p2)

/// Applies parsers p1 and p2 returning the result of p1.
let (.>>) p1 p2 : Parser<'a> =
    p1 >>= (fun x -> p2 >>% x)
    
/// Applies parsers p1 and p2 returning both results.
let (.>>.) p1 p2: Parser<'a*'b> =
    p1 >>= (fun x -> p2 >>= (fun y -> Return (x, y)))

type ParserBuilder() =
    member x.Bind(p, f) = Bind p f
    member x.Return(y) = Return y

let parse = new ParserBuilder()

let Either (p1: Parser<'a>) (p2: Parser<'a>) : Parser<'a> =
    let p stream =
        match p1 stream with
        | Failure -> p2 stream
        | res -> res
    in p

// This is the Either combinator defined in the previous blog post.
let (<|>) = Either

let rec Many p : Parser<list<'a>> =
    parse {
        let! x = p          // Applies p
        let! xs = (Many p)  // Applies (Many p) recursively
        return x :: xs      // returns the cons of the two results
    } <|> Return []

let Many1 p : Parser<list<'a>> =
    parse {
        let! x = p          // Applies p
        let! xs = (Many p)  // Applies (Many p) recursively
        return x :: xs      // returns the cons of the two results
    }

let CharParser (c: char) : Parser<char> =
    let p stream =
        match stream with
        | x::xs when x = c -> Success(x, xs)
        | _ -> Failure
    in p

let DigitParser : Parser<char> =
    ['0'..'9']
    |> List.map CharParser
    |> List.reduce Either

/// Parses float numbers which match /[+-]?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?/
let FloatParser: Parser<float> =
    parse {
        let! s = (CharParser '+' <|> CharParser '-') <|> Return '+'     // [+-]?
        let! l = (parse {                                               // (
            let! l = Many1 DigitParser                                  //  \d+
            let! pd = (parse {                                          //  (
                let! p = CharParser '.'                                 //   \.
                let! d = Many DigitParser                               //   \d*
                return p::d
                } <|> Return [])                                        //  )?
            return l @ pd
            } <|>                                                       //  |
            parse {
                let! l = Many DigitParser                               // \d*
                let! p = CharParser '.'                                 // \.
                let! d = Many1 DigitParser                              // \d+
                return l @ p::d
            }
        )
        let! e = (parse {
            let! e = CharParser 'e' <|> CharParser 'E'                  // ([eE]
            let! s = (CharParser '+' <|> CharParser '-') <|> Return '+' //   [+-]?
            let! x = Many1 (DigitParser)                                //   \d+
            return e::s::x                                              // )?
        } <|> Return [])
        return float (new System.String(s::(l @ e) |> List.toArray))
    }

printfn "%A" (FloatParser ("-1.23e45" |> Seq.toList))