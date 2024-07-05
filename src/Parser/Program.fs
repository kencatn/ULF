module Parser.Program

open System.IO
open FSharp.Text.Lexing


open Syntax
open ULFBase

type A = Range


let lexfwithPos () = 
    let mutable state = {
        LexHelper.indents = []
        LexHelper.LexState.eof = false
        LexHelper.LexState.acceptIndent = false
        LexHelper.LexState.tokens = []
    }
    fun lexbuf ->
        let loop lexbuf =
            let token, s = Lexer.start state lexbuf
            state <- s
            token
        loop lexbuf

let lexf () = lexfwithPos() >> fst

let lexbufToSeq lexbuf =
    let lex = lexf ()
    Seq.unfold (fun lexbuf ->
        try
            (
                lex lexbuf, lexbuf
            ) |> Some
        with
        | _ -> None

    ) lexbuf

let lex text =
    let lexbuf = LexBuffer<char>.FromString text
    lexbuf |> lexbufToSeq

let parse parser str =
    let lexbuf = LexBuffer<char>.FromString str
    let lexf = lexf()
    try
        parser lexf lexbuf
        |> Ok
    with
    | Parser.ParseError (err, s) -> Error (err, s)
    | e -> 
        printfn "%O" e 
        failwith "eer"
    
    
let str = """module primdtt.foundation where 
        Type : ⇒ □
"""
let indent = """qwsad where
    a
    asdf
    qwsafe where
        asdf
"""
// let str = """module a 
//     where {
//         open x
//         open y
// }"""
// let str = """(x : el A) → el (B x)"""
// let str = "app(abs(b), a)"
// let str = "app()"
let printErrors err (str: string) (state: FSharp.Text.Parsing.IParseState) =
    let (s, t) = state.ResultRange
    let s = s.pos_cnum + s.pos_lnum
    let t = t.pos_cnum + t.pos_lnum
    printfn "%O" state.ResultRange
    printfn "%O error" err
    printfn "%O" str.[(s - 5) |> max 0..(s-1) |> max 0]
    printfn "↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓"
    printfn "%O" str.[s..t]
    printfn "↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑"
    printfn "%O" str.[(t+1) |> min (str.Length - 1)..(t + 5) |> min (str.Length - 1)]

try 
    let lexed = lex
    let ls = lexed str |> Seq.toList
    ls |> Seq.iter (printf "%O;")
    printfn ""
    let lexbuf = LexBuffer<char>.FromString str
    let lexf = lexf()
    let x = Parser.start lexf lexbuf
    x |> function
    | (a) -> 
        // a |> string
        // |> printfn "%O"
        a.display |> printfn "%O"
        
        let env = Env.empty
        // check env a.syntax.syntax |> printfn "%O"
        let preSig = a.syntax.ulf env
        printfn "----------------"
        // preSig |> ULF.signatureToString |> printfn "%O"
        printfn "----------------"
        Sig (preSig)
        |> checkTypeAll
        |> function
        | Ok _ -> 
            // a |> printfn "%O"
            printfn "ok" 
        | Error e ->
            printfn "error"
with
| Parser.ParseError (err, state) ->
    printErrors err str state
| _ -> ()    


let pi = """module pidtt.foundation where
    Type : => □
    el : (A : Type) => *
    Pi : (A : Type) (B : (el A) → Type) ⇒ Type
    abs : (A : Type) (B : (el A) → Type) (b : (x : el A) → el (B x)) ⇒ el (Pi(A, B))
    app : (A : Type) (B : (el A) → Type) (p : el (Pi(A, B))) (a : el A) ⇒ el (B a)
    (A : Type) (B : (el A) → Type) (b : (x : el A) → el (B x)) (a : el A) ⇒ app(A, B, abs(A, B, b), a) = b a
    (A : Type) (B : (el A) → Type) (p : el (Pi(A, B))) ⇒ abs(A, B, λ (a : el A) → app(A, B, p, a)) = p
"""

parse Parser.start pi
|> function 
| Ok a -> 
    printfn "ok!!"
    
| Error (err, s) -> 
    printErrors err pi s

// let syntax = """Type : ⇒ □; el : (A : Type) ⇒ *;"""
// lex syntax |> Seq.iter (printf "%O; ")
// printfn ""
// parse Parser.startSyntax syntax
// |> function
// | Ok a -> 
//     printfn "ok"
//     printfn "%O" a.display
// | Error (err, state) -> 
//     printErrors err syntax state