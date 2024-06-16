module Parser.Program

open System.IO
open FSharp.Text.Lexing


open Syntax
open ULF

type A = Range


// let testLexerAndParserFromString text expectedCount = 
//     let lexbuf = LexBuffer<char>.FromString text

//     let countFromParser = Parser.start Lexer.tokenstream lexbuf

//     printfn "countFromParser: result = %O, expected %d" countFromParser expectedCount

// let testLexerAndParserFromFile (fileName:string) expectedCount = 
//     use textReader = new System.IO.StreamReader(fileName)
//     let lexbuf = LexBuffer<char>.FromTextReader textReader

//     let countFromParser = Parser.start Lexer.tokenstream lexbuf

//     printfn "countFromParser: result = %O, expected %d" countFromParser expectedCount

// testLexerAndParserFromString "hello" 1
// testLexerAndParserFromString "hello.hello" 2

// let testFile = Path.Combine(__SOURCE_DIRECTORY__, "test.txt")
// File.WriteAllText(testFile, "hello hello")
// testLexerAndParserFromFile testFile 2

// printfn "Press any key to continue..."
// System.Console.ReadLine() |> ignore

let lexbufToSeq lexbuf =
    Seq.unfold (fun lexbuf ->
        try
            (
                Lexer.tokenstream lexbuf, lexbuf
            ) |> Some
        with
        | _ -> None

    ) lexbuf

let lex text =
    let lexbuf = LexBuffer<char>.FromString text
    lexbuf |> lexbufToSeq

let parse parser str =
    let lexbuf = LexBuffer<char>.FromString str
    try
        parser Lexer.tokenstream lexbuf
        |> Ok
    with
    | _ -> Error ("", "")
    // | Parser.ParseError (err, s) -> Error (err, s)
    
let str = """module primdtt.foundation where {
        Type : ⇒ □;
        el : (A : Type) ⇒ ∗;
        Id : (A : Type) (a : el(A)) (b : el(A)) ⇒ Type;
        refl : (A : Type) (a : el(A)) ⇒ el(Id(A, a, a));
        indId : (A : Type) (a : el(A)) (b : el(A)) (p : el(Id(A, a, b))) (C : (x : el(A)) → (y : el(Id(A, a, x))) → Type) (c : el(C a (refl(A, a))))⇒ el(C b p);
        _eq: (A : Type) (a : el(A)) (C : (x : el(A)) → (y : el(Id(A, a, x))) → Type) (c : el(C a (refl(A, a)))) ⇒ indId(A, a, a, refl(A, a), C, c) = c
    }
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
    lexed str |> Seq.iter (printf "%O;")
    printfn ""
    let lexbuf = LexBuffer<char>.FromString str
    let x = Parser.start Lexer.tokenstream lexbuf
    x |> function
    | (a) -> 
        a |> string
        |> printfn "%O"
        a.display |> printfn "%O"
        
        let env = Env.empty
        // check env a.syntax.syntax |> printfn "%O"
        let preSig = a.syntax.ulf env
        printfn "----------------"
        preSig |> ULF.signatureToString |> printfn "%O"
        printfn "----------------"
        Sig (preSig)
        |> checkTypeAll
        |> function
        | a -> 
            // a |> printfn "%O"
            printfn "ok" 

with
| _ -> ()    
// | Parser.ParseError (err, state) ->
//     printErrors err str state

// let arg = """a → b → c"""

// let lexbuf = LexBuffer<char>.FromString arg
// arg
// |> lex
// |> Seq.iter (printf "%O; ")
// try
//     let x = Parser.term Lexer.tokenstream lexbuf
//     printfn ""
//     lexbuf
//     |> lexbufToSeq
//     |> Seq.iter (printf "%O; ")
//     printfn ""
//     printfn "%O" (x.ulf Env.empty)
//     x.display
//     |> printfn "%O"
// with
// | Parser.ParseError (a, b) ->
//     printErrors a arg b