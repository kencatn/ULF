module Parser.LexHelper 

open FSharp.Text.Lexing


type LexState =
    {
        eof: bool
        tokens: (Parser.token * (Position * Position)) list
        acceptIndent: bool
        indents: int list
    }

module Lex =
    let token token state (lexbuf : LexBuffer<char>) =
        (token, (lexbuf.StartPos, lexbuf.EndPos)) , state
    
