module Parser.LexHelper 

open FSharp.Text.Lexing


type LexState =
    {
        eof: bool
        tokens: Parser.token list
        acceptIndent: bool
        indents: int list
    }

module Lex =
    let token token state =
        token, state
    
