module Parser.Lexer

open FSharp.Text.Lexing
open Parser.Parser
open Parser.LexHelper/// Rule start
val start: state: LexState -> lexbuf: LexBuffer<char> -> token
/// Rule tokenstream
val tokenstream: state: obj -> lexbuf: LexBuffer<char> -> token
