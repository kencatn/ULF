module Parser.Lexer

open FSharp.Text.Lexing
open Parser.Parser/// Rule start
val start: lexbuf: LexBuffer<char> -> token
/// Rule tokenstream
val tokenstream: lexbuf: LexBuffer<char> -> token
