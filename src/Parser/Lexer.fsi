module Parser.Lexer

open FSharp.Text.Lexing
open Parser.Parser/// Rule tokenstream
val tokenstream: lexbuf: LexBuffer<char> -> token
