module Parser.Lexer

 

// Opens methods related to fslex.exe
open Lexing

let newline (lexbuf: LexBuffer<_>) = 
  lexbuf.EndPos <- lexbuf.StartPos.NextLine
let lexeme = LexBuffer<_>.LexemeString
open Parser.Parser
let keywords =
    [
        "_", UNDER_BAR;
        "\\", LAMBDA;
        "lambda", LAMBDA;
        "open", OPEN;
        "import", IMPORT;
        "module", MODULE;
        "where", WHERE;
        "public", PUBLIC;
        "refl_", REFL;
    ] |> Map.ofList


let trans : uint16[] array = 
    [| 
    (* State 0 *)
     [| 20us;20us;20us;20us;20us;20us;20us;20us;20us;21us;18us;20us;20us;19us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;17us;20us;20us;20us;20us;20us;20us;20us;2us;3us;12us;20us;7us;10us;1us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;5us;6us;20us;4us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;15us;20us;16us;20us;20us;8594us;8us;8658us;9us;955us;11us;8727us;13us;9633us;14us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;20us;22us;|];
    (* State 1 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 2 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 3 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 4 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;26us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 5 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 6 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 7 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 8 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 9 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 10 *)
     [| 23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;23us;65535us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;65535us;25us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;23us;65535us;23us;23us;8594us;65535us;8658us;65535us;955us;65535us;8727us;23us;9633us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;|];
    (* State 11 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 12 *)
     [| 23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;23us;65535us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;23us;65535us;23us;23us;8594us;65535us;8658us;65535us;955us;65535us;8727us;23us;9633us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;|];
    (* State 13 *)
     [| 23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;23us;65535us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;23us;65535us;23us;23us;8594us;65535us;8658us;65535us;955us;65535us;8727us;23us;9633us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;|];
    (* State 14 *)
     [| 23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;23us;65535us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;23us;65535us;23us;23us;8594us;65535us;8658us;65535us;955us;65535us;8727us;23us;9633us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;|];
    (* State 15 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 16 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 17 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 18 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 19 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;24us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 20 *)
     [| 23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;23us;65535us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;23us;65535us;23us;23us;8594us;65535us;8658us;65535us;955us;65535us;8727us;23us;9633us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;|];
    (* State 21 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 22 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 23 *)
     [| 23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;23us;65535us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;23us;65535us;23us;23us;8594us;65535us;8658us;65535us;955us;65535us;8727us;23us;9633us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;|];
    (* State 24 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    (* State 25 *)
     [| 23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;23us;65535us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;65535us;23us;65535us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;23us;65535us;23us;23us;8594us;65535us;8658us;65535us;955us;65535us;8727us;23us;9633us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;23us;65535us;|];
    (* State 26 *)
     [| 65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;8594us;65535us;8658us;65535us;955us;65535us;8727us;65535us;9633us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;|];
    |] 
let actions : uint16[] = [|65535us;0us;1us;2us;3us;4us;5us;6us;7us;8us;20us;11us;13us;14us;15us;16us;17us;18us;19us;21us;20us;21us;22us;20us;19us;10us;9us;|]
let _fslex_tables = Lexing.UnicodeTables.Create(trans,actions)
let rec _fslex_dummy () = _fslex_dummy() 
// Rule tokenstream
and tokenstream  lexbuf =
  match _fslex_tables.Interpret(0,lexbuf) with
  | 0 -> ( 

                         Parser.DOT 

          )
  | 1 -> ( 

                      LPAREN

          )
  | 2 -> ( 

                      RPAREN

          )
  | 3 -> ( 

                      EQ

          )
  | 4 -> ( 

                      COLON

          )
  | 5 -> ( 

                      SEMICOLON

          )
  | 6 -> ( 

                      COMMA

          )
  | 7 -> ( 

                      ARROW

          )
  | 8 -> ( 

                      DARROW

          )
  | 9 -> ( 

                       DARROW

          )
  | 10 -> ( 

                       ARROW

          )
  | 11 -> ( 

                      LAMBDA

          )
  | 12 -> ( 

                      DOT

          )
  | 13 -> ( 

                      STAR

          )
  | 14 -> ( 

                      STAR

          )
  | 15 -> ( 

                      RECT

          )
  | 16 -> ( 

                      BLOCKBEGIN

          )
  | 17 -> ( 

                      BLOCKEND

          )
  | 18 -> ( 

                              tokenstream lexbuf 

          )
  | 19 -> ( 

                           newline lexbuf; tokenstream lexbuf

          )
  | 20 -> ( 

                        
                 match keywords.TryFind(lexeme lexbuf) with
                               | Some(token) -> token
                               | None -> IDENT(lexeme lexbuf)

          )
  | 21 -> ( 

                          failwith ("ParseError" + LexBuffer<_>.LexemeString lexbuf) 

          )
  | 22 -> ( 

                          Parser.EOF 

          )
  | _ -> failwith "tokenstream"


