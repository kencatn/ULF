module ULF
open Parser
open Lexing
open Common.Utils
open NonEmptyTree
open Parser.ULF
open Elmish

open Common.Utils.ReduUndo

#if DEBUG
    open Elmish.HMR
#endif

type Parsed = {
    tokens: (Parser.token * Position * Position) seq
    synModule: Syntax.SynModule
    typeCheck: Result<NonEmptyTree<((TypelessJudgemnt*string option))>, NonEmptyTree<(TypelessJudgemnt * string option)>>
    typeCheckTree: Result<RedoUndo<(TypelessJudgemnt* string option) >, RedoUndo<(TypelessJudgemnt * string option)>> 
}
type Model =
    {
        code: string
        parseResult: Parsed option
    }
let str = """module primdtt.foundation where {
        Type : ⇒ □;
        el : (A : Type) ⇒ ∗;
        Id : (A : Type) (a : el(A)) (b : el(A)) ⇒ Type;
        refl : (A : Type) (a : el(A)) ⇒ el(Id(A, a, a));
        indId : (A : Type) (a : el(A)) (b : el(A)) (p : el(Id(A, a, b))) (C : (x : el(A)) → (y : el(Id(A, a, x))) → Type) (c : el((C a) (refl(A, a))))⇒ el(C b p);
        _eq: (A : Type) (a : el(A)) (C : (x : el(A)) → (y : el(Id(A, a, x))) → Type) (c : el((C a) (refl(A, a)))) ⇒ indId(A, a, a, refl(A, a), C, c) = c
    }
"""
type Msg =
    | ChangeCode of string
let init () =
    {
        code = ""
        parseResult = None
    }, Cmd.ofMsg (ChangeCode str)
let update msg model =
    match msg with
    | ChangeCode str ->
        let parseResult =
            Program.parse Parser.start str
            |> function
            | Error _ -> None
            | Ok a ->
                let lexbuf = LexBuffer.FromString str
                let tokens = 
                    Seq.unfold (fun buf -> 
                        try
                            let r = Lexer.tokenstream buf
                            let sp = buf.StartPos
                            let ep = buf.EndPos
                            ((r, sp, ep), buf )|> Some
                        with
                        | _ -> None
                    ) lexbuf |> Seq.cache
                let typeCheckResult =
                    a.syntax.ulf (Syntax.Env.empty)
                    |> Sig
                    |> (ULF.checkTypeAll)
                let t =
                    match typeCheckResult with
                    | Ok j -> j |> RedoUndo.ofNonEmptyTree |> Ok
                    | Error t -> t |> RedoUndo.ofNonEmptyTree |> Error
                {
                    tokens = tokens
                    synModule = a
                    typeCheck = typeCheckResult
                    typeCheckTree = t
                } |> Some
        {
            code = str
            parseResult = parseResult
        }, Cmd.none

open Feliz

let view model dispatch =
    Html.div [
        Html.textarea [
            prop.defaultValue model.code
            prop.onChange (ChangeCode>>dispatch)
        ]
        match model.parseResult with
        | None -> ()
        | Some x ->
        Html.pre [
            let mutable p = 0
            let mutable cn = 0
            for (token, sp, ep) in x.tokens do
                let n = sp.Line
                cn <- n
                let sp = sp.pos_cnum + n
                let ep = ep.pos_cnum + n
                if sp > p then yield Html.span (model.code.[p..sp-1])
                yield 
                    Html.span [
                        let color =
                            match token with
                            | Parser.token.MODULE -> "module"
                            | Parser.token.IDENT str -> "ident"
                            | Parser.token.WHERE -> "where"
                            | Parser.token.BLOCKBEGIN -> "blockbegin"
                            | Parser.token.BLOCKEND -> "blockend"
                            | Parser.token.RECT -> "rect"
                            | Parser.token.DARROW -> "darrow"
                            | Parser.token.COLON -> "colon"
                            | Parser.token.COMMA -> "comma"
                            | Parser.token.DOT -> "dot"
                            | Parser.token.EQ -> "eq"
                            | Parser.token.IMPORT -> "import"
                            | Parser.token.JEQ -> "jeq"
                            | Parser.token.LAMBDA -> "lambda"
                            | Parser.token.LPAREN -> "lparen"
                            | Parser.token.RPAREN -> "rparen"
                            | Parser.token.OPEN -> "open"
                            | Parser.token.PUBLIC -> "public"
                            | Parser.token.STAR -> "star"
                            | Parser.token.SEMICOLON -> "semicolon"
                            | Parser.token.REFL -> "refl"
                            | Parser.token.ARROW -> "arrow"
                            | Parser.token.UNDER_BAR -> "under_bar"
                            | Parser.token.EOF 
                            | Parser.token.INDENT 
                            | Parser.token.NEWLINE
                            | Parser.token.OUTDENT
                            | Parser.token.SPACE                                   -> ""
                        prop.classes ["token"; color]
                        prop.children [
                            Html.span (model.code.[sp..ep-1])
                        ]
                    ]
                p <- ep
        ]
        match model.parseResult with
        | None -> ()
        | Some r ->
            let rec p t =
                Html.ul [
                    Html.pre (
                        t.current 
                        |> fun (x, y) -> 
                            let j = judgementToString x
                            sprintf "%O: %O" y j)
                    for x in t |> RedoUndo.allRedo do
                        Html.li [
                            prop.children [
                                p x
                            ]
                        ]]
            Html.div [
                Html.pre 
                    r.synModule.display
                
            ]
            match r.typeCheckTree with
            | Ok t ->
                Html.div [
                    prop.classes ["type_check"; "ok"]
                    prop.children [
                        p t
                    ]
                ]
                
            | Error t -> 
                Html.div [
                    prop.classes ["type_check"; "error"]
                    prop.children [
                        p t
                    ]
                ]
    ]