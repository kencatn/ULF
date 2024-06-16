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
        indId : (A : Type) (a : el(A)) (b : el(A)) (p : el(Id(A, a, b))) (C : (x : el(A)) → (y : el(Id(A, a, x))) → Type) (c : el(C a (refl(A, a))))⇒ el(C b p);
        _eq: (A : Type) (a : el(A)) (C : (x : el(A)) → (y : el(Id(A, a, x))) → Type) (c : el(C a (refl(A, a)))) ⇒ indId(A, a, a, refl(A, a), C, c) = c
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
                let typeCheckResult =
                    a.syntax.ulf (Syntax.Env.empty)
                    |> Sig
                    |> (ULF.checkTypeAll)
                let t =
                    match typeCheckResult with
                    | Ok j -> j |> RedoUndo.ofNonEmptyTree |> Ok
                    | Error t -> t |> RedoUndo.ofNonEmptyTree |> Error
                {
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
        | Some r ->
            let rec p t =
                Html.ul [
                    Html.span (
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
                    prop.style [style.color "green"]
                    prop.children [
                        p t
                    ]
                ]
                
            | Error t -> 
                Html.div [
                    prop.style [style.color "red"]
                    prop.children [
                        p t
                    ]
                ]
    ]