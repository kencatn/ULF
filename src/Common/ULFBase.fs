module ULFBase
open Common
type Ident = string
type LongIdent = Ident list
type VariableName = 
    | VariableName of LongIdent
type SymbolName = 
    | SymbolName of LongIdent
    | Annonymouse of System.Guid
type PreTerm = 
    | Star
    | Rect
    | Symbol of (SymbolName * PreTerm list)
    | Variable of VariableName
    | Pi of (PreTerm * VariableName option * PreTerm)
    | Abs of (PreTerm option * VariableName * PreTerm)
    | App of (PreTerm option * VariableName option * PreTerm option * PreTerm * PreTerm)
    | Eq of (PreTerm option * PreTerm * PreTerm)
    | Refl of PreTerm
type PreContext = 
    {
        preContext: (VariableName * PreTerm) list
    }
type PreSignatureElement = (SymbolName * PreContext * PreTerm)
type PreSignature = 
    {
        symbols: PreSignatureElement list
    }


type Judgment =
    | Sig of PreSignature
    | Ctx of (PreSignature * PreContext)
    | Term of (PreSignature * PreContext * PreTerm * PreTerm)
    | JEq of (PreSignature * PreContext * PreTerm * PreTerm * PreTerm)

// [<RequireQualifiedAccess>]
type TypelessJudgemnt =
    | Sig of (PreSignature)
    | Ctx of (PreSignature * PreContext)
    | Term of (PreSignature * PreContext * PreTerm * PreTerm option)
    | JEq of (PreSignature * PreContext * PreTerm * PreTerm * PreTerm)
let longIdentToString (ident) =
    ident |> String.concat "."

let symbolNameToString symbolName =
    match symbolName with
    | SymbolName longIdent -> longIdent |> longIdentToString
    | Annonymouse a -> a |> string
let termToString term =
    let rec loop term =
        match term with
        | Star -> "∗"
        | Rect -> "□"
        | Symbol (s, ls) ->
            let s = s |> symbolNameToString
            match ls with
            | [] -> s
            | _ ->
            let ls = ls |> List.map loop
            let str =String.concat ", " ls 
            $"{s}({str})"
        | Variable (VariableName name) -> name |> longIdentToString 
        | Pi (A, x, B) ->
            let A = loop A
            let B = loop B
            match x with
            | None -> $"{A} → ({B})"
            | Some (VariableName x) -> $"({x}: {A}) → ({B})"
        | Abs (A, (VariableName x), b) ->
            let b = loop b
            match A with
            | None -> $"λ {x}. {b}"
            | Some A -> 
                let A = loop A
                $"λ {x}: {A}. {b}"
        | App (A, x, B, b, a) ->
            let b = loop b
            let a = loop a
            match A, x, B with
            | Some A, Some (VariableName x), Some B ->
                let A = loop A
                let B = loop B
                $"App({A}, {x}, {B}, {b}, {a})"
            | _ -> $"{b} ({a})"
        | Eq (A, a, b) ->
            let a = loop a
            let b = loop b
            match A with
            | None -> $"{a} = {b}"
            | Some A -> 
                let A = loop A
                $"Eq({A}, {a}, {b})"
        | Refl A ->
            let A = loop A
            $"refl_({A})"
    loop term
let contextToString context =
    context.preContext 
    |> Seq.rev
    |> Seq.map (fun (VariableName n, t) -> 
        let t = termToString t
        $"{n}: {t}"
    ) |> String.concat ", "
    |> fun a -> $"({a})"
let symbolElmToString (s, gamma, term) =
    let gamma = contextToString gamma
    let term = term |> termToString
    let s = s |> symbolNameToString
    $"{s}: {gamma} ⇒ {term}"

let signatureToString (signature) =
    signature.symbols
    |> Seq.rev
    |> Seq.map symbolElmToString
    |> String.concat ", "
    |> fun a -> $"({a})"

let judgementToString judgement =
    match judgement with
    | Sig sigma -> 
        let sigma = signatureToString sigma
        $"{sigma}⊢sig"
    | Ctx (sigma, gamma) -> 
        let sigma = signatureToString sigma
        let gamma = contextToString gamma
        $"{sigma}|{gamma}⊢ctx"
    | Term (sigma, gamma, a, A) -> 
        let sigma = signatureToString sigma
        let gamma = contextToString gamma
        let a = termToString a
        match A with
        | Some A ->
            let A = termToString A
            $"{sigma}|{gamma}⊢{a}:{A}"
        | None ->
            $"{sigma}|{gamma}⊢{a}"
    | JEq (sigma, gamma, a, b, A) -> 
        let sigma = signatureToString sigma
        let gamma = contextToString gamma
        let a = termToString a
        let b = termToString b
        let A = termToString A
        $"{sigma}|{gamma}⊢{a}≡{b}:{A}"

let splitLast ls = 
    let rec loop ls cnt =
        match ls with
        | [] -> None
        | [x] -> Some (cnt [], x)
        | x::xs -> loop xs (fun ans -> x::ans |> cnt)
    loop ls id
let split ls =
    match ls with
    | [] -> None
    | x::xs -> Some (x, xs)
module PreTerm = 
    let simbol name args = Symbol (SymbolName name, args)
    let variable name = Variable (VariableName name)
    let substitute x n term =
        let rec loop term =
            match term with
            | Variable (y) when x = y ->
                n
            | Symbol (s, args) -> Symbol (s, args |> List.map loop)
            | Pi (A, y, B) when Some x = y -> 
                Pi (A |> loop, y, B)
            | Pi (A, y, B) ->
                Pi (A |> loop, y, B |> loop)
            | Abs (A, y, b) when x = y -> 
                Abs (A |> Option.map loop, y, b)
            | Abs (A, y, b) -> 
                Abs (A |> Option.map loop, y, b |> loop)
            | App(A, y, B, b, a) when Some x = y -> 
                App(A |> Option.map loop, y, B, b |> loop, a |> loop)
            | App (A, y, B, b, a) ->
                App (A |> Option.map loop, y, B |> Option.map loop, b |> loop, a |> loop)
            | Eq (A, a, b) -> 
                Eq (A|> Option.map loop, a |> loop, b |> loop)
            | Refl(A) -> Refl (loop A)
            | a -> a
        loop term
    let substituteMany env term =
        let rec loop term =
            match term with
            | Variable (y) ->
                env 
                |> Map.tryFind y
                |> function
                | None -> Variable (y)
                | Some n -> n
            | Symbol (s, args) -> Symbol (s, args |> List.map loop)
            | Pi (A, Some (y), B) ->
                env
                |> Map.tryFind (y)
                |> function
                | Some _ ->
                    Pi (A |> loop, Some (y), B)
                | _ -> 
                    Pi (A |> loop, Some (y), B |> loop)
            | Pi (A, y, B) ->   
                Pi (A |> loop, y, B |> loop)
            | Abs (A, y, b) ->
                env
                |> Map.tryFind y
                |> function
                | Some _ ->
                    Abs (A |> Option.map loop, y, b)
                | _ ->
                    Abs (A |> Option.map loop, y, b |> loop)
            | App(A, Some (y), B, b, a) ->
                env 
                |> Map.tryFind y
                |> function
                | Some _ ->
                    App(A |> Option.map loop, Some (y), B, b |> loop, a |> loop)
                | None -> 
                    App(A |> Option.map loop, Some (y), B |> Option.map loop, b |> loop, a |> loop)
            | App (A, y, B, b, a) ->
                App (A |> Option.map loop, y, B |> Option.map loop, b |> loop, a |> loop)
            | Eq (A, a, b) -> 
                Eq (A|> Option.map loop, a |> loop, b |> loop)
            | Refl(A) -> Refl (loop A)
            | a -> a
        loop term
module PreContext = 
    let ofList ls = {preContext = ls}
    let split preConetxt =
        split preConetxt.preContext
        |> Option.map (fun (h, t) ->
            h ,
            t |> ofList
        )
    let cons x preContext = ofList (x::preContext.preContext)
    let mkSimple ls =
        ls |> List.map (fun (a, b) -> VariableName a, b)
    let tryFindVariable v preContext =
        preContext.preContext
        |> List.tryFind (fun (x, y) -> x = v)
        |> Option.map snd
    let contains v t preContext =
        preContext.preContext 
        |> List.tryFind (fun (x, y) -> x = v)
        |> (=) (Some (v, t))

module PreSignature =
    let tryFind symbol preSig =
        preSig.symbols |> List.tryFind (fun (a, _, _) -> a = symbol)
    let ofList ls = {symbols = ls}
    let split preSig =
        let res = split preSig.symbols 
        res
        |> Option.map (fun (h, t) -> 
            h, t |> ofList 
        )
    let cons t preSig = ofList (t::preSig.symbols)
type Continue<'T> =
    | Single of 'T
    | And of Continue<'T> list
    | Or of (Continue<'T>) list
    | Label of (string * Continue<'T>)
    | True
    | False
type CheckTypeRes =
    | Continue of (Continue<TypelessJudgemnt> * Judgment)
module Continue =
    let label str c = Label (str, c)
    let singleContinue str judge = Label (str, Single judge)
let rec inferenceType (sigma, gamma, term) =

    let c =
            match term with
            | Variable (x) ->
                gamma |> PreContext.tryFindVariable x
                |> function
                | None -> None, $"variable {x} is not in context"
                | Some t ->
                    Some t
                    , "variable"
            | Symbol (alpha, f) ->
                sigma
                |> PreSignature.tryFind alpha
                |> function
                | None ->
                    None, 
                    $"symbol {alpha} is not defined" 
                | Some (alpha, delta, A) ->
                    // :slow?
                    let ctx = Single (Ctx (sigma, gamma))
                    let ld = delta.preContext.Length
                    let lf = f.Length
                    if ld <> lf then   
                        None
                        , $"length not match: {ld} {lf}" 
                    else
                    let ls =
                        List.zip (delta.preContext |> List.rev) f
                    let cs = 
                        ls 
                        |> List.fold 
                            (fun (ls, s) ((y, B), f) -> 
                                match s with 
                                | None -> 
                                    (Single (Term (sigma, gamma, f, Some B)))::ls
                                    , Some (Map.empty |> Map.add y f)
                                | Some m ->
                                    printfn "%O" m
                                    let B' = B |> PreTerm.substituteMany m
                                    (Single (Term (sigma, gamma, f, Some B')))::ls
                                    , (Some (m |> Map.add y f))) 
                            ([], None)
                    match cs with
                    | (_, None) -> 
                        Some A, "9: empty" 
                    | (ls', Some m) ->
                        let A' = A |> PreTerm.substituteMany m
                        Some A',
                        "9: "   
            | App (Some A, Some x, Some B, b, a) ->
                let B' = PreTerm.substitute x a B

                Some B', "13: app"
            | App (A, x, B, b, a) ->
                let A =
                    match A with
                    | Some A -> Some A
                    | None ->
                        inferenceType (sigma, gamma, a) |> fst
                match A with
                | None -> None, $"type inference failed {a}"
                | Some A ->
                let B = 
                    match B with
                    | Some B -> Some B
                    | None ->
                        inferenceType (sigma, gamma, b) |> fst
                match B with
                | None -> None, $"type inference failed {b}"
                | Some B ->
                match B with
                | Pi(A',x',B') ->
                    match x', x with
                    | None, None -> Some B', "13: app with type inference"
                    | _, Some x | Some x, _ ->
                        let B'' = B' |> PreTerm.substitute x a
                        Some B'', "13: app with type inference"
                | _ -> None, "type inference failed"
            | Star | Rect -> None, "star or rect"
            | Pi(_) -> Some Rect, "pi"
            | Abs(A,x,b) -> 
                match A with
                | None -> None, "TODO:"
                | Some A -> 
                    let B = inferenceType (sigma, gamma |> PreContext.cons (x, A), b) |> fst
                    B |> Option.map (fun B ->
                        Pi(A, Some x, B)), "TODO:"
            | Eq(_) -> Some Rect, "eq"
            | Refl(a) ->
                Some (Eq(None, a, a)), "refl"
    c    
let checkType judge =
    match judge with
    | Sig (sigma) ->
        sigma |> PreSignature.split
        |> function
        | None -> True |> Continue.label ("3")
        | Some ((alpha, gamma, s), sigma) ->
            match sigma |> PreSignature.tryFind alpha with
            | None -> 
                match s with
                | Star | Rect ->
                    Continue.singleContinue "1" (Ctx (sigma, gamma))
                | A -> 
                    Continue.singleContinue  "2"
                        (Term (sigma, gamma, A, Some Rect))
            | Some a ->
                False |> Continue.label ($"{alpha} is already in signature")
    | Ctx (sigma, gamma) ->
        gamma |> PreContext.split
        |> function
        | None -> Continue.singleContinue "4" (Sig (sigma))
        | Some ((x, A), gamma) ->
            gamma |> PreContext.tryFindVariable x
            |> function
            | None -> 
                Continue.singleContinue "5" (Term (sigma, gamma, A, Some Rect))
            | Some a ->
                False |> Continue.label $"5: duplicate definition of {x}"
    | Term (sigma, gamma, term0, term1) ->
        let c =
            Or [
                let c =
                    match term1 with
                    | None -> False
                    | Some term1 ->
                    match term1 with
                    | Rect -> 
                        Or[
                            Continue.label "6: rect" <| Single (Term (sigma, gamma, term0, Some Star))
                            match term0 with
                            | Pi (A, Some x, B) -> 
                                Continue.label "11"
                                    <| And [
                                        Single (Term (sigma, gamma, A, Some Star))
                                        Single (Term (sigma, PreContext.cons (x, A) gamma, B, Some Rect))
                                    ]
                            | Pi (A, None, B) -> 
                                Continue.label "11" <| And [
                                    Single (Term (sigma, gamma, A, Some Star))
                                    Single (Term (sigma, gamma, B, Some Rect))
                                ]
                            | Eq (Some A, a, b) ->
                                Continue.label "16: eq" <| And [
                                    Single (Term (sigma, gamma, A, Some Rect))
                                    Single (Term (sigma, gamma, a, Some A))
                                    Single (Term (sigma, gamma, b, Some A))
                                ]
                            | Eq(None, a, b) ->
                                let A = inferenceType (sigma, gamma, a) |> fst
                                match A with
                                | None -> False |> Continue.label "a"
                                | Some A -> 
                                    And [
                                        Single (Term (sigma, gamma, A, Some Rect))
                                        Single (Term (sigma, gamma, a, Some A))
                                        Single (Term (sigma, gamma, b, Some A))
                                    ]
                                    |> Continue.label "16: eq with type inference"
                            | _ -> 
                                Continue.label "term rect" <| False 
                        ]
                    | Pi (A, x, B) ->
                        match term0 with
                        | Abs (Some A', x', b) when A = A' && x = Some x' ->
                            // "12"
                            And [
                                Term (sigma, gamma, A, Some Star) |> Single
                                Term (sigma, PreContext.cons (x', A) gamma, b, Some B) |> Single
                            ]
                        | _ -> Or []
                    | App (A, x, B, b, a) -> failwithf "not inmplemented term app"
                    | Eq (Some A, a, a') when a = a' ->
                        match term0 with
                        | Refl a' when a = a' ->
                            And [
                                Single (Term (sigma, gamma, A, Some Rect))
                                Single (Term (sigma, gamma, a, Some A))
                            ] |> Continue.label "eq"
                        | _ -> Or []
                    | _ -> Continue.label "term" False
                    
                c
                match term0 with
                | Variable x ->
                    gamma |> PreContext.tryFindVariable x
                    |> function
                    | None -> False |> Continue.label $"variable {x} is not in context"
                    | Some t ->
                        match term1 with
                        | None -> //x : t
                            Single(Ctx(sigma, gamma)) |> Continue.label "variable"
                        | Some t' ->
                            if t = t' then
                                Single (Ctx (sigma, gamma)) |> Continue.label "variable" 
                            else
                                False |> Continue.label $"type of {x} is not match {t} and {t'}" 
                | Symbol (alpha, f) ->
                    sigma
                    |> PreSignature.tryFind alpha
                    |> function
                    | None ->
                        Continue.label $"symbol {alpha} is not defined" 
                            <| False
                    | Some (alpha, delta, A) ->
                        // :slow?
                        let ctx = Single (Ctx (sigma, gamma))
                        let ld = delta.preContext.Length
                        let lf = f.Length
                        if ld <> lf then   
                            Continue.label $"length not match: {ld} {lf}" <| False
                        else
                        let ls =
                            List.zip (delta.preContext |> List.rev) f
                        let cs = 
                            ls 
                            |> List.fold 
                                (fun (ls, s) ((y, B), f) -> 
                                    match s with 
                                    | None -> 
                                        (Single (Term (sigma, gamma, f, Some B)))::ls
                                        , Some (Map.empty |> Map.add y f)
                                    | Some m ->
                                        printfn "%O" m
                                        let B' = PreTerm.substituteMany m B
                                        (Single (Term (sigma, gamma, f, Some B')))::ls
                                        , (Some (m |> Map.add y f))) 
                                ([], None)
                        match cs with
                        | (_, None) -> 
                            if term1 <> Some A && term1 <> None then
                                Continue.label $"type not match {term1} and {A}" <| False
                            else
                                Continue.label "9: empty" ctx
                        | (ls', Some m) ->
                            let A' = PreTerm.substituteMany m A
                            if term1 <> Some A' && term1 <> None then
                                Continue.label $"type not match {term1} and {A'}" <| False
                            else
                                Continue.label $"9: " <| And [
                                    ctx
                                    yield! ls'
                                ]   
                | App (Some A, Some x, Some B, b, a) ->
                    let B' = PreTerm.substitute x a B
                    if Some B' <> term1 && term1 <> None then
                        Continue.label $"13: type not match {term1} and {B'}" False
                    else
                    And [
                        Single (Term (sigma, gamma, A, Some Star))
                        Single (Term (sigma, PreContext.cons (x, A) gamma, A, Some Star))
                        Single (Term (sigma, gamma, b, Some (Pi(A, Some x, B))))
                    ] |> Continue.label "13: app"
                | App (A, x, B, b, a) ->
                    let A =
                        match A with
                        | Some A -> Some A
                        | None ->
                            inferenceType (sigma, gamma, a) |> fst
                    let (r),m =
                        let B_ = 
                            match B with
                            | Some B -> Some B
                            | None ->
                                inferenceType (sigma, gamma, b) |> fst
                        match B_ with
                        | None -> None, $"type inference failed {b}"
                        | Some B ->
                        match B with
                        | Pi(A',x',B') ->
                            if A = Some A' || A = None then
                                match x' with
                                |  None -> Some (x', A', B, B', B'), "13: app with type inference"
                                |  Some x ->
                                    let B'' = B' |> PreTerm.substitute x a
                                    if Some B'' = term1 || term1 = None then
                                        Some (x', A', B, B', B''), "13: app with type inference"
                                    else
                                        None, $"13: type not match {B''} {term1}"
                            else
                                None, $"type not match {A} {A'}"
                        | _ -> None, $"{B} must be Pi"
                    match r with
                    | None -> False |> Continue.label $"13: {m}"
                    | Some (x, A, B, B', Ba) ->
                        And [
                            Single (Term (sigma, gamma, A, Some Star))
                            let gamma' = gamma |> Option.foldBack (fun x -> PreContext.cons (x, A)) x
                            Single (Term (sigma, gamma', B', Some Rect))
                            Single (Term (sigma, gamma, b, Some B))
                            Single (Term (sigma, gamma, a, Some A))
                        ] |> Continue.label $"13: app {Ba}"
                | _ -> ()
                match term1 with
                | None ->
                    inferenceType (sigma, gamma, term0) |> fst
                    |> function
                    | None -> False |> Continue.label "type inference failed"
                    | Some x -> Single (Term (sigma, gamma, term0, Some x))
                | _ -> False |> Continue.label "not implement dayo"]
        c
    | JEq (sigma, gamma, termL, termR, term) ->
        Or [
            match term with
            | Eq (A, a, a') when a = a' ->
                match termL, termR with
                | (b, Refl a') when a = a' ->
                    Continue.label "19" <| Single (Term (sigma, gamma, b, Some <| Eq(A, a, a)))
                | _ -> False
        ] |> fun a -> a
type R =
    | SingleR of Judgment
    | OrR of R
    | AndR of (R) list
open Utils.NonEmptyTree
let mutable checkTypeMemoMap = Map.empty
let checkTypeMemo j =
    checkTypeMemoMap |> Map.tryFind j
    |> function
    | None -> 
        let res = checkType j
        checkTypeMemoMap <- checkTypeMemoMap |> Map.add j res
        res
    | Some a ->
        a
let checkTypeAllWithMemo memo j = 
    let mutable memo = memo
    let rec checkTypeAll j  =
        let rec cn c =
            match c with
            | Single j -> checkTypeAll j
            | And ls -> 
                let forall f ls =
                    let rec loop ls cnt =
                        match ls with
                        | [] -> Ok ([] |> cnt)
                        | x::xs ->
                            match f x with
                            | Ok x -> loop xs (fun ans ->x::ans |> cnt)
                            | Error x -> Error x
                    loop ls id                                   
                let ls = ls |> List.map cn
                let ls = 
                    ls |> forall (function
                    | Ok a -> Ok a 
                    | Error b -> Error b)
                match ls with
                | Ok ls -> Branch ((j, Some "and"), ls) |> Ok
                | Error e -> Branch ((j, Some "and"), [e]) |> Error
            | Or ls -> 
                let exists f ls =
                    let rec loop ls cnt =
                        match ls with
                        | [] -> Error ([] |> cnt)
                        | x::xs ->
                            match f x with
                            | Ok x -> Ok x
                            | Error x -> loop xs (fun ans -> x::ans |> cnt)
                    loop ls id
                ls|> List.map cn
                |> exists (function
                | Error e -> Error e
                | Ok a -> Ok (a))
                |> function
                | Ok a -> Ok (Branch((j, None), [a]))
                | Error (a) -> Branch ((j, None), a) |> Error
            | False -> Error <| Branch((j, None), [])
            | True -> Ok <| Branch((j, None), [])
            | Label (str, c) -> 
                match cn c with
                | Ok a -> Ok (Branch((j,Some str),[a])) 
                | Error a -> Error (Branch ((j, Some str), [a]))
        memo |> Map.tryFind j
        |> function
        | None -> 
            let res = checkType j 
            let r = res |> cn
            memo <- memo |> Map.add j r
            r
        | Some a -> 
            match a with
            | Ok(Branch ((j, a), ls)) -> Ok (Branch ((j, Some $"memo: {a}"), []))
            | Error (Branch ((j, a), ls)) -> Error (Branch((j, Some $"memo: {a}"), []))
    checkTypeAll j, memo

let checkTypeAll j = checkTypeAllWithMemo Map.empty j |> fst