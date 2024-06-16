module rec Parser.Syntax

open FSharp.Text.Lexing
open FSharp.Text.Parsing
open FSharp.Text.Parsing.ParseHelpers
open Parser.ULF

type Env =
    {
        variables: Map<VariableName, PreTerm>
        symbols: Map<SymbolName, (PreContext * PreTerm)>
        anonymousSymbols: Set<PreContext * PreTerm>
    }
module Env =
    let addVariable a A env =
        {
            env with
                variables = env.variables |> Map.add a A
        }
    let addSymbol alpha (gamma, A) (env: Env) =
        {
            env with
                symbols = env.symbols |> Map.add alpha (gamma, A)
        }
    let addAnonymousSymbols (gamma, A) (env: Env) =
        {
            env with
                anonymousSymbols = env.anonymousSymbols |> Set.add (gamma, A)
        }
    let tryGetSymbol alpha (env: Env) =
        env.symbols 
        |> Map.tryFind alpha
    let empty = {
        variables = Map.empty
        symbols = Map.empty
        anonymousSymbols = Set.empty
    }
type Msg =
    | AddVariable of VariableName * PreTerm
    | AddSymbol of SymbolName * PreContext list * PreTerm
type Cmd<'Msg> =
    | OfMsg of 'Msg
    | Batch of Cmd<'Msg> list

type Builder () =
    [<CustomOperation("addVariable")>]
    member _.AddVariable (f, a, A) =
        fun env ->
            let (r, env) = f env
            let env = env |> Env.addVariable a A
            r, env
    [<CustomOperation("addSymbol")>]
    member _.AddSymbol (f, alpha, (gamma, A)) =
        fun env ->
            let (r, env) = f env
            let env = env |> Env.addSymbol alpha (gamma, A)
            r, env
    [<CustomOperation("tryGetSymbol")>]
    member _.TryGetSymbol (f, alpha) = f >> snd >> fun env -> Env.tryGetSymbol alpha env, env
    member _.Bind(x, f) =
        fun env ->
            let (r, env) = x env
            let (r2, env) = f r env
            r2, env
    member _.YieldFrom (f) = fun env -> f env 
    member _.Yield (x) = fun env -> x, env
    member _.Return (y) = fun env -> y, env
    member _.ReturnFrom (x) = x
    member _.Zero () = fun env -> (), env

type Reader () =
    member _.Bind (x, f) =
        fun env ->
            let r = x env
            f r env
    member _.Return y = fun env -> y
    member _.ReturnFrom x = x
    member _.Zero () = fun env -> ()

let env = new Builder ()


let read = new Reader()
// type Range = {
//     startLine: int
//     startColumn: int
//     endLine: int
//     endColumn: int
//     fileIndex: int
// }
type Range = Position * Position

type SynSymbolIdent =
    {
        symbolIdent: SynLongIdent
        range: Range
    }
    member x.display = x.symbolIdent.display
    member x.ulf = SymbolName x.symbolIdent.ulf
type SynVariableIdent = 
    {
        variableIdent: SynLongIdent
        range: Range
    }
    member x.display: string = x.variableIdent.display
    member x.ulf = x.variableIdent.ulf |> VariableName
type ContextElement = 
    {
        variable: SynVariableIdent
        preTerm: SynPreTerm
        range: Range
    }
    member x.display =
        sprintf "%O: %O" (x.variable.display) (x.preTerm.display) 
    member x.ulf = fun env -> x.variable.ulf, x.preTerm.ulf env 
type SynPreContext =
    {
        preContext: ContextElement list
        range: Range
    }
    member x.display  =
        x.preContext 
        |> Seq.map (fun x -> sprintf "(%O)" (x.display)) 
        |> String.concat " "
    member x.ulf =
        fun env ->
            let ls = x.preContext |> List.map (fun a -> a.ulf env) 
            
            {
                preContext = ls |> List.rev
            }
        
type _SynPreTerm = 
    | Star
    | Rect
    | Symbol of (SynLongIdent * SynPreTerm list)
    | SymbolOrApp of (SynLongIdent * SynPreTerm)
    | SymbolOrVariable of (SynLongIdent)
    | Variable of SynVariableIdent
    | Pi of (SynPreTerm * SynLongIdent option * SynPreTerm)
    | Abs of (SynPreTerm option * SynVariableIdent * SynPreTerm)
    | App of (SynPreTerm option * SynVariableIdent option * SynPreTerm option * SynPreTerm * SynPreTerm)
    | Eq of (SynPreTerm option * SynPreTerm * SynPreTerm)
    | Refl of SynPreTerm
    member x.display: string =
        match x with
        | Star -> "*"
        | Rect -> "□"
        | SymbolOrApp (sym, terms) ->
            let str = terms.display
            sprintf "%O (%O)" (sym.display) str
        | SymbolOrVariable s ->
            let str = s.display
            s.display
        | Variable (v) ->
            v.display
        | Pi (A, a, B) ->
            match a with
            | None -> sprintf "%O -> (%O)" (A.display) (B.display)
            | Some a -> sprintf "(%O : %O) -> (%O)" (A.display) (a.display) (B.display)
        | Abs (A, a, B) ->
            match A with
            | None -> sprintf "λ %O -> %O" (a.display) (B.display)
            | Some A -> sprintf "λ (%O : %O) -> %O" (a.display) (A.display) (B.display)
        | App (_, _, _, A, B) ->
            match A.preTerm with
            | Variable a -> sprintf "%O (%O)" (a.display) (B.display)
            | App a -> sprintf "%O (%O)" (A.display) (B.display)
            | _ -> sprintf "(%O) (%O)" (A.display) (B.display)
        | Eq (_, a, b) ->
            sprintf "%O = %O" (a.display) (b.display)
        | Refl A ->
            sprintf "refl %O" (A.display)
        | Symbol(sym, terms) ->
            let str = terms |> List.map _.display |> String.concat ", "
            sprintf "%O(%O)" (sym.display) str
    member x.ulf =    
        read {
            match x with
            | Star -> return ULF.Star
            | Rect -> return ULF.Rect
            | Symbol (synId, pts) -> 
                return! fun env -> ULF.Symbol (synId.ulf |> ULF.SymbolName, pts |> List.map (fun a -> a.ulf env))
            | SymbolOrApp (synId, pts) -> 
                let synId = synId.ulf
                let symName = ULF.SymbolName synId
                let! a = Env.tryGetSymbol symName
                match a with
                | None -> 
                    return! fun env -> ULF.App (None, None, None, (ULF.Variable (ULF.VariableName synId)), pts.ulf env)
                | Some x ->
                    return! fun env -> ULF.Symbol (symName, [pts.ulf env])
            | SymbolOrVariable (synId) ->
                let synId = synId.ulf
                let synName = ULF.SymbolName synId
                let! a = Env.tryGetSymbol synName
                match a with
                | None ->
                    return! fun env -> ULF.Variable (ULF.VariableName synId)
                | Some a ->
                    return! fun env -> ULF.Symbol (synName, [])

            | Variable (vident) -> return ULF.Variable vident.ulf
            | Pi (A, x, B) -> 
                return! fun env -> ULF.Pi (A.ulf env, x |> Option.map (_.ulf >> ULF.VariableName), B.ulf env)
            | Abs (A, x, B) -> return! fun env -> ULF.Abs (A |> Option.map (fun a -> a.ulf env), x.ulf, B.ulf env)
            | App (A, x, B, a, b) ->
                return! fun env -> ULF.App (A |> Option.map (fun a -> a.ulf env), x |> Option.map _.ulf, B |> Option.map (fun a -> a.ulf env), a.ulf env, b.ulf env)
            | Eq (A, a, b) -> return! fun env -> ULF.Eq (A |> Option.map (fun a -> a.ulf env), a.ulf env, b.ulf env)
            | Refl A -> return! fun env -> ULF.Refl (A.ulf env)
        }
type SynPreTerm = 
    {
        preTerm: _SynPreTerm
        range: Range
    }
    member x.display: string =
        x.preTerm.display
    member x.ulf: Env -> _ = x.preTerm.ulf
type _SynExpr =
    | Paren of expr: SynExpr * leftParenRange: Range * rightParenRange: Range option * range: Range
    | Signature of symbol: SynSymbolIdent option * preContext: SynPreContext * preTerm: SynPreTerm * range: Range
    member x.ulf =
        match x with
        | Signature (alpha, gamma, A, _) ->
            fun env ->
                match alpha with
                | Some alpha -> 

                    alpha.ulf, gamma.ulf env, A.ulf env
                | _ -> Annonymouse (System.Guid.NewGuid()), gamma.ulf env, A.ulf env
        | Paren (expr, _, _, _) -> expr.ulf
type SynExpr =
    {
        expr: _SynExpr
        range: Range
    }
    member x.display =
        match x.expr with
        | Signature (symbol, context, term, _) ->
            match symbol with
            | None ->
                sprintf "%O ⇒ %O" (context.display) (term.display)
            | Some symbol ->
                sprintf "%O: %O ⇒ %O" (symbol.display) (context.display) (term.display)
        | Paren(expr, leftParenRange, rightParenRange, range) -> 
            expr.display
    member x.ulf =
        x.expr.ulf
type SynIdent = 
    {
        str: string
        range: Range
    }
    member x.display = x.str
    member x.ulf: Ident = x.str
type LongIdent = SynIdent list

type SynModule =    
    {
        moduleName: SynLongIdent option
        context: SynPreContext
        syntax: Syntax
        range: Range
    }
    member x.display =
        let name = 
            match x.moduleName with
            | None -> "_"
            | Some name -> name.display
        sprintf "module %O %O where \n%O" name (x.context.display) (x.syntax.display)


type SynOpen = 
    {
        openIdent: SynLongIdent
        public': bool
        range: Range
    }
    member x.display =
        if x.public' then
            sprintf """open %O public""" (x.openIdent.display)
        else
            sprintf "open %O" (x.openIdent.display)
type _SyntaxElement =
    | Module of SynModule
    | Open of SynOpen
    | Expr of SynExpr
    member x.ulf =
        match x with
        | Expr expr -> 
            expr.ulf 
        // | Module m ->
        //     m.ulf
        // | Open x ->
            
type SyntaxElement = 
    {
        syntaxElement: _SyntaxElement
        range: Range
    }
    member x.display =
        match x.syntaxElement with
        | Module m ->
            m.display
        | Open o -> 
            o.display
        | Expr e ->
            e.display
    member x.ulf = x.syntaxElement.ulf 
type Syntax =
    {
        syntax: SyntaxElement list
    }
    member x.display =
        x.syntax |> Seq.map (fun x -> x.display)
        |> String.concat "\n"
    member x.ulf = fun env ->
        x.syntax
        |> List.fold (fun (ls, env) x -> 
            let (a, b, c) =x.ulf env 
            let env = env |> Env.addSymbol a (b, c)
            (a, b, c)::ls
            , env
            ) ([], env)
        |> fst
        |> fun ls -> {
            symbols = ls
        }

// let range (s, t) = {
//     startColumn = s
//     startLine = s
//     endColumn = t
//     endLine = t
//     fileIndex = s    }
// let unionRanges range1 range2 =
//     if range1.fileIndex <> range2.fileIndex then
//         range2
//     else
//     let fileIndex = range1.fileIndex
//     let startColumn = 
//         if range1.startLine < range2.startLine then
//             range1.startColumn
//         elif range1.startLine = range2.startLine then
//             min range1.startColumn range2.startColumn
//         else
//             range2.startColumn
//     let endColumn =
//         if range1.endLine > range2.endLine then
//             range1.endColumn
//         elif range1.endLine = range2.endLine then
//             max range1.endLine range2.endLine
//         else
//             range2.endLine

//     {
//         startLine = min range1.startLine range2.startLine
//         startColumn = startColumn
//         endLine = max range1.endLine range2.endLine
//         endColumn = endColumn
//         fileIndex = fileIndex
//     }
    
type SynLongIdent =
    | SynLongIdent of id: LongIdent * dotRanges: Range list 
    member x.display: string = 
        let (SynLongIdent (id, range)) = x
        id |> Seq.map (fun x -> x.display) |> String.concat "."
    // member this.Range =
    //     match this with
    //     | SynLongIdent ([], _) -> failwith "rangeOfLidwd"
    //     | SynLongIdent ([id], []) -> id.range
    //     | SynLongIdent ([id], [m]) -> unionRanges id.range m
    //     | SynLongIdent (h::t, []) -> unionRanges h.range (List.last t).range
    //     | SynLongIdent (h::t, dotRanges) -> unionRanges h.range (List.last t).range |> unionRanges (List.last dotRanges)
    member x.ulf =
        match x with
        | SynLongIdent (id, dotRanges) -> id |> List.map _.ulf
let (|SynLongIdent|) t =
    match t with
    | SynLongIdent (id, dotRanges) -> SynLongIdent (id, dotRanges)


// type SynOpenTarget =
//     | Module of longId: SynLongIdent * range: Range

type Nesting = (int * Range)
type lexArg = {
    nestings: Nesting list
}
