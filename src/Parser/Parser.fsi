// Signature file for parser generated by fsyacc
module Parser.Parser
type token = 
  | EOF
  | OUTDENT
  | INDENT
  | BLOCKEND
  | BLOCKBEGIN
  | EQ
  | COLON
  | DARROW
  | ARROW
  | SEMICOLON
  | REFL
  | RECT
  | STAR
  | JEQ
  | SPACE
  | NEWLINE
  | MODULE
  | WHERE
  | PUBLIC
  | OPEN
  | IMPORT
  | COMMA
  | DOT
  | RPAREN
  | LPAREN
  | LAMBDA
  | UNDER_BAR
  | IDENT of (string)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_OUTDENT
    | TOKEN_INDENT
    | TOKEN_BLOCKEND
    | TOKEN_BLOCKBEGIN
    | TOKEN_EQ
    | TOKEN_COLON
    | TOKEN_DARROW
    | TOKEN_ARROW
    | TOKEN_SEMICOLON
    | TOKEN_REFL
    | TOKEN_RECT
    | TOKEN_STAR
    | TOKEN_JEQ
    | TOKEN_SPACE
    | TOKEN_NEWLINE
    | TOKEN_MODULE
    | TOKEN_WHERE
    | TOKEN_PUBLIC
    | TOKEN_OPEN
    | TOKEN_IMPORT
    | TOKEN_COMMA
    | TOKEN_DOT
    | TOKEN_RPAREN
    | TOKEN_LPAREN
    | TOKEN_LAMBDA
    | TOKEN_UNDER_BAR
    | TOKEN_IDENT
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstartSyntax
    | NONTERM__startstart
    | NONTERM__startsyntax
    | NONTERM__startlongIdent
    | NONTERM__startcontextElm
    | NONTERM__startterm
    | NONTERM__startsymbolArgs
    | NONTERM__startsymbolLongIdent
    | NONTERM__startident
    | NONTERM__startsyntaxElm
    | NONTERM__startsyntaxElms
    | NONTERM_start
    | NONTERM_module
    | NONTERM_longIdentOrUnderBar
    | NONTERM_startSyntax
    | NONTERM_syntax
    | NONTERM_syntaxElms
    | NONTERM_syntaxElm
    | NONTERM_expr
    | NONTERM_open
    | NONTERM_openImport
    | NONTERM_context
    | NONTERM_context'
    | NONTERM_contextElm
    | NONTERM_variableIdent
    | NONTERM_term_
    | NONTERM_term
    | NONTERM_termElement_
    | NONTERM_termElement
    | NONTERM_termElementOrParen
    | NONTERM_symbolArgs
    | NONTERM_symbolOrApp
    | NONTERM_app
    | NONTERM_longIdent
    | NONTERM_longIdent'
    | NONTERM_symbolIdent
    | NONTERM_symbolLongIdent
    | NONTERM_pi
    | NONTERM_abs
    | NONTERM_eq
    | NONTERM_refl
    | NONTERM_terms
    | NONTERM_terms'
    | NONTERM_ident
    | NONTERM_symbolIdentOrUnderBar
    | NONTERM_signature_left
    | NONTERM_signature
    | NONTERM_end
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val startSyntax : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (Syntax) 
val start : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (SynModule) 
val syntax : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (Syntax) 
val longIdent : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (SynLongIdent) 
val contextElm : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (ContextElement) 
val term : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (SynPreTerm) 
val symbolArgs : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (SynPreTerm list) 
val symbolLongIdent : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (SynSymbolIdent) 
val ident : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (SynIdent) 
val syntaxElm : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (SyntaxElement) 
val syntaxElms : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (SyntaxElement list) 
