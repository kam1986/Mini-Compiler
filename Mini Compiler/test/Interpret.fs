module Test.Interpret

open Table
open Lexer
open Parser
open Interpret


let test1 =
    "42"
    |> Lex
    |> Parse
    |> InterpretExpr

let test2 = 
    "21 + 21"
    |> Lex
    |> Parse
    |> InterpretExpr

let test3 = 
    "-10.123 + 11 * 2"
    |> Lex
    |> Parse
    |> InterpretExpr

