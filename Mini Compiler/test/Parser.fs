module Test.Parser

open Lexer
open Parser

let testValue() = 
    "42"
    |> Lex
    |> ParseExpr

let testBinary() = 
    "22 + 22"
    |> Lex
    |> ParseExpr

let testPrecedence1() =
    "22 + 11 * 2"
    |> Lex
    |> ParseExpr

let testPrecedence2() =
    "22 * 11 - 2"
    |> Lex
    |> ParseExpr


let testNestedExpr1() =
    "(22 + 11) * 2"
    |> Lex
    |> ParseExpr

// Error
let testNestedExpr2() =
    "2 * (22 + 11)"
    |> Lex
    |> ParseExpr


let RunLex() = ()
    

