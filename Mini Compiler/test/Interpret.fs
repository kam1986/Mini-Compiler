module Test.Interpret

open Information
open Table
open Lexer
open Parser
open Syntax
open Interpret


let testValue =
    "42"
    |> Lex
    |> Parse
    |> Interpret Table.empty
    |> printfn "%A"

let testBinary() = 
    "21 + 21"
    |> Lex
    |> Parse
    |> Interpret Table.empty
    |> printfn "%A"

let testPrecedence1() = 
    "-10.123 + 11 * 2"
    |> Lex
    |> Parse
    |> Interpret Table.empty
    |> printfn "%A"

let testIfThenElse =
    "if true then { 42 } else { 0 }"
    |> Lex
    |> Parse
    |> Interpret Table.empty
    |> printfn "%A"

let testVariable() =
    "2.0 + x "
    |> Lex
    |> Parse
    |> Interpret (Table.Bind "x" (Val 2.0 (Info (Position()) (Pos.Move (Position()) 2))) Table.empty)
    |> printfn "%A"