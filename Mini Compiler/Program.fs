
open System

#nowarn "25"

open Test
open Lexer
open Parser
open Interpret
open Syntax
open Table

let [<EntryPoint>] Mini args =
    
#if DEBUG
    //Test.Run()
    Lex "mut count = 0\nwhile count < 10 do\n  count <- count + 1\ncount"
    |> ParseStmts
    |> Option.map fst
    |> Option.bind (Interpret Table.empty)
    |> printfn "%A"
#else
    let mutable running = true
    while running do
        let input = Console.ReadLine()
        running <- "exit" <> input && "quit" <> input
        if running then
            input
            |> Lex
            |> Parse
            |> Interpret 
            |> fun (Val(v,_)) -> printfn $"it: {v}"
        
    
#endif
    0