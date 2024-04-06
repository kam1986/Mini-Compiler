
open System

#nowarn "25"

open Test
open Lexer
open Parser
open Interpret
open Syntax
open Table
open TypeChecking


let [<EntryPoint>] Mini args =
    
#if DEBUG
    //Test.Run()
    Lex "fun Test(a)\n   if true -> false then 1 else -1"
    |> Parse
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