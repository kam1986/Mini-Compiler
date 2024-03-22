(*
    In this file we make the syntacticall analyses of the code
    building the syntax tree of Mini

    We are building and LR parser by hand i.e. left most, left to right parser.
    we use mutual recursive functions here.
*)
module Parser

open Information
open Lexer
open Syntax

let Parantized f tokens =
    match tokens with
    | { Tag = LEFTPARENTESE } :: tokens ->
        let item, tokens = f tokens
        match tokens with
        | { Tag = RIGHTPARANTESE } :: tokens -> item, tokens
        | token :: _ -> 
            printfn $"at {token.Info.StartsAt} expecting to find a ) but found {token}"
            exit -1
        | _ ->
        printfn $"expecting to find a ), but reached end of content"
        exit -1
    | _ -> 
        printfn $"expecting to find a (, but reached end of content"
        exit -1

let rec ParseValue tokens =
    match tokens with
    | { Tag = REAL } as real :: tokens -> Val (float real.Content) real.Info, tokens

    | _ -> 
        Parantized ParseExpr tokens


and ParseUnary tokens =
    match tokens with
    | { Tag = MINUS } as unary :: tokens ->
        let value, tokens = ParseValue tokens 
        let info = { unary.Info with EndsAt = (GetInfo value).EndsAt }
        Unary Neg value info, tokens
        
    | { Tag = CEIL } as unary :: tokens ->
        let value, tokens = ParseValue tokens 
        let info = { unary.Info with EndsAt = (GetInfo value).EndsAt }
        Unary Ceil value info, tokens
        
    | { Tag = FLOOR } as unary :: tokens ->
        let value, tokens = ParseValue tokens 
        let info = { unary.Info with EndsAt = (GetInfo value).EndsAt }
        Unary Floor value info, tokens

    | { Tag = ROUND } as unary :: tokens ->
        let value, tokens = ParseValue tokens 
        let info = { unary.Info with EndsAt = (GetInfo value).EndsAt }
        Unary Round value info, tokens

    | { Tag = SQRT } as unary :: tokens ->
        let value, tokens = ParseValue tokens 
        let info = { unary.Info with EndsAt = (GetInfo value).EndsAt }
        Unary Sqrt value info, tokens
    // if no unary operator presides we try parsing for a value
    | _ -> ParseValue tokens

and ParseBinary tokens =
    let left, tokens = ParseUnary tokens
    let op =
        match tokens with
        | { Tag = PLUS } :: tokens       -> ValueSome(Binop Add, tokens)
        | { Tag = MINUS } :: tokens      -> ValueSome(Binop Sub, tokens)
        | { Tag = STAR } :: tokens       -> ValueSome(Binop Mul, tokens)
        | { Tag = DASH } :: tokens       -> ValueSome(Binop Div, tokens)
        | { Tag = PROCENTAGE } :: tokens -> ValueSome(Binop Rem, tokens)
        | _ -> ValueNone
    
    match op with
    | ValueSome(op, tokens) ->
        match tokens with
        | { Tag = LEFTPARENTESE } :: _ ->
            let right, tokens = ParseBinary tokens
            let info = Info (GetInfo left).StartsAt (GetInfo right).EndsAt
            Binary op left right info, tokens
        | _ ->
        let right, tokens = ParseBinary tokens
        match right with
        | Binary(op', left', right', info') ->
            match compare op op' with 
            | Less -> 
                let info = Info (GetInfo left).StartsAt (GetInfo right).EndsAt
                Binary op left right info, tokens
            | Greater ->
                let info = Info (GetInfo left).StartsAt (GetInfo left'.Value).EndsAt
                let left = Binary op left left'.Value info
                let info = Info (GetInfo left).StartsAt info'.EndsAt
                Binary op' left right'.Value info, tokens
            | _ -> failwith $"at {info'.StartsAt} cannot determine precedence of {op} and {op'}"
        | _ -> 
            let info = Info (GetInfo left).StartsAt (GetInfo right).EndsAt
            Binary op left right info, tokens
    | _ -> left, tokens

and ParseExpr tokens = ParseBinary tokens


let Parse tokens =
    match ParseExpr tokens with
    | e, [] -> e
    | _ -> failwith "not fully parsed"

