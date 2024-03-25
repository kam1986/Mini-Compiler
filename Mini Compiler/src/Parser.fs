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

let SpanInfo left right =
    let pos = (GetInfo left).StartsAt
    let pos'  = (GetInfo right).EndsAt
    Info pos pos'

let ParseParantized f tokens =
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


let ParseBracket f tokens =
    match tokens with
    | { Tag = LEFTBRACKET } :: tokens ->
        let item, tokens = f tokens
        match tokens with
        | { Tag = RIGHTBRACKET } :: tokens -> item, tokens
        | token :: _ -> 
            printfn $"at {token.Info.StartsAt} expecting to find a {'}'} but found {token.Tag}"
            exit -1
        | _ ->
        printfn "expecting to find a }, but reached end of content"
        exit -1
    | _ -> 
        printfn "expecting to find a {, but reached end of content"
        exit -1

let rec ParseValue tokens =
    match tokens with
    | { Tag = REAL } as real :: tokens -> Val (float real.Content) real.Info, tokens

    | _ -> 
        ParseParantized ParseExpr tokens


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


// Stage 2
and ParseRelOp tokens =
    let left, tokens = ParseBinary tokens
    match tokens with
    | { Tag = EQ } :: tokens ->
        let right, tokens = ParseBinary tokens
        Compare Eq left right (SpanInfo left right), tokens

    | { Tag = NE } :: tokens ->
        let right, tokens = ParseBinary tokens
        Compare Ne left right (SpanInfo left right), tokens

    | { Tag = LE } :: tokens ->
        let right, tokens = ParseBinary tokens
        Compare Le left right (SpanInfo left right), tokens

    | { Tag = LT } :: tokens ->
        let right, tokens = ParseBinary tokens
        Compare Lt left right (SpanInfo left right), tokens

    | { Tag = GE } :: tokens ->
        let right, tokens = ParseBinary tokens
        Compare Ge left right (SpanInfo left right), tokens

    | { Tag = GT } :: tokens ->
        let right, tokens = ParseBinary tokens
        Compare Ge left right (SpanInfo left right), tokens

    | _ -> Bool left, tokens

and ParseLogic tokens =
    let left, tokens = ParseRelOp tokens
    match tokens with
    | { Tag = LAND } :: tokens ->
        let right, tokens = ParseLogic tokens
        Logic And left right (SpanInfo left right), tokens

    | { Tag = LOR } :: tokens ->
        let right, tokens = ParseLogic tokens
        match right with
        | Logic(Imply, left', right',_) -> 
            let left = Logic Or left left' (SpanInfo left left')
            Logic Imply left right' (SpanInfo left right'), tokens

        | _ -> Logic Or left right (SpanInfo left right), tokens

    | { Tag = LIMPLY } :: tokens ->
        let right, tokens = ParseLogic tokens
        Logic Imply left right (SpanInfo left right), tokens
        
    | _ -> left, tokens


and ParseVarBind tokens =
    match tokens with
    | { Tag = LET } as bind :: ({ Tag = VAR } as name) :: { Tag = EQ } :: tokens ->
        let e, tokens = ParseExpr tokens
        Let name.Content (Info bind.Info.StartsAt (GetInfo e).EndsAt), tokens

    | { Tag = MUT } as bind :: ({ Tag = VAR } as name) :: { Tag = EQ } :: tokens ->
        let e, tokens = ParseExpr tokens
        Mut name.Content (Info bind.Info.StartsAt (GetInfo e).EndsAt), tokens
    | _ -> ParseBinary tokens

and ParseIfThenElse tokens =
    match tokens with
    | { Tag = IF } as start :: tokens ->
        let cond, tokens = ParseCond tokens
        match tokens with
        | { Tag = THEN } :: tokens ->
            let meet, tokens = ParseBracket ParseExpr tokens
            match tokens with 
            | { Tag = ELSE } :: tokens ->
                let otherwise, tokens = ParseBracket ParseExpr tokens
                Ite cond meet otherwise (SpanInfo start otherwise), tokens
            | _ -> failwith "missing 'else' in if expression"
        | _ -> failwith "missing 'then' in if expression"

    | _ -> ParseVarBind tokens

and ParseExpr tokens = ParseIfThenElse tokens

and ParseCond tokens = ParseLogic tokens

let Parse tokens =
    match ParseExpr tokens with
    | e, [] -> e
    | _ -> failwith "not fully parsed"

