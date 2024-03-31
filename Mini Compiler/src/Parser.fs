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


(*
    we here introduce the indentation rules of Mini

    any binary operator must extend to the indentation level or further of its left operand
    
    the body of a declaration must be indented at least one level further than the let/mut keyword

    in the if .. then .. else and when .. do .. otherwise .. the keywords 
    must be aligned on different lines or the whole expression must be on the same line
    and the bodies of the branches must be atleast one indentation level further than
    if/when.

    ex.
        if cond then meet else notmeet

        or

        if cond then
          meet
        else
          notmeet


    while .. do .. has the same rules as if expressions when it comes to keywords, 
    condition and body

    ex.
        while cond do body

        or

        while cond do
          body

        or

        while
          cond
        do
          body

    scoping like paranteses must either be on the same line or the body
    must be on seperate line(s) and both the starting and ending tag of the enclosing
    must be indented at least one indentation level less than the body. 

    statement sequences must either be seperated by ; when on the same line or
    be indented at the same as the previeus statement
*)

// right shift is the same as dividing by a power of 2
let (<!=) (left: #Information) (right: #Information) =
    (left.GetInfo.StartsAt.Offset >>> 1) <= (right.GetInfo.StartsAt.Offset >>> 1)

let (<!) (left: #Information) (right: #Information) =
    (left.GetInfo.StartsAt.Offset >>> 1) < (right.GetInfo.StartsAt.Offset >>> 1)

// different line and same offset
let (=!) (left: #Information) (right: #Information) =
    left.GetInfo.StartsAt.Line < right.GetInfo.StartsAt.Line &&
    left.GetInfo.StartsAt.Offset = right.GetInfo.StartsAt.Offset

let (==) (left: #Information) (right: #Information) = left.GetInfo.StartsAt.Line = right.GetInfo.StartsAt.Line


let SpanInfo left right =
    let pos = (GetInfo left).StartsAt
    let pos'  = (GetInfo right).EndsAt
    Info pos pos'

let rec ParseParantized f tokens =
    match tokens with
    | { Tag = LEFTPARENTESE } as lp :: tokens ->
        let item, tokens = f tokens
        match tokens with
        | { Tag = RIGHTPARANTESE } as rp :: tokens 
            when (lp <! item && rp <! item) || (item == lp && item == rp) ->             
                item, tokens

        | token :: _ -> 
            printfn $"at {token.Info.StartsAt} expecting to find a ) but found {token}"
            exit -1
        | _ ->
        printfn $"expecting to find a ), but reached end of content"
        exit -1
    | _ -> 
        printfn $"expecting to find a (, but reached end of content"
        exit -1


and ParseBracket f tokens =
    match tokens with
    | { Tag = LEFTBRACKET } as lb :: tokens ->
        let item, tokens = f tokens
        match tokens with
        | { Tag = RIGHTBRACKET } as rb :: tokens 
            when (lb <! item && rb <! item) || (item == lb && item == rb) ->             
                item, tokens
        
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

    | { Tag = VAR } as variable :: tokens -> Var variable.Content variable.Info, tokens

    | _ -> ParseParantized ParseExpr tokens


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


// TODO need to use indentation instead of brackets
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

    | _ -> ParseBinary tokens

and ParseExpr tokens = ParseIfThenElse tokens

and ParseCond tokens = 
    match tokens with
    | { Tag = TRUE } as bool :: tokens -> True bool.Info, tokens
    | { Tag = FALSE } as bool :: tokens -> False bool.Info, tokens
    | { Tag = LEFTPARENTESE } :: _ -> ParseParantized ParseCond tokens
    | _ -> ParseLogic tokens


// we use the option type as return type to distrinquish between error and ending 
and ParseStmt tokens =
    match tokens with
    | [] -> None 
    | { Tag = LET } as dec :: ({ Tag = VAR } as id) :: { Tag = EQ } :: tokens ->
        let body, tokens = ParseExpr tokens
        if dec <! body then
            Some(Declare(Imm, id.Content, body, SpanInfo dec body), tokens)
        else
            failwith $"body of let declaration are not indentet enough at {(GetInfo body).StartsAt}"

    | { Tag = MUT } as dec :: ({ Tag = VAR } as id) :: { Tag = EQ } :: tokens ->
        let body, tokens = ParseExpr tokens
        if dec <! body then
            Some(Declare(Mut, id.Content, body, SpanInfo dec body), tokens)
        else
            failwith $"body of mut declaration are not indentet enough at {(GetInfo body).StartsAt}"

    | { Tag = VAR } as var :: { Tag = ASSIGN } :: tokens ->
        let body, tokens = ParseExpr tokens
        if var <! body then
            Some(Assign(var.Content, body, SpanInfo var body), tokens)
        else
            failwith $"body of mut declaration are not indentet enough at {(GetInfo body).StartsAt}"

    | { Tag = WHILE } as w :: tokens ->
        let cond, tokens = ParseCond tokens
        if not(w <! cond) then
            failwith $"condition of the while statement at {(GetInfo cond).StartsAt} are not properly indentet"
        else
        match tokens with
        | { Tag = DO } as d :: tokens ->
            if w == d || w =! d then
                match ParseStmts tokens with
                | None -> failwith $"the while statement at {(GetInfo w).StartsAt} has no body"
                | Some(body, tokens) -> 
                    if w <! body then
                        (While(cond, body, SpanInfo w body), tokens)
                        |> Some
                    else
                        failwith "while loop missing body" 
            else
                failwith "do not correctly for while loop"
        | _ -> failwith "while expression missing do keyword"

    | { Tag = WHEN } as w :: tokens ->
        let cond, tokens = ParseCond tokens
        if not(w <! cond) then
            failwith $"condition of the when statement at {(GetInfo cond).StartsAt} are not properly indentet"
        else
        match tokens with
        | { Tag = THEN } as t :: tokens ->
            if w == t || w =! t then
                match ParseStmts tokens with
                | None -> failwith $"the when statement at {(GetInfo w).StartsAt} has no body for when the condition was meet"
                
                | Some(meet, tokens) -> 
                    if not(w <! meet) then
                        failwith "the first body of the when statement are not indentet correctly"
                    else
                    match tokens with
                    | { Tag = ELSE } as e :: tokens ->
                        if not(w == e || w =! e) then
                            failwith "the else keyword in the when statement are not correctly placed"
                        else
                        match ParseStmts tokens with
                        | Some(otherwise, tokens) when w <! otherwise -> When(cond, meet, Some otherwise, SpanInfo w otherwise), tokens
                        | _ -> When(cond, meet, None, SpanInfo w e), tokens
                    | _ -> When(cond, meet, None, SpanInfo w meet), tokens
                    |> Some                        
            else
                failwith "the then keyword are not placed synatctically correct"

        | _ -> failwith "when expression missing the then keyword"
    | _ ->
        try
            let ret, tokens = ParseExpr tokens
            Some(Return(ret, GetInfo ret), tokens)
        with
            _ -> None

// something is wrong here
and ParseStmts tokens =
    match ParseStmt tokens with
    | None -> None
    | Some(stmt, []) -> Some(stmt, [])
    | Some(stmt, ({ Tag = ELSE } :: _ as tokens)) -> Some(stmt, tokens)
    | Some(stmt, tokens) ->
        match ParseStmts tokens with
        | Some(stmts, tokens) when stmt =! stmts ->
            (Sequence(stmt, stmts, SpanInfo stmt stmts), tokens)
            |> Some
            
        | _ -> Some(stmt, tokens)
            
   

let Parse tokens =
    ParseStmts tokens
    |> Option.defaultWith (fun _ -> failwith "not parsable")




