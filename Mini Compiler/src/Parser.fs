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

#nowarn "46"

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
        | { Tag = PLUS } as op :: tokens       when left <! op -> ValueSome(Binop Add op.Info, tokens)
        | { Tag = MINUS } as op :: tokens      when left <! op -> ValueSome(Binop Sub op.Info, tokens)
        | { Tag = STAR } as op :: tokens       when left <! op -> ValueSome(Binop Mul op.Info, tokens)
        | { Tag = DASH } as op :: tokens       when left <! op -> ValueSome(Binop Div op.Info, tokens)
        | { Tag = PROCENTAGE } as op :: tokens when left <! op -> ValueSome(Binop Rem op.Info, tokens)
        | _ -> ValueNone
    
    match op with
    | ValueSome(op, tokens) ->
        match tokens with
        | { Tag = LEFTPARENTESE } as l :: _ when op <!= l ->
            let right, tokens = ParseBinary tokens
            if op <!= right then
                let info = Info (GetInfo left).StartsAt (GetInfo right).EndsAt
                Binary op left right info, tokens
            else
                failwith $"the left an right side of the operation at {(GetInfo left).StartsAt}"
        | _ ->
        let right, tokens = ParseBinary tokens
        match right with
        | Binary(op', left', right', info') when op <!= left' ->
            match compare op op' with 
            | Less -> 
                let info = Info (GetInfo left).StartsAt (GetInfo right).EndsAt
                Binary op left right info, tokens
            | Greater -> 
                let info = Info (GetInfo left).StartsAt (GetInfo left').EndsAt
                let left = Binary op left left' info
                let info = Info (GetInfo left).StartsAt info'.EndsAt
                Binary op' left right' info, tokens
            | _ -> failwith $"at {info'.StartsAt} cannot determine precedence of {op} and {op'}"
        | _ when op <!= right -> 
            let info = Info (GetInfo left).StartsAt (GetInfo right).EndsAt
            Binary op left right info, tokens
        | _ -> failwith ""
    | _ -> left, tokens

and ParseBoolean tokens =
    match tokens with
    | { Tag = TRUE } as bool :: tokens -> True bool.Info, tokens
    | { Tag = FALSE } as bool :: tokens -> False bool.Info, tokens
    | _ -> ParseRelOp tokens
// Stage 2
and ParseRelOp tokens =
    let left, tokens = ParseBinary tokens
    match tokens with
    | { Tag = EQ } as op :: tokens when left <! op ->
        let right, tokens = ParseBinary tokens
        if op <!= right then
            Compare Eq left right (SpanInfo left right), tokens
        else
            failwith ""
    | { Tag = NE } as op :: tokens when left <! op ->
        let right, tokens = ParseBinary tokens
        if op <!= right then
            Compare Ne left right (SpanInfo left right), tokens
        else
            failwith ""

    | { Tag = LE } as op :: tokens when left <! op ->
        let right, tokens = ParseBinary tokens
        if op <!= right then
            Compare Le left right (SpanInfo left right), tokens
        else
            failwith ""

    | { Tag = LT } as op :: tokens when left <! op ->
        let right, tokens = ParseBinary tokens
        if op <!= right then
            Compare Lt left right (SpanInfo left right), tokens
        else
            failwith ""

    | { Tag = GE } as op :: tokens when left <! op ->
        let right, tokens = ParseBinary tokens
        if op <!= right then
            Compare Ge left right (SpanInfo left right), tokens
        else
            failwith ""

    | { Tag = GT } as op :: tokens when left <! op ->
        let right, tokens = ParseBinary tokens
        if op <!= right then
            Compare Ge left right (SpanInfo left right), tokens
        else
            failwith ""

    | _ -> Bool left, tokens


and ParseLogic tokens =
    let left, tokens = ParseBoolean tokens
    match tokens with
    | { Tag = LAND } as op :: tokens when left <! op ->
        let right, tokens = ParseLogic tokens
        if op <!= right then
            Logic And left right (SpanInfo left right), tokens
        else
            failwith ""

    | { Tag = LOR } as op :: tokens when left <! op ->
        let right, tokens = ParseLogic tokens
        match right with
        | Logic(Imply, left', right',_) when op <!= left'-> 
            let left = Logic Or left left' (SpanInfo left left')
            Logic Imply left right' (SpanInfo left right'), tokens

        | Logic(Imply, _, _, _) -> failwith ""
        | _ -> 
            if op <!= right then
                Logic Or left right (SpanInfo left right), tokens
            else
                failwith ""
    | { Tag = LIMPLY } as op :: tokens when left <!= op ->
        let right, tokens = ParseLogic tokens
        Logic Imply left right (SpanInfo left right), tokens
        
    | _ -> left, tokens


// TODO need to use indentation instead of brackets
and ParseIfThenElse tokens =
    match tokens with
    | { Tag = IF } as start :: tokens ->
        let cond, tokens = ParseCond tokens
        match tokens with
        | { Tag = THEN } as t :: tokens when start == t || t =! start ->
            let meet, tokens = ParseExpr tokens
            match tokens with 
            | { Tag = ELSE } as e :: tokens when start == e || e =! start ->
                
                let otherwise, tokens = ParseExpr tokens
                Ite cond meet otherwise (SpanInfo start otherwise), tokens
            | { Tag = ELSE } as t :: _ ->
                failwith $"the 'else' at {t.Info.StartsAt} are not placed correctly"            
            | _ -> failwith "missing 'else' in if expression"
        | { Tag = THEN } as t :: _ ->
            failwith $"the 'then' at {t.Info.StartsAt} are not placed correctly"
        | _ -> failwith "missing 'then' in if expression"
        
    | _ -> ParseBinary tokens

and ParseExpr tokens : _ Expr * Token list = ParseIfThenElse tokens

and ParseCond tokens : _ Cond * Token list= 
    match tokens with
    | { Tag = LEFTPARENTESE } :: _ -> ParseParantized ParseCond tokens
    | _ -> ParseLogic tokens


// we use the option type as return type to distrinquish between error and ending 
and ParseStmt tokens =
    match tokens with
    | [] -> None 
    | { Tag = LET } as dec :: ({ Tag = VAR } as id) :: { Tag = EQ } :: tokens ->
        let body, tokens = ParseExpr tokens
        if dec <! body then
            Some(Declare Imm id.Content body (SpanInfo dec body), tokens)
        else
            failwith $"body of let declaration are not indentet enough at {(GetInfo body).StartsAt}"

    | { Tag = MUT } as dec :: ({ Tag = VAR } as id) :: { Tag = EQ } :: tokens ->
        let body, tokens = ParseExpr tokens
        if dec <! body then
            Some(Declare Mut id.Content body (SpanInfo dec body), tokens)
        else
            failwith $"body of mut declaration are not indentet enough at {(GetInfo body).StartsAt}"

    | { Tag = VAR } as var :: { Tag = ASSIGN } :: tokens ->
        let body, tokens = ParseExpr tokens
        if var <! body then
            Some(Assign var.Content body (SpanInfo var body), tokens)
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
                        (While cond body (SpanInfo w body), tokens)
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
                        | Some(otherwise, tokens) when w <! otherwise -> 
                            When cond meet (Some otherwise) (SpanInfo w otherwise), tokens
                        | _ -> When cond meet None (SpanInfo w e), tokens
                    | _ -> When cond meet None (SpanInfo w meet), tokens
                    |> Some                        
            else
                failwith "the then keyword are not placed synatctically correct"

        | _ -> failwith "when expression missing the then keyword"
    | _ ->
        try
            let ret, tokens = ParseExpr tokens
            Some (Return ret (GetInfo ret), tokens)
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
            (Sequence stmt stmts (SpanInfo stmt stmts), tokens)
            |> Some
            
        | _ -> Some(stmt, tokens)
            

and ParseDeclaration tokens =
    match tokens with
    | { Tag = LET } as dec :: ({ Tag = VAR } as id) :: { Tag = EQ } :: tokens ->
        let body, tokens = ParseExpr tokens
        if dec <! body then
            Some(VariableDec(Imm, id.Content, body, SpanInfo dec body), tokens)
        else
            failwith $"body of let declaration are not indentet enough at {(GetInfo body).StartsAt}"

    | { Tag = MUT } as dec :: ({ Tag = VAR } as id) :: { Tag = EQ } :: tokens ->
        let body, tokens = ParseExpr tokens
        if dec <! body then
            Some(VariableDec(Mut, id.Content, body, SpanInfo dec body), tokens)
        else
            failwith $"body of mut declaration are not indentet enough at {(GetInfo body).StartsAt}"

    | { Tag = FUN } as dec :: ({ Tag = FVAR } as id) :: ({ Tag = LEFTPARENTESE } as lp :: tokens) when dec == id && dec <! lp  ->
        let params, tokens = ParseParams tokens
        ParseStmts tokens
        |> Option.bind (fun (body, tokens) ->
            if dec <! body then
                Some(FunctionDec(id.Content, List.toArray params, body, SpanInfo dec body), tokens)
            else
                None
            )
    | _ -> None
        
(*
    this is a very simple way of parsing parameters
    we may alter this for patterns instead.

    types are not covered yet.
*)
and ParseParams tokens =
    let rec loop params tokens =
        match tokens with
        | ({ Tag = VAR } | { Tag = FVAR} as param) :: { Tag = COMMA } :: tokens -> loop (param.Content :: params) tokens
        | ({ Tag = VAR } | { Tag = FVAR} as param) :: { Tag = RIGHTPARANTESE } :: tokens -> List.rev (param.Content :: params), tokens
        | _ -> failwith ""
    loop [] tokens


and ParseDeclarations tokens =
    ParseDeclaration tokens
    |> Option.map (fun (dec, tokens) ->
        let decs, tokens = ParseDeclarations tokens
        dec :: decs, tokens)
    |> Option.defaultValue ([],[])


let Parse tokens = ParseDeclarations tokens




