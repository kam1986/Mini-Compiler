(*
    In this file we make the lexing fase of the mini langauge.
    As in all other parts this will be extended. 

    lexing is the process of finding tokens i.e words of the language

    we define a token type, which might be altered later to accomedate multi

    The compiler is not meant to be fast but correct, 
    optimisation of the compiler is a project beside the extend of this one
*)
module Lexer

open Information

type Tag = 
    | CEIL
    | FLOOR
    | ROUND
    | SQRT
    | PLUS
    | MINUS
    | STAR
    | DASH
    | PROCENTAGE
    | REAL
    | EQ // Equal
    | NE // Not equal
    | LE // Less than or equal
    | LT // less than
    | GE // greater than or equal
    | GT // greater than
    | NOT
    | LEFTPARENTESE
    | RIGHTPARANTESE
    | LEFTBRACKET
    | RIGHTBRACKET
    | IF | THEN | ELSE 
    | LET | MUT
    | VAR
    | LAND
    | LOR
    | LIMPLY
    | ASSIGN
    | WHILE
    | WHEN
    | DO
    | SEMICOLON
    | TRUE | FALSE


type [<Struct>] Token =
    {
        Tag: Tag        
        Info: Info      
        Content: string 
    }   
with
    interface Information with
        member T.GetInfo = T.Info
   
let Token tag ctnt info =
        {
            Tag = tag
            Info = info
            Content = ctnt
        }

let IsDigit c = '0' <= c && c <= '9'
let IsLower c = 'a' <= c && c <= 'z'
let IsUpper c = 'A' <= c && c <= 'Z'
let IsLetter c = IsLower c || IsUpper c
let IsAlphaNumeric c = IsLetter c || IsDigit c


// rewriting higher order functions to better fit the patterns we need
let Bind f opt src pos = ValueOption.bind f (opt src pos)  
let Map f opt src pos = ValueOption.map f (opt src pos)

let (<|>) opt1 opt2 src pos = ValueOption.orElseWith (fun _ -> opt2 src pos) (opt1 src pos)

let (<&>) opt1 opt2 =
    fun src pos ->
        opt1 src pos
        |> ValueOption.bind (fun (o1, (src, pos))  -> 
            opt2 src pos
            |> ValueOption.map (fun (o2, (src, pos)) -> (o1,o2), (src, pos))
        )

let LexDot src pos =
    match src with
    | '.' :: src -> ValueSome (".", (src, Pos.Move pos 1))
    | _ -> ValueNone

let LexNum src pos =
    let rec loop num src =
        match src with
        | c :: cs when IsDigit c -> loop (num + string c) cs
        | _ when num <> "" -> ValueSome(num, (src, Pos.Move pos num.Length))
        | _ -> ValueNone

    loop "" src

let LexMaybe lex src pos =
    match lex src pos with
    | ValueNone -> ValueSome("", (src, pos))
    | ret -> ret 
    


let LexReal src pos = 
    Map (fun (((num, dot), frac), next) -> num + dot + frac, next) (LexNum <&> LexDot <&> LexMaybe LexNum) <|> LexNum
    |> fun lex -> lex src pos
    |> ValueOption.map (fun (real, (src, pos')) -> 
        Token REAL real (Info pos (Pos.Move pos' -1)), (src, pos')
    )


let LexKeyWord src pos =    
    match src with
    | '&' :: '&' :: src -> 
        let pos' = Pos.Move pos 1
        let token = Token LAND null (Info pos pos')
        ValueSome(token, (src, pos'))
    
    | '|' :: '|' :: src -> 
        let pos' = Pos.Move pos 1
        let token = Token LOR null (Info pos pos')
        ValueSome(token, (src, pos'))

    | '-' :: '>' :: src ->        
        let pos' = Pos.Move pos 1
        let token = Token LIMPLY null (Info pos pos')
        ValueSome(token, (src, pos'))
    

    | '+' :: src -> 
        let token = Token PLUS null (Info pos pos)
        ValueSome(token, (src, pos))
    | '-' :: src -> 
        let token = Token MINUS null (Info pos pos)
        ValueSome(token, (src, pos))
    | '*' :: src -> 
        let token = Token STAR null (Info pos pos)
        ValueSome(token, (src, pos))
    | '/' :: src -> 
        let token = Token DASH null (Info pos pos)
        ValueSome(token, (src, pos))

    | '=' :: src -> 
        let token = Token EQ null (Info pos pos)
        ValueSome(token, (src, pos))
    
    | '<' :: '-' :: src -> 
        let pos' = Pos.Move pos 1
        let token = Token ASSIGN null (Info pos pos')
        ValueSome(token, (src, pos'))
   
    | '<' :: '>' :: src ->
        let pos' = Pos.Move pos 1
        let token = Token NE null (Info pos pos')
        ValueSome(token, (src, pos'))
    
    | '<' :: '=' :: src ->
        let pos' = Pos.Move pos 1
        let token = Token LE null (Info pos pos')
        ValueSome(token, (src, pos'))

    | '<' :: src -> 
        let token = Token LT null (Info pos pos)
        ValueSome(token, (src, pos))
    
    | '>' :: '=' :: src ->
        let pos' = Pos.Move pos 1
        let token = Token GE null (Info pos pos')
        ValueSome(token, (src, pos'))

    | '>' :: src -> 
        let token = Token GT null (Info pos pos)
        ValueSome(token, (src, pos))
    
    | '%' :: src ->
        let token = Token DASH null (Info pos pos)
        ValueSome(token, (src, pos))    

    | 'n' :: 'e' :: 'g' :: src ->
        let pos' = Pos.Move pos 2
        let token = Token PROCENTAGE null (Info pos pos')
        ValueSome(token, (src, pos'))

    | 'c' :: 'e' :: 'i' :: 'l' :: src ->
        let pos' = Pos.Move pos 3
        let token = Token CEIL null (Info pos pos')
        ValueSome(token, (src, pos'))

    | 'f' :: 'l' :: 'o' :: 'o' :: 'r' :: src ->
        let pos' = Pos.Move pos 4
        let token = Token FLOOR null (Info pos pos')
        ValueSome(token, (src, pos'))

    | 'r' :: 'o' :: 'u' :: 'n' :: 'd' :: src ->
        let pos' = Pos.Move pos 4
        let token = Token ROUND null (Info pos pos')
        ValueSome(token, (src, pos'))

    | 's' :: 'q' :: 'r' :: 't' :: src ->
        let pos' = Pos.Move pos 3
        let token = Token SQRT null (Info pos pos')
        ValueSome(token, (src, pos'))

    | 'l' :: 'e' :: 't' :: src ->
        let pos' = Pos.Move pos 2
        let token = Token LET null (Info pos pos')
        ValueSome(token, (src, pos'))

    | 'm' :: 'u' :: 't' :: src ->
        let pos' = Pos.Move pos 2
        let token = Token MUT null (Info pos pos')
        ValueSome(token, (src, pos'))
        
    | 'i' :: 'f' :: src ->
        let pos' = Pos.Move pos 1
        let token = Token IF null (Info pos pos')
        ValueSome(token, (src, pos'))
        
    | 't' :: 'h' :: 'e' :: 'n' :: src ->
        let pos' = Pos.Move pos 3
        let token = Token THEN null (Info pos pos')
        ValueSome(token, (src, pos'))
        
    | 'e' :: 'l' :: 's' :: 'e' :: src ->
        let pos' = Pos.Move pos 3
        let token = Token ELSE null (Info pos pos')
        ValueSome(token, (src, pos'))

    | 't' :: 'r' :: 'u' :: 'e' :: src ->
        let pos' = Pos.Move pos 3
        let token = Token TRUE null (Info pos pos')
        ValueSome(token, (src, pos'))

    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: src ->
        let pos' = Pos.Move pos 4
        let token = Token FALSE null (Info pos pos')
        ValueSome(token, (src, pos'))

    | 'd' :: 'o' :: src ->
        let pos' = Pos.Move pos 1
        let token = Token DO null (Info pos pos')
        ValueSome(token, (src, pos'))

    | 'w' :: 'h' :: 'i' :: 'l' :: 'e' :: src ->
        let pos' = Pos.Move pos 4
        let token = Token WHILE null (Info pos pos')
        ValueSome(token, (src, pos'))

    | 'w' :: 'h' :: 'e' :: 'n' :: src ->
        let pos' = Pos.Move pos 3
        let token = Token WHEN null (Info pos pos')
        ValueSome(token, (src, pos'))


    | _ -> ValueNone
    // moving the position 1 position the the right
    // minimize code size.
    |> ValueOption.map (fun (token, (src, pos)) -> token, (src, Pos.Move pos 1))


let rec LexVar src pos =
    let rec loop id src =
        match src with
        | c :: src when IsAlphaNumeric c -> loop (id + string c) src
        | _ -> id, src

    match src with
    | c :: src when IsLower c -> 
        let id, src = loop (string c) src 
        let token = Token VAR id (Info pos (Pos.Move pos (id.Length-1)))
        ValueSome(token, (src, Pos.Move pos id.Length))

    | _ -> ValueNone


let rec LexWhitespace src pos =

    let rec loop cnt src =
        match src with
        | '\t' :: src 
        | '\r' :: src
        | '\v' :: src
        | ' ' :: src -> loop (cnt + 1) src
        | _ -> cnt, src
        
    let rec loop2 src pos =
        match src with
        | '\n' :: src -> loop2 src (Pos.NewLine pos)
        | (' ' | '\t' | '\r' | '\v') :: src -> 
            let cnt, src = loop 1 src
            loop2 src (Pos.Move pos cnt)
        | _ -> src, pos

    let src, pos' = loop2 src pos
    if pos = pos' then
        ValueNone
    else
        ValueSome("", (src, pos'))


let LexParantese src pos =
    let pos' = Pos.Move pos 1
    match src with
    | '(' :: src -> 
        let token = Token LEFTPARENTESE  null (Info pos pos)
        ValueSome(token, (src, pos'))
    | ')' :: src -> 
        let token = Token RIGHTPARANTESE null (Info pos pos)
        ValueSome(token, (src, pos'))
    | '}' :: src -> 
        let token = Token RIGHTBRACKET  null (Info pos pos)
        ValueSome(token, (src, pos'))
    | '{' :: src -> 
        let token = Token LEFTBRACKET null (Info pos pos)
        ValueSome(token, (src, pos'))
    | _ -> ValueNone


let Src str = List.ofSeq str


let Lex source =
    let pattern =
        LexReal <|> 
        LexKeyWord <|> 
        LexParantese <|>
        LexVar
    
    let rec loop tokens src pos =
        match src with
        | [] -> List.rev tokens
        | _ ->
        match pattern src pos with
        | ValueNone -> 
            match LexWhitespace src pos with
            | ValueNone -> failwith $"onrecognizable token at {pos}"
            | ValueSome(_,(src, pos)) -> loop tokens src pos
        | ValueSome(token, (src, pos)) -> 
            loop (token :: tokens) src pos


    loop [] (Src source) (Position())
    