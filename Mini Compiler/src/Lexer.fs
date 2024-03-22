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
    | LEFTPARENTESE
    | RIGHTPARANTESE



type [<Struct>] Token =
    {
        Tag: Tag        
        Info: Info      
        Content: string 
    }   
   
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


let LexOperator src pos =    
    match src with
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
        let pos' = Pos.Move pos 4
        let token = Token SQRT null (Info pos pos')
        ValueSome(token, (src, pos'))

    | _ -> ValueNone
    // moving the position 1 position the the right
    // minimize code size.
    |> ValueOption.map (fun (token, (src, pos)) -> token, (src, Pos.Move pos 1))

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
        let token = Token LEFTPARENTESE  "" (Info pos pos)
        ValueSome(token, (src, pos'))
    | ')' :: src -> 
        let token = Token RIGHTPARANTESE "" (Info pos pos)
        ValueSome(token, (src, pos'))
    | _ -> ValueNone


let Src str = List.ofSeq str


let Lex source =
    let pattern =
        LexReal <|> 
        LexOperator <|> 
        LexParantese
    
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
    