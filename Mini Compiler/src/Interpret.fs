(*
    This file contains the semantical interpretation
    of the Mini Language

    Stage 2 comments
        
        We need to address some different implementation ways

        when binding and referencing to values, there is a meriot of
        ways we can do it. there is two major types of value passing.
        1. Pass by value
            this is simply copy the whole value
            it is often the fastet and safest type
            to pass a value, if the value are small and immutable.


        2. Pass by reference
            this is when we pass a reference to the value instead.
            in F# it is equalivalent to the ref type for heap, class instance passing
            for heap based values and byref for stack based values. 
            In F# all records and DU are reference passed too, and
            as long as they are immutable (not assigned with mutable) 
            it is no problem, but as in many imperative and OO langauge
            this is cause to problem of runtime problem that is hard to
            catch. Rust addresses this problem by the borrow checker, 
            and scoping rules.

        how you implement this will affect performance, and optimization possibilities
        We will take a rather strict way of value passing, implementing both of these
        patterns but doing it explicit and distiquish between mutable and immutable
        value declaration.

        at some point we will change the language to be indentation sensitive
        hence make rules on structual layout on the placement inbetween token
        groups. f.x. the body of a declaration should be indentet more
        than the let/mut, and so forth. this is a different choice than
        C like langauges, and will be more like Pythen and the ML family
        like F# syntactic features. I prefer this syntax over brackets because
        all common code guidelines already states that a body should
        be indentet as above, statements should be indented equaly
        so to include another form of nesting feature is only to make
        the parsing fase easier, and allow for the use of Parser generators.

        Site note:
            a parser/lexer generator is a language of it's own
            a lexer generator takes some preabmle of the target langauge
            some "regex * action" patterns where the 
            regex is a language describing complex patterns of tokens
            In a late stage of Mini I intent to implement pattern 
            deconstruction/matching like the match .. with in F#
            here I will try to implement regex as a pattern choice
            for strings.

            The lexer generated from a lexer generator are some of the fastest
            you can get, and most handwritten lexers are slower than them.
            it uses a DFA (deterministic finite automata) to get a runtime of 
            O(n) with a rather small constant overheath, meaning it can generate a
            correct tokens sequence in linear time without lookahead or branching.
            such form for lexer are also really small in memory compsumtion for ASCII
            because it uses a jumbtable (array), it can typically be in memory for a ASCII
            lexer it takes up 256^2 bytes for the table, down to 256 bits for a table that checks
            if we are at a point that allow for emiting a token or it should keep searching
            or emites an error. the code running the lexer are the same independent on the table.
            hence implementing multiple tables, do only extent the memoryfootprint by a table and accept table.
            for bigger classes of langauges like utf-18 we either fall back to byte by byte parsing to minimize
            the table or use a different type of table to minimize the table.

            the parser generated from a parser generator are also faster than most
            handwritten parsers, but it limits the langauges we can make. Most 
            parser generators today can't handle context sensitivity langauge like
            Python, F#, Haskell, Ocaml and more. We can make some hacks that allow them to 
            do so however. it is however possible to implement a parser generator that
            check for indentation with a runtime of O(n) like the lexer above.
            the tables of these are larger and most often they will be some smart form of
            jump tables. the types are LR(n) parsers where the 'n' express the number of lookaheads
            it allow, it will still run in linear runtime, but the greater the number of n the
            larger the table. 

            as a fun exercise in the later part of the implementation, we can implement 
            both a lexer and a context sensitive parser generator. 




*)
module Interpret
//#nowarn "25"

open Syntax
open Table
open Lexer
open Parser

let rec InterpretExpr vtab expr =
    match expr with
    | Val _ as v -> v
    | Unary(op, e, info) ->
        let (Val(v, _)) = InterpretExpr vtab e.Value
        match op with
        | Neg -> Val -v info
        | Ceil -> Val (ceil v) info
        | Floor -> Val (floor v) info
        | Round -> Val (round v) info
        | Sqrt -> Val (sqrt v) info

    | Binary(op, left, right, info) ->
        let (Val(left, _))  = InterpretExpr vtab left.Value
        let (Val(right, _)) = InterpretExpr vtab right.Value
        match op.BinOp with
        | Add -> Val (left + right) info
        | Sub -> Val (left - right) info
        | Mul -> Val (left * right) info
        | Div -> Val (left / right) info
        | Rem -> Val (left % right) info

    // in stage 2 we havn't yet implemented statements so we have no formal
    // way of binding variables yet.
    | Variable(id, _) -> 
        Lookup id vtab
        
    | Cond c -> 
        InterpretCond vtab c
        |> Cond

    | IfThenElse(cond, meet, otherwise, _) ->
        match InterpretCond vtab cond with
        | True _ -> InterpretExpr vtab meet
        | _ -> InterpretExpr vtab otherwise


and InterpretCond vtab cond =
    match cond with
    | True _ | False _ -> cond
    | Not(cond, _) ->
        match InterpretCond vtab cond with
        | True info -> False info
        | False info -> True info
        | _ -> failwith ""

    | Logic(op, left, right, info) ->
        let left = InterpretCond vtab left
        match op, left with
        | And, False _
        | Or, True _ -> left
        | Imply, False _ -> True info
        | Or, _ | And, _ | Imply, _ -> InterpretCond vtab right
        
    | Bool e ->
        let (Val(v, info)) = InterpretExpr vtab e
        if v <> 0. then
            True info
        else
            False info

    | Compare(op, left, right, info) ->
        let (Val(left, info)) = InterpretExpr vtab left
        let (Val(right, _)) = InterpretExpr vtab right
        let b =
            match op with
            | Eq -> left =  right
            | Ne -> left <> right
            | Le -> left <= right
            | Lt -> left <  right
            | Ge -> left >= right
            | Gt -> left >  right
        if b then
            True info
        else
            False info


and InterpretStmt vtab stmt =
    match stmt with
    | Assign(adr, value, info) ->
        let value = InterpretExpr vtab value
        Update adr value vtab 
        |> ValueOption.iter (fun _ -> ())
        None

    | When(cond, meet, otherwise, _) ->
        match InterpretCond vtab cond with
        | True _ -> InterpretStmt vtab meet
        | _ -> Option.bind (InterpretStmt vtab) otherwise
            
    | While(cond, body, info) ->
        let mutable run = 
            match InterpretCond vtab cond with
            | True _ -> true
            | _ -> false
        
        while run do
            InterpretStmt vtab body 
            |> ignore
            run <-
                match InterpretCond vtab cond with
                | True _ -> true
                | _ -> false
        None

    | Return(ret, _) -> 
        InterpretExpr vtab ret
        |> Some

    | Sequence(Declare(_, name, value, _), next, _) ->
        let value = InterpretExpr vtab value
        let vtab = Table.Bind name value vtab
        InterpretStmt vtab next

    | Sequence(first, next, _) ->
        InterpretStmt vtab first
        |> Option.map (fun _ -> failwith "error")
        |> Option.orElseWith (fun _ -> InterpretStmt vtab next)
     
    // declarens without a following statement are semantically meaningless
    // so we ignore them
    | Declare _ -> None

let Interpret vtab input = InterpretStmt vtab input