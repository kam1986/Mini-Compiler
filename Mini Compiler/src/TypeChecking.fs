module TypeChecking

(*
    we have for now only tree types of output.
    either we have a value of type real or a value of boolean type
    or no return in C like languages they use void, which are in my point of veiw
    missleading, we use the ML (meta langauge) type called unit, which simply are the
    type where there is only one item in the set of types of unit. void on the other hand
    are a lake of information of return type hence it could be interpret as another type
*)

open Table
open Syntax

type Type = voption<bool>
// these are a handy way to cover all current types
// but also limit them to only them by combining two types with two options
let real = ValueSome true  : Type
let bool = ValueSome false : Type
let unit = ValueNone : Type

// for now this do only check that they are the same
// this will change later to full type inference
let rec Unify t1 t2 : Type =
    if t1 = t2 then t1
    else failwith "not unifiable"


let rec ValidateExpr vtab expr =
    match expr with
    | Val _ -> real
    | Variable(name, _) -> Lookup name vtab
    | Unary(_, body, _) -> ValidateExpr vtab !body
    | Binary(_,left, right,_) ->
        let left = ValidateExpr vtab !left
        let right = ValidateExpr vtab !right
        Unify left right
    | Cond c -> 
        ValidateCond vtab c
        
    | IfThenElse(cond, meet, otherwise, _) ->
        let cond = ValidateCond vtab !cond
        if cond <> bool then
            failwith ""
        else
        let meet = ValidateExpr vtab !meet
        let otherwise = ValidateExpr vtab !otherwise
        Unify meet otherwise


and ValidateCond vtab cond = 
    match cond with
    | True _ | False _ -> bool
    | Not(cond, info)  -> ValidateCond vtab !cond
    | Logic(_, left, right, _) ->
        let left = ValidateCond vtab !left
        let right = ValidateCond vtab !right
        if left = bool then
            Unify left right
        else
            failwith ""

    | Compare(op, left, right, _) ->
        let left = ValidateExpr vtab !left
        let right = ValidateExpr vtab !right
        Unify left right
        |> ignore
        bool

    | Bool e -> ValidateExpr vtab !e


and ValidateStmt vtab stmt = 
    match stmt with
    | Assign(name, value, _) ->
        let value = ValidateExpr vtab !value
        let name = Lookup name vtab
        Unify name value
        |> ignore
        unit

    | When(cond, meet, otherwise, _) ->
        let cond = ValidateCond vtab !cond
        if cond <> bool then
            failwith ""
        else
        let meet = ValidateStmt vtab !meet
        Option.map (ValidateStmt vtab) !otherwise
        |> Option.map (fun otherwise -> Unify meet otherwise)
        |> Option.defaultWith (fun _ -> Unify meet unit)

    | While(cond, body, _) ->
        let cond = ValidateCond vtab !cond
        if cond <> bool then
            failwith ""
        else
        let body = ValidateStmt vtab !body
        Unify body unit

    | Return(ret, _) -> ValidateExpr vtab !ret

    | Sequence({ contents = Declare(_, name, value, _) }, next, _) ->
        let value = ValidateExpr vtab !value
        let vtab = Bind name value vtab
        ValidateStmt vtab !next



    | Sequence(first, next, _) ->
        let first = ValidateStmt vtab !first
        let next = ValidateStmt vtab !next
        // we needs to differentiate between cases.
        // the first can be unit and the next can be of any type
        // or next can have a branch where it return and therefore
        // return a value of some type
        if first = unit then next
        else Unify first next
        
    | _ -> unit