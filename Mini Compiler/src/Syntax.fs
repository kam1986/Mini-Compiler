(*
    This file contains the syntax of the Mini language
    it will be extended as we evovle the language
*)
module Syntax

open System
open System.Collections


open Information
(*
    Stage 1
    
    for now we introduce the most basic arimthetics of an expression, we do only allow for floating point values
    and do not include decleration of variables or function calls because functions are not defined yet.
    
    we do though implement operations as generalized as some unary operation or binary operation marked by a prefix of types UnOp or BinOp.
    This way we can reduce rewriting and implementation of new operations later on.
*)

type UnOp =
    | Neg
    | Ceil
    | Floor
    | Round
    | Sqrt

type Association = Left | Right 


type Order = Less | Equal | Greater

// implement a order interface and a compare function
type ICompare<'item> = abstract member Compare: 'item -> Order

let compare<'item when 'item :> ICompare<'item>> 
    (left: 'item) (right: 'item) = left.Compare right


type Bin =
    | Add
    | Sub
    | Rem
    | Mul
    | Div 
with 
    override B.ToString() =
        match B with
        | Add -> "+"
        | Sub -> "-"
        | Rem -> "%"
        | Mul -> "*"
        | Div -> "/" 


type LogOp =
    | Imply
    | Or
    | And

type RelOp = Eq | Ne | Le | Lt | Ge | Gt

type BinOp =
    {
        BinOp: Bin
        Association: Association
        Precedence: int
    }
with
    override B.ToString() = string B.BinOp
    interface ICompare<BinOp> with
        member B.Compare b =
            if B.Precedence = b.Precedence then
                if B.Association <> b.Association then
                    Equal
                elif B.Association = Left then
                    Less
                else
                    Greater
            else
            match B.Precedence.CompareTo b.Precedence with
            | 0 -> Equal
            | p when p < 0 -> Less
            | _ -> Greater


let Binop op = 
    {
        BinOp = op
        Association =
            match op with
            | _ -> Left
        Precedence =
            match op with
            | Add | Sub -> 1
            | Rem -> 2
            | Mul | Div -> 3
            
    }

type Mutability =
    | Mut
    | Imm

// we use a generic parameter for variable names, because we at later stages need to reference to them by index
type 'id Expr =
    | Val of float * Info
    | Variable of Mutability * 'id * Info
    | Unary of UnOp * 'id Expr ref * Info
    | Binary of BinOp * 'id Expr ref * 'id Expr ref * Info
    | Cond of 'id Cond 
    | IfThenElse of 'id Cond * 'id Expr * 'id Expr * Info
with
    interface Information with
        member E.GetInfo =
            match E with
            | Val(_,info)
            | Variable(_,_,info)
            | Unary(_,_,info)
            | Binary(_,_,_,info)
            | IfThenElse(_,_,_,info) -> info
            | Cond c -> GetInfo c


and 'id Cond = 
    | Not of 'id Cond * Info
    | Logic of LogOp * 'id Cond * 'id Cond * Info
    | Compare of RelOp * 'id Expr * 'id Expr * Info
    | Bool of 'id Expr // true > 0 and false = 0
with
    interface Information with
        member C.GetInfo =
            match C with
            | Not(_,info)
            | Logic(_,_,_,info)
            | Compare(_,_,_,info) -> info
            | Bool v -> GetInfo v

let Not cond info = Not(cond, info)
let Logic op left right info = Logic(op, left, right, info)
let Compare op left right info = Compare(op, left, right, info)


let Val v info = Val(v, info)
let Unary op expr info = Unary(op, ref expr, info)
let Binary op left right info = Binary(op, ref left, ref right, info)
let Ite cond meet otherwise info = IfThenElse(cond, meet, otherwise, info)

// we make 3 destinct binding functions to enhance readability
let Var mut name info = Variable(mut, name, info)
let Let name info = Var Imm name info
let Mut name info = Var Mut name info