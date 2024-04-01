﻿(*
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


    we use reference cells to minize memory allocation and being able to change the syntax tree inline
    in optimization and 
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
    | Variable of 'id * Info
    | Unary of UnOp * 'id Expr ref * Info
    | Binary of BinOp * 'id Expr ref * 'id Expr ref * Info
    | Cond of 'id Cond 
    | IfThenElse of 'id Cond ref * 'id Expr ref * 'id Expr ref * Info
with
    interface Information with
        member E.GetInfo =
            match E with
            | Val(_,info)
            | Variable(_,info)
            | Unary(_,_,info)
            | Binary(_,_,_,info)
            | IfThenElse(_,_,_,info) -> info
            | Cond c -> GetInfo c


and 'id Cond = 
    | True  of Info
    | False of Info
    | Not   of 'id Cond ref * Info
    | Logic of LogOp * 'id Cond ref * 'id Cond ref * Info
    | Compare of RelOp * 'id Expr ref * 'id Expr ref * Info
    | Bool of 'id Expr ref // true > 0 and false = 0
with
    interface Information with
        member C.GetInfo =
            match C with
            | True info | False info
            | Not(_,info)
            | Logic(_,_,_,info)
            | Compare(_,_,_,info) -> info
            | Bool v -> GetInfo !v


type 'id Stmt =
    | Declare of Mutability * 'id * 'id Expr ref * Info
    | Assign of 'id * 'id Expr ref * Info
    | When of 'id Cond ref * 'id Stmt ref * 'id Stmt option ref * Info
    | While of 'id Cond ref * 'id Stmt ref * Info
    | Sequence of 'id Stmt ref * 'id Stmt ref * Info
    | Return of 'id Expr ref * Info
with
    interface Information with
        member S.GetInfo =
            match S with
            | Declare(_,_,_,info)
            | Assign(_,_, info)
            | When(_,_,_,info)
            | While(_,_,info)
            | Sequence(_,_,info)
            | Return(_,info) -> info



let Declare mut id body info = Declare(mut, id, ref body, info)
let Assign id value info = Assign(id, ref value, info)
let When cond meet otherwise info =  When(ref cond, ref meet, ref otherwise, info)
let While cond body info = While(ref cond, ref body, info)
let Sequence first next info = Sequence(ref first, ref next, info)
let Return ret info = Return(ref ret, info)

let Not cond info = Not(ref cond, info)
let Logic op c1 c2 info = Logic(op, ref c1, ref c2, info)
let Compare op e1 e2 info = Compare(op, ref e1, ref e2, info)
let Bool e = Bool (ref e)

let Val v info = Val(v, info)
let Unary op expr info = Unary(op, ref expr, info)
let Binary op left right info = Binary(op, ref left, ref right, info)
let Ite cond meet otherwise info = IfThenElse(ref cond, ref meet, ref otherwise, info)

// we make 3 destinct binding functions to enhance readability
let Var name info = Variable(name, info)
