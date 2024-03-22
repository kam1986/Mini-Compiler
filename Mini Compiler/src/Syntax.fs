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

type Expr =
    | Val of float * Info
    | Unary of UnOp * Expr ref * Info
    | Binary of BinOp * Expr ref * Expr ref * Info
with
    interface Information with
        member E.GetInfo =
            match E with
            | Val(_,info)
            | Unary(_,_,info)
            | Binary(_,_,_,info) -> info

let Val v info = Val(v, info)
let Unary op expr info = Unary(op, ref expr, info)
let Binary op left right info = Binary(op, ref left, ref right, info)

