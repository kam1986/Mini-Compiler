(*
    This file contains the semantical interpretation
    of the Mini Language
*)
module Interpret
#nowarn "25"

open Syntax
open Table
open Lexer
open Parser

let rec InterpretExpr expr =
    match expr with
    | Val _ as v -> v
    | Unary(op, e, info) ->
        let (Val(v, _)) = InterpretExpr e.Value
        match op with
        | Neg -> Val -v info
        | Ceil -> Val (ceil v) info
        | Floor -> Val (floor v) info
        | Round -> Val (round v) info
        | Sqrt -> Val (sqrt v) info

    | Binary(op, left, right, info) ->
        let (Val(left, _))= InterpretExpr left.Value
        let (Val(right, _)) = InterpretExpr right.Value
        match op.BinOp with
        | Add -> Val (left + right) info
        | Sub -> Val (left - right) info
        | Mul -> Val (left * right) info
        | Div -> Val (left / right) info
        | Rem -> Val (left % right) info

    
let Interpret input = InterpretExpr input