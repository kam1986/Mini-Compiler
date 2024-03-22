module Test.Lexer

open Information
open Lexer

let testNum() = LexNum (Src "42.2") (Position())
let testReal() = LexReal (Src "42.2") (Position())

let testWhiteSpace() = LexWhitespace (Src "    \n  24") (Position())
let testLex() = Lex "0 + 43 * (23 - 43) / 43"