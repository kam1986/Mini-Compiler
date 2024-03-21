# Toy compiler
This is a toy compiler for a generic programming language.
It is meant as a step by step implementation of a programming language.

we start by making some of the language as an interpreter, and when we comes to a point
where we can make a single file program for the most simple language, we extend it into
compilation to WASM and later LLVM.

## Interpretation
### first stage
Here we define the initial structure of values and expression.
In this stage we do not include anything else than the bare minimum.
we make the lexer and the parser for these structures and test that they
function correctly.
i.e. we simply implement the calculator.


## Compilelation
