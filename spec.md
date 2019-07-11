

RBNF: a front-end of parser generator.

```
module <LangName> with

import std.layout.{has_next_the_same_indent_as}

external:
    make_tuple

macro:

lexer:
    double_quoted_string = start with '"', end with '"', escape '\\"' to '"'
    int                  = regex '-\d+|\d+'
    const                = 'fun' | 'let' | 'in'

parser:

a = |  a int
    | a

b =  'b' 'c' b

exp =
    | 'if' cond=exp 'then' br1=blocl 'else' br2=block -> If(cond, br1, br2)
    | 'while' cond=exp 'then' block

block =
    | Suite  of hd=exp ?has_next_the_same_indent_as(hd) tl=block
    | Single of value=exp

custom_rewrite =
    | 'a' term1=b term2=c -> make_tuple(term1, term2)
    | 'b' term1=c         -> term1
```