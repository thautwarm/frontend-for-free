# Frontend-For-Free

A bootstrap of RBNF.hs for generating **standalone** parsers targeting multiple programming languages, where the generated code needs no runtime system.

## Usage

```shell
sh> python setup.py install
sh> stack install .
sh> fff <parser_file>.rbnf <lexer_file>.rlex
```

See examples at `test/list_parser.rbnf` and `test/list_lex.rlex`.

## What is this?

A work-in-progress framework for generating context-free parsers with following properties:

- [x] **LL(k)** capability 
- [x] efficient left recursions
- [ ] cross-language
- [x] **standalone**
- [x] defined with a most intuitive and expressive BNF derivative
    - action/rewrite:

        `pair := a b { ($1, $2) }`
        
    - parameterised polymorphisms for productions:
  
        `nonEmpty[A] := A { [$1] } | hd=A tl=nonEmpty[A] { append(hd, tl) }`
        
        where `append` shall be provided by the user code.

This framework also provided you with a cross-language lexer generator, still WIP.

Currently, 
- the parser generator support for a programming language is hard coded in `src/RBNF/BackEnds/<LanguageName>.hs`.
- the lexer generator support for a programming language is hard coded in `ffflex.py`, any this shall be a function named `<lang>_be`.

   
       