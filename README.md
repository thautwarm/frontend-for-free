# Frontend-For-Free

A bootstrap of RBNF.hs for generating **standalone** parsers targeting multiple programming languages, where the generated code needs no runtime system.

## Usage

```shell
sh> python setup.py install
sh> stack install .
sh> fff <xxx>.rbnf --trace # note that you should also provide a <xxx>.rlex
sh> ls | grep <xxx>
<xxx>_parser.py <xxx>_lex.py
```

See examples at [runtest](https://github.com/thautwarm/frontend-for-free/tree/cfg/runtest).

## What is this?

A work-in-progress framework for generating context-free parsers with following properties:

- [x] **LL(k)** capability 
- [x] efficient left recursions
- [ ] cross-language
- [x] **standalone**(while the generator requires Python3.6+ with a few dependencies)
- [x] defined with a most intuitive and expressive BNF derivative
    - action/rewrite:

        `pair := a b { ($1, $2) }`
        
    - parameterised polymorphisms for productions:
  
        `nonEmpty[A] := A { [$1] } | hd=A tl=nonEmpty[A] { append(hd, tl) }`
        
        where `append` shall be provided by the user code.

This framework also provided you with a cross-language lexer generator, still WIP.

Currently, 
- the parser generator support for a programming language is hard coded in `src/RBNF/BackEnds/<LanguageName>.hs`.
- the lexer generator support for a programming language is hard coded in `ffflex.py`.

   
       