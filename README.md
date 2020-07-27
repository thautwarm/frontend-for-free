# Frontend-For-Free

A bootstrap of RBNF.hs for generating **standalone** parsers targeting multiple programming languages, where the generated code needs no runtime system.

## Installation

### Binary files

You can install binary files via [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).

```shell
sh> stack install .
```

Otherwise, so far Windows binary files are released on GitHub.

Download it from [Releases](https://github.com/thautwarm/frontend-for-free/releases), add `fff-lex` and `fff-pgen` to your PATH.

### Python Wrapper

`frontend-for-free` now supports Python only:

`pip install frontend-for-free` or install it from GitHub.

## Usage

```shell
sh> fff <xxx>.rbnf --trace # note that you should also provide a <xxx>.rlex
sh> ls | grep <xxx>
<xxx>_parser.py <xxx>_lex.py
```

See examples at [runtest](https://github.com/thautwarm/frontend-for-free/tree/cfg/runtest).

## What is Frontend-For-Free?

A framework for generating context-free parsers with following properties:

- [ ] cross-language
- [x] distributed with a lexer generator, but feel free to use your own lexers.
- [x] **LL(k)** capability 
- [x] efficient left recursions
- [x] **standalone**
  No 3rd party library is introduced, while the generator requires Python3.6+ with a few dependencies.
- [x] defined with a most intuitive and expressive BNF derivative
    - action/rewrite:

        `pair := a b { ($1, $2) }`
        
    - parameterised polymorphisms for productions:
  
        `nonEmpty[A] := A { [$1] } | hd=A tl=nonEmpty[A] { tl.append(hd); tl }`
        
        where `append` shall be provided by the user code.

Currently, 
- the parser generator support for a programming language is hard coded in `src/RBNF/BackEnds/<LanguageName>.hs`.
- the lexer generator support for a programming language is hard coded in `ffflex.py`.

   
       