# Frontend-For-Free

A bootstrap of RBNF.hs to generate **standalone** parsers targeting multiple programming languages.

**Standalone**: the generated code can run without runtime dependencies other than the language and standard libraries.

## Installation

### Install from Sources

You can install binary files via: [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).

```shell
sh> stack install .
```

### Install from Binaries

Otherwise, so far Windows binary files are released on GitHub.

Download it from [Releases](https://github.com/thautwarm/frontend-for-free/releases), add `fff-lex` and `fff-pgen` to your PATH.

### Python Wrapper

`frontend-for-free` now supports Python only:

`pip install frontend-for-free` or install it from GitHub.

## Usage

```shell
sh> fff <xxx>.rbnf --trace [--lexer_out <xxx>_lex.py] [--parser_out <xxx>_parser.py] 
sh> # note that you should also provide a <xxx>.rlex file
sh> ls | grep <xxx>
<xxx>_parser.py <xxx>_lex.py
```

See examples at [runtest](https://github.com/thautwarm/frontend-for-free/tree/cfg/runtest).

## What is Frontend-For-Free?

A framework for generating context-free parsers with the following features:

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


## Galleries
- Parsing JSON
  - lexer: [json.rlex](https://github.com/thautwarm/frontend-for-free/blob/cfg/runtest/rbnfjson/json.rlex)
  - parser: [json.rbnf](https://github.com/thautwarm/frontend-for-free/blob/cfg/runtest/rbnfjson/json.exrbnf)

- Parsing LaTeX
  - lexer: [gkdtex.rlex](https://github.com/thautwarm/gkdtex/blob/master/gkdtex.rlex)
  - parser: [gkdtex.gg](https://github.com/thautwarm/gkdtex/blob/master/gkdtex.gg)
  
- Parsing LLVM IR(A major subset)
  - lexer: [llvmir.rlex](https://github.com/thautwarm/frontend-for-free/blob/cfg/runtest/llvmir/llvmir.rlex)
  - parser: [llvmir.rbnf](https://github.com/thautwarm/frontend-for-free/blob/cfg/runtest/llvmir/llvmir.rbnf)

- Parsing nested arithmetic expressions
  - lexer: [arith.rlex](https://github.com/thautwarm/frontend-for-free/blob/cfg/runtest/arith/arith.rlex)
  - parser: [arith.rbnf](https://github.com/thautwarm/frontend-for-free/blob/cfg/runtest/arith/arith.rbnf)


- Parsing the BNF derivative used by FFF(bootstrap)
  - lexer: [fffbnf.rlex](https://github.com/thautwarm/frontend-for-free/blob/cfg/fffbnf.rlex)
  - parser: [fffbnf.rbnf](https://github.com/thautwarm/frontend-for-free/blob/cfg/fffbnf.rbnf)

- Parsing ML syntax:
  - lexer: [mlfs.rlex](https://github.com/thautwarm/UPL/blob/master/aparser/mlfs.rlex)
  - parser: [mlfs.bnf](https://github.com/thautwarm/UPL/blob/master/aparser/mlfs.bnf)


- (**OLD VER 0**)Parsing ML syntax and convert it to DrRacket
    - lexer: [yesml.rlex](https://github.com/thautwarm/ml-to-scheme/blob/master/yesml.rlex)
    - parser: [yesml.rbnf](https://github.com/thautwarm/ml-to-scheme/blob/master/yesml.rbnf)

- (**OLD VER 1**)Muridesu: 以**木兰的方式**, 三小时做出强比Python，形似GoLang的语言
  - lexer: [muridesu.rlex](https://github.com/LanguageAsGarbage/muridesu-lang/blob/master/muridesu.rlex)
  - parser: [muridesu.exrbnf](https://github.com/LanguageAsGarbage/muridesu-lang/blob/master/muridesu.exrbnf)

- (**OLD VER 2**)Parsing Python [ASDL](https://raw.githubusercontent.com/python/cpython/3.6/Parser/Python.asdl) files
  - lexer: [asdl.rlex](https://github.com/python-compiler-tools/ast-compat/blob/master/asdl.rlex)
  - parser: [asdl.exrbnf](https://github.com/python-compiler-tools/ast-compat/blob/master/asdl.exrbnf)

**OLD VER 2**, **OLD VER 1** and **OLD VER 0** are out-of-date, hence the code generation does not work with the master branch.

However, the generated code is permanent and now still working.

Further, **OLD VER 2** can be easily up-to-date by manually performing the following transformations:

1. changing slots `$0, $1, $2, ...` to `$1, $2, $3, ...` 
2. changing `list(rule)` to `list[rule]`, and provide the definition of `list` production:

    ```bnf
    list[p] ::= p        { [$1] }
            |  list[p] p { $1.append($2); $1 }
    ```

3. changing `separated_list(sep, rule)` to `separated_list[sep, rule]`, and provide the definition of `separated_list` production:

    ```bnf
    separated_list[sep, p] ::= 
             p             { [$1] }
          |  list[p] sep p { $1.append($3); $1 }
    ```
