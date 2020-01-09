# RBNF

Due to the obstacle of individually maintaining type inference framework and corresponding IRs,
I now rule out current implementation for OCaml back end.

Instead, a plan to support following back ends is established.

- Julia(source code)
- Python(bytecode, by [sijuiacion-ir](https://github.com/RemuLang/sijuiacion-lang))
- Ruby(source code)
- Lua(source code)

## Usage

```
Usage: [-v] [-h] [-in filename] [-out filename]
[-be python|ocaml|marisa(default)]
[-k lookahead number] [--trace : codegen with tracebacks.]
[--noinline : might be useful when viewing generated code]

# gen python
rbnf-pgen -in <infile>.rbnf -k <lookahead number> -out <outfile> -be python [--codegen-with-trace] --noinline

# list out all terminals of the given grammar
rbnf-lex -in <infile>.rbnf -out <outfile>
```

## Repo Structure

- RBNF.IRs: a bunch of IRs useful for generating codes for various backend
    - Cirno: not for codegen, used before generating parsing graphs, to analyse parsing semantics.

        It will be simply interpreted in a stack-based virtual machine(a simple abstract interpreter).

    - Marisa: the first layer which is generated from the parsing graphs and user settings.

        It lacks of declarations, and is untyped, however sufficient for dynamic languages to
        do codegen.

- RBNF.BackEnds
    - Pyrrha: code generator targeting Python back end

- to be continue

## Example

```
Number ::= number
Factor ::= Number | "-" !a=Factor
Mul    ::= ?always_true Factor
           | Mul "*" Factor
```

You can configure the generator to specify whether to generate
codes with error reports.

## Note: Front End

### .rbnf

- `<a>/"a"`: terminal rule `a/quote a`
- `a`:  non-terminal rule
- `a | b`: `a` or `b`
- `[a b c]`: an optional block
- `a b c`: a sequence of parsing symbols
- `!name=rule`: context-xsensitive feature, locally bind what produced by `rule` to `name`.
- `?f(x)/?f/?(f(g(x))`: use locally-binded variables to do some predictions.
- `{E: p}` : a syntactic sugar for `!tmp=p ?E(tmp)`, where `tmp` is a mangled name.
- `a ::= b;`: classic production definition
- `a ::= b -> f;`: specify custom reduction rule

```
exp ::= "if" cond=exp "then"
            t=exp
        "else" f=exp
        -> If(cond, t, f);
exp ::= ...
```

Line comments are supported(`# ...`), but not very fancy.

The `.rbnf` is really weak, however, with a simple bootstrap and adding some syntax sugars,
it can be as powerful as OCaml Menhir.

Check out the capabilities of `.exrbnf` in [rbnf-rts](https://github.com/thautwarm/rbnf-rts),
and you can find the fastest generated JSON parser by RBNF at [rbnfjson](https://github.com/thautwarm/rbnf-rts/blob/master/test/rbnfjson/json.exrbnf).

### Top Level Combinators in Haskell

RBNF accepts a `C` data(given below), and produces efficient generated parser.

```haskell
data C
    = CTerm    String
    | CNonTerm String
    | CSeq     [C]
    | CAlt     [C]
    | COpt     C
    -- advanced:
    | CBind    String C
    | CPred    MiniLang
    | CModif   MiniLang -- modify current context
    deriving (Eq, Ord, Generic)
type CRule = C
type CProd = (String, C, Maybe MiniLang)
data MiniLang
    = MTerm String
    | MApp MiniLang [MiniLang]
    deriving (Eq, Ord, Generic)
```

## Note: Left Recursion Handling

![Left recur](./lr.png)


# Note: Lookahead decision trees(by ID3 algorithm)

```
======== Node1 ========
  LAShift (Case "-")
      LAShift (Case "-")
          LAShift (Case "-")
              [6]
          LAShift (Case "number")
              [6]
      LAShift (Case "number")
          [6]
  LAShift (Case "number")
      [12]

--- LA optimization:
case elts[0]
      LAShift (Case "-") => [6]
      LAShift (Case "number") => [12]

======== Node19 ========
  LAShift (Case "-")
      LAShift (Case "-")
          LAShift (Case "-")
              [20]
          LAShift (Case "number")
              [20]
      LAShift (Case "number")
          LAShift (Case "*")
              [20]
  LAShift (Case "number")
      LAShift (Case "*")
          LAShift (Case "-")
              [28]
          LAShift (Case "number")
              [28]
--- LA optimization:
case elts[1]
      LAShift (Case "*") => [28]
      LAShift (Case "-") => [20]
      LAShift (Case "number") => [20]

======== Node38 ========
  LAShift (Case "-")
      LAShift (Case "-")
          LAShift (Case "-")
              [39]
          LAShift (Case "number")
              [39]
      LAShift (Case "number")
          LAShift (Case "*")
              [39]
  LAShift (Case "number")
      LAShift (Case "*")
          [48]

--- LA optimization:
case elts[1]
      LAShift (Case "*") => [48]
      LAShift (Case "-") => [39]
      LAShift (Case "number") => [39]
```
