from subprocess import call
from pathlib import Path
from fffbnf import build as bnf_build
import sys


def main(
    parser,
    *,
    lexer_in="",
    lexer_out="",
    parser_out="",
    lookahead: int = 1,
    trace: bool = False
):
    lang = "python"

    parser_file = Path(parser)
    lex_file = lexer_in and Path(lexer_in) or parser_file.with_suffix(".rlex")

    if lexer_out:
        lexer_out = Path(lexer_out)
    else:
        lexer_out = lex_file.with_name(lex_file.with_suffix("").name + "_lex.py")

    if parser_out:
        parser_out = Path(parser_out)
    else:
        parser_out = parser_file.with_name(
            parser_file.with_suffix("").name + "_parser.py"
        )

    parser_requires = parser_file.with_suffix(".out.requires")
    parser_fff = parser_file.with_suffix(".out.fff")
    parser_py = parser_file.with_suffix(".out.py")
    lex_terminals = lex_file.with_suffix(".out.terminals")

    params = bnf_build(
        str(parser_file), str(parser_requires), str(parser_fff), lang=lang
    )

    if call(
        [
            "fff-pgen",
            "-in",
            str(parser_fff),
            "-k",
            str(lookahead),
            "-out",
            str(parser_py),
            "-be",
            "python",
            "--noinline",
            *(["--trace"] if trace else []),
        ]
    ):
        sys.exit(1)

    if call(["fff-lex", "-in", str(parser_fff), "-out", str(lex_terminals)]):
        sys.exit(1)

    if call(["ffflex", str(lex_terminals), str(lex_file), str(lexer_out)]):
        sys.exit(1)

    if call(
        [
            "fffpylinker",
            str(parser_requires),
            str(parser_py),
            str(parser_out),
            "--params",
            ",".join(params),
        ]
    ):
        sys.exit(1)

    for each in (parser_requires, parser_py, parser_fff, lex_terminals):
        each.unlink()


def entry():
    from wisepy2 import wise

    wise(main)()
