from subprocess import call
from pathlib import Path


def main(
    parser,
    lex,
    *,
    lexer_out="",
    parser_out="",
    lookahead: int = 1,
    trace: bool = False
):
    lang = "python"
    parser_file = Path(parser)
    lex_file = Path(lex)
    
    lexer_out = lexer_out and Path(lexer_out) or lex_file.with_suffix(".py")
    parser_out = parser_out and Path(parser_out) or parser_file.with_suffix(".py")

    parser_requires = parser_file.with_suffix(".out.requires")
    parser_fff = parser_file.with_suffix(".out.fff")
    parser_py = parser_file.with_suffix(".out.py")
    lex_terminals = lex_file.with_suffix(".out.terminals")

    call(
        [
            "fffbnf",
            str(parser_file),
            str(parser_requires),
            str(parser_fff),
            "--lang",
            lang,
        ]
    )

    call(
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
            *(["--trace"] if trace else [])
        ]
    )

    call(["fff-lex", "-in", str(parser_fff), "-out", str(lex_terminals)])

    call([
        "ffflex",
        str(lex_terminals),
        str(lex_file),
        str(lexer_out)
    ])

    call([
        "fffpylinker",
        str(parser_requires),
        str(parser_py),
        str(parser_out)
    ])

    for each in (parser_requires, parser_py, parser_fff, lex_terminals):
        each.unlink()

def entry():
    from wisepy2 import wise
    wise(main)()
