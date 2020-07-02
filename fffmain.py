from subprocess import call
from pathlib import Path


def main(
    parser,
    lex,
    *,
    lexer_out="",
    parser_out="",
    parser_suffix=".rbnf",
    lexer_suffix=".rlex",
    lookahead: int = 1
):
    lang = "python"
    parser_file = Path(parser + parser_suffix)
    lex_file = Path(lex + lexer_suffix)
    
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
            "--trace",
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


def entry():
    from wisepy2 import wise
    wise(main())
