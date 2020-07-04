from json_parser import *
from json_lex import lexer

__all__ = ["parse"]

_parse = mk_parser()


def parse(text: str, filename: str = "unknown"):
    tokens = lexer(filename, text)
    res = _parse(None, Tokens(tokens))
    if res[0]:
        return res[1]

    msgs = []
    for each in res[1]:
        i, msg = each
        token = tokens[i]
        lineno = token.lineno
        colno = token.colno
        msgs.append(f"Line {lineno}, column {colno}, {msg}")
    raise SyntaxError(f"Filename {filename}:\n" + "\n".join(msgs))
