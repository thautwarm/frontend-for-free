from collections import OrderedDict, defaultdict
from rbnf_rts.routine import DQString as unesc
from rbnf_rts.rts import Tokens
from ffflex_parser import mk_parser, run_lexer
from wisepy2 import wise
from warnings import warn
import typing as t
import pathlib
import io

lex_template = (pathlib.Path(__file__).parent / "lex_template.py").open().read()
def literal(x):
    return "quote " + unesc(x)


_parse = mk_parser(literal=literal, unesc=unesc)


def _find_n(s: str, ch, n: int):
    since = 0
    for _ in range(0, n - 1):
        since = s.find(ch, since)

    return s[since : s.find(ch, since)]


def parse(text: str, filename: str = "unknown"):
    tokens = list(run_lexer(filename, text))
    res = _parse(None, Tokens(tokens))
    if res[0]:
        return res[1]
    msgs = []
    assert res[1]
    maxline = 0
    for each in res[1]:
        i, msg = each
        token = tokens[i]
        lineno = token.lineno
        maxline = max(lineno, maxline)
        colno = token.colno
        msgs.append(f"Line {lineno + 1}, column {colno}, {msg}")

    e = SyntaxError()
    e.lineno = maxline + 1
    e.msg = "\n".join(msgs)
    e.filename = filename
    off = token.offset
    e.offset = off
    e.text = text[: text.find("\n", off)]
    raise e

def main(filename_terminal, filename_lex, out, *, be: str="python"):

    parent_dir = pathlib.Path(filename_lex).parent
    regex_rules = OrderedDict()
    pragmas = dict(ignore=[], code=[], reserved_map={})

    with open(filename_lex) as lex_f:
        lines = lex_f.readlines()
        for line in lines:
            line = line.strip()
            if not line:
                continue
            if line.startswith(r"%"):
                tag, a = parse(line, filename_lex)
                if tag == r"%ignore":
                    pragmas["ignore"].extend(a)
                elif tag == r"%require":
                    pass
                elif tag == r"%code":
                    lang, files = a
                    pragmas["code"].append((lang, files))
                elif tag == r"%reserve":
                    a1, a2 = a
                    pragmas["reserved_map"][a1] = a2
                else:
                    raise ValueError("unknown tag {}".format(tag))    
                continue
            i = line.index(" ")
            regex_rules[line[:i]] = line[i + 1 :]

    with open(filename_terminal) as f:
        terminal_ids = {each: i + 2 for i, each in enumerate(map(str.strip, f.readlines()))}

    rules = []
    for n, rule in regex_rules.items():
        if n not in terminal_ids:
            warn(
                "{} presented in lexer definitions but not referenced in grammar.".format(
                    n
                )
            )
        rules.append((n, "regex", rule, terminal_ids[n]))

    for n in set(terminal_ids).difference(regex_rules):
        if not n.startswith("quote "):
            raise NameError(
                "{} referenced in grammar but not presented in lexer definitions.".format(
                    n
                )
            )
        rules.append((n, "literal", n[len("quote ") :], terminal_ids[n]))

    # TODO: dispatch by back end
    
    if be == "python":
        code = python_be(parent_dir, rules, **pragmas)
    else:
        # FIXME: how to achieve extensible back ends?
        # without modifying native code.
        raise ValueError
    
    with open(out, 'w') as f:
        f.write(lex_template)
        f.write(code)
    
def python_be(
    parent : pathlib.Path,
    rules: t.List[t.Tuple[str, str, str, int]],
    code: t.List[t.Tuple[str, t.List[str]]],
    ignore: t.List[str],
    reserved_map : t.Dict[str, str]
):  

    ignore = tuple(ignore)
    
    rules = [
        kind == "literal" and
        f"lit_rule({id}, {n!r}, {rule!r})" or
        f"reg_rule({id}, {n!r}, {rule!r})"
        for (n, kind, rule, id) in rules
    ]

    rules = "[" + ", ".join(rules) + "]"
    mk_lexer = f"numbering, lexer = mk_lexer(*{rules}, ignores={ignore!r}, reserved_map={reserved_map!r})"
    buf = io.StringIO()

    for lang, includes in code:
        if lang is None or lang == "python":
            pass
        else:
            continue
        for include in includes:
            include = parent / include
            try:
                with include.open() as r:
                    buf.write(r.read())
            except FileNotFoundError:
                print(f"{include} not found")
    buf.write(mk_lexer)
    return buf.getvalue()

if __name__ == "__main__":
    wise(main)()