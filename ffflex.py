from collections import OrderedDict, defaultdict
from rbnf_rts.routine import DQString as unesc
from rbnf_rts.rts import Tokens
from ffflex_parser import mk_parser, run_lexer
from wisepy2 import wise
from warnings import warn
import typing as t


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


def python_be(
    rules: t.List[t.Tuple[str, str, str, int]],
    code: t.Dict[str, t.List[str]],
    require: t.List[str],
    ignore: t.List[str],
):
    raise NotImplemented


def main(filename_terminal, filename_lex, out, be):

    with open(filename_lex) as lex_f:
        lines = lex_f.readlines()
        l = None
        for l in reversed(range(len(lines))):
            if lines[l].startswith(r"%regex"):
                break
        regex_rules = OrderedDict()
        pragmas = dict(ignore=[], require=[], code=defaultdict(list))

        for tag, a in parse("".join(lines[:l]), filename_lex):
            if tag == r"%ignore":
                pragmas["ignore"].extend(a)
            elif tag == r"%require":
                pragmas["require"].extend(a)
            elif tag == r"%code":
                lang, files = a
                pragmas["code"][lang].extend(files)
            else:
                raise ValueError("unknown tag {}".format(tag))

        if l is not None:
            for each in lines[l + 1 :]:
                i = each.index(" ")
                regex_rules[each[:i]] = each[i + 1 :]

    with open(filename_terminal) as f:
        terminal_ids = {each: i for i, each in enumerate(map(str.strip, f.readlines()))}

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
        python_be(rules, **pragmas)
    # FIXME: how to achieve extensible back ends?
    # without modifying native code.
