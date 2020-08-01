from ffflex_lex import lexer
from collections import OrderedDict, defaultdict
from wisepy2 import wise
from warnings import warn
import typing as t
import pathlib
import io
import re
import itertools

lex_template = (pathlib.Path(__file__).parent / "ffflex_template.py").open().read()


def literal(x):
    return "quote " + unesc(x)

def _find_n(s: str, ch, n: int):
    since = 0
    for _ in range(0, n - 1):
        since = s.find(ch, since)

    return s[since : s.find(ch, since)]


def parse(text: str, filename: str = "unknown"):
    _parse = mk_parser()
    tokens = lexer(filename, text)
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


def main(filename_terminal, filename_lex, out, *, be: str = "python"):

    parent_dir = pathlib.Path(filename_lex).parent
    regex_lexers = OrderedDict()
    reserved_map = defaultdict(dict)
    ignores = []

    with open(filename_lex) as lex_f:
        lines = lex_f.readlines()
        for line in lines:
            line = line.strip()
            if not line:
                continue
            if line.startswith(r"%"):
                tag, a = parse(line, filename_lex)
                if tag == r"%ignore":
                    ignores.extend(a)
                else:
                    raise ValueError("unknown tag {}".format(tag))
                continue
            i = line.index(" ")
            regex_lexers[line[:i]] = line[i + 1 :]

    with open(filename_terminal) as f:
        terminal_ids = {each: i for i, each in enumerate(map(str.strip, f.readlines()))}

    regex_rules = []
    for n, rule in regex_lexers.items():
        if n not in terminal_ids:
            warn(
                "{} presented in lexer definitions but not referenced in grammar.".format(
                    n
                )
            )
            terminal_ids[n] = len(terminal_ids)
        regex_rules.append((n, rule, terminal_ids[n]))

    literal_rules = []
    for n in set(terminal_ids).difference(regex_lexers):
        if n in ("EOF", "BOF"):
            continue
        if not n.startswith("quote "):
            raise NameError(
                "{} referenced in grammar but not presented in lexer definitions.".format(
                    n
                )
            )

        my_id = terminal_ids[n]
        literal = n[len("quote ") :]
        for (_, rule, regex_id) in regex_rules:
            tmp = re.match(rule, literal)
            if tmp:
                if tmp.group() == literal:
                    # extract match
                    reserved_map[regex_id][literal] = my_id
                    break
                else:
                    raise ValueError(
                        "literal {} cannot be fully matched by regex {}".format(
                            literal, rule
                        )
                    )
        else:  # no break
            literal_rules.append((n, literal, my_id))

    reserved_map = dict(reserved_map)
    unionall_rule = []
    unionall_cast = [None]
    unionall_typeid = [None]
    for n, regex, my_id in regex_rules:

        n_group = re.compile(regex).groups
        unionall_rule.append("(" + regex + ")")
        unionall_typeid.append(my_id)
        unionall_cast.append(reserved_map.get(my_id))

        padding = (None,) * n_group
        unionall_typeid.extend(padding)
        unionall_cast.extend(padding)

    literal_rules.sort(key=lambda _: _[1], reverse=True)
    for n, literal, my_id in literal_rules:
        unionall_rule.append("(" + re.escape(literal) + ")")
        unionall_typeid.append(my_id)
        unionall_cast.append(None)

    lexer = "|".join(unionall_rule)
    ignores = tuple(terminal_ids[n] for n in ignores)

    unionall_info = tuple(zip(unionall_typeid, unionall_cast))
    unionall_info_bytes = tuple(
        zip(
            unionall_typeid,
            [
                cast and {k.encode("utf-8"): v for k, v in cast.items()}
                for cast in unionall_cast
            ],
        )
    )

    with open(out, "w") as f:
        f.write(lex_template)
        f.write("\n")
        f.write(f"EOF = {terminal_ids['EOF']!r}\n")
        f.write(f"BOF = {terminal_ids['BOF']!r}\n")
        f.write(f"REGEX = {lexer!r}\n")
        f.write(f"REGEX_STR = __import__('re').compile(REGEX)\n")
        f.write(f"REGEX_BYTES = __import__('re').compile(REGEX.encode())\n")
        f.write(f"IGNORES = {ignores!r}\n")
        f.write(f"UNIONALL_INFO = {unionall_info!r}\n")
        f.write(f"UNIONALL_INFO_BYTES = {unionall_info_bytes!r}\n")
        f.write(f"numbering = {terminal_ids!r}\n")


def entry():
    wise(main)()
