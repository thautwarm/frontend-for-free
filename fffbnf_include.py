from fffbnf_lex import lexer

from json import dumps
from collections import defaultdict
from io import StringIO
import warnings
import pathlib
import re
import sys
import attr

def unesc(s):
    return eval(s)

@attr.s(hash=True)
class Include:
    lang = attr.ib()
    files = attr.ib()

@attr.s(hash=True)
class Params:
    params = attr.ib(converter=tuple)

@attr.s(hash=True)
class Terminal:
    kind = attr.ib()
    value = attr.ib()


LITERAL = "LITERAL"
SYMBOL = "SYMBOL"


@attr.s(hash=True)
class NonTerminal:
    name = attr.ib()


@attr.s(hash=True)
class Optional:
    rule = attr.ib()


@attr.s(hash=True)
class MacroUse:
    name = attr.ib()
    args = attr.ib(converter=tuple)


@attr.s(hash=True)
class Bind:
    name = attr.ib()
    atom = attr.ib()


@attr.s(hash=True)
class Rewrite:
    rule = attr.ib()
    action = attr.ib()


Define = object()
Alias = object()


def _cond_conv(x):
    if isinstance(x, list):
        return tuple(x)
    return x


@attr.s(hash=True)
class MacroDef:
    name = attr.ib()
    args = attr.ib(converter=tuple)
    alts = attr.ib(converter=_cond_conv)


@attr.s(hash=True)
class Def:
    name = attr.ib()
    alts = attr.ib(converter=_cond_conv)


@attr.s(hash=True)
class Call:
    f = attr.ib()
    args = attr.ib(converter=tuple)


@attr.s(hash=True)
class Var:
    name = attr.ib()


@attr.s(hash=True)
class Int:
    i = attr.ib()


@attr.s(hash=True)
class List:
    elts = attr.ib(converter=tuple)

@attr.s(hash=True)
class Attr:
    subject = attr.ib()
    attr = attr.ib()

@attr.s(hash=True)
class Stmts:
    suite = attr.ib(converter=tuple)

def maybeStmts(suite):
    if len(suite) == 1:
        return suite[0]
    return Stmts(suite)

@attr.s(hash=True)
class Tuple:
    elts = attr.ib(converter=tuple)


def maybeTuple(elts):
    if len(elts) is 1:
        return elts[0]
    return Tuple(elts)


@attr.s(hash=True)
class Seq:
    ps = attr.ib(converter=tuple)


@attr.s(hash=True)
class Alt:
    ps = attr.ib(converter=tuple)


def seq(xs):
    if len(xs) is 1:
        return xs[0]
    return Seq(xs)


def alt(xs):
    if len(xs) is 1:
        return xs[0]
    return Alt(xs)


@attr.s(hash=True)
class Slot:
    i = attr.ib()


class Interpreter:
    dispatches = {}

    def __init__(self, **scope):
        self.scope = scope
        self.macros = {}
        self.macro_use_cache = {}
        self.expanded_rules = []
        self.includes = []
        self.params = []

    def sub(self, **scope):
        it = Interpreter(**scope)
        it.macros = self.macros
        it.macro_use_cache = self.macro_use_cache
        it.expanded_rules = self.expanded_rules
        it.includes = self.includes
        return it

    def eval(self, x):
        return self.dispatches[type(x)](self, x)

    @classmethod
    def build(cls, xs):
        top = cls()
        io = StringIO()
        print("[", file=io)
        for each in xs:
            top.eval(each)
        print("\n, ".join(top.expanded_rules), file=io)
        print("]", file=io)
        return io.getvalue(), top.includes, top.params

    def v_Include(self, x: Include):
        self.includes.append((x.lang, x.files))

    dispatches[Include] = v_Include

    def v_Params(self, x: Params):
        self.params = x.params

    dispatches[Params] = v_Params


    def v_Terminal(self, x: Terminal):
        if x.kind is LITERAL:
            lit = dumps(f"quote {x.value}")
        elif x.kind is SYMBOL:
            lit = dumps(x.value)
        else:
            raise Exception
        return f"CTerm {lit}"

    dispatches[Terminal] = v_Terminal

    def v_NonTerminal(self, x: NonTerminal):
        if x.name in self.scope:
            return self.scope[x.name]

        return f"CNonTerm {dumps(x.name)}"

    dispatches[NonTerminal] = v_NonTerminal

    def v_Optional(self, x: Optional):
        return f"COpt ({self.eval(x.rule)})"

    dispatches[Optional] = v_Optional

    def v_Bind(self, x: Bind):
        return f"CBind {dumps(x.name)} ({self.eval(x.atom)})"

    dispatches[Bind] = v_Bind

    def v_Rewrite(self, x: Rewrite):
        if x.action:
            action = f"Just ({self.eval(x.action)})"
        else:
            action = "Nothing"
        return self.eval(x.rule), action

    dispatches[Rewrite] = v_Rewrite

    def v_Def(self, x: Def):
        alts = x.alts
        name = x.name
        if isinstance(alts, tuple):
            alts = map(self.eval, alts)
            return self.expanded_rules.extend(
                f"({dumps(name)}, ({alt}), {action})" for alt, action in alts
            )
        else:
            expr = self.eval(alts)
            return self.expanded_rules.append(
                f"({dumps(name)}, ({expr}), Just (MSlot 1))"
            )
        
    dispatches[Def] = v_Def

    def v_Seq(self, x: Seq):
        ps = ", ".join(f"({self.eval(p)})" for p in x.ps)
        return f"CSeq [{ps}]"

    def v_Alt(self, x: Alt):
        ps = ", ".join(f"({self.eval(p)})" for p in x.ps)
        return f"CAlt [{ps}]"

    dispatches[Seq] = v_Seq
    dispatches[Alt] = v_Alt

    def v_Call(self, x: Call):
        f = self.eval(x.f)
        args = (f"({self.eval(a)})" for a in x.args)
        return f'MApp ({f}) [{", ".join(args)}]'

    dispatches[Call] = v_Call

    def v_Int(self, x: Int):
        return f"MInt {x.i}"

    dispatches[Int] = v_Int

    def v_Slot(self, x: Int):
        return f"MSlot {x.i}"

    dispatches[Slot] = v_Slot

    def v_Var(self, x: Var):
        return f"MTerm {dumps(x.name)}"

    dispatches[Var] = v_Var

    def v_Tuple(self, x: Tuple):
        args = (f"({self.eval(a)})" for a in x.elts)
        return f'MTuple [{", ".join(args)}]'

    dispatches[Tuple] = v_Tuple

    def v_List(self, x: List):
        args = list(f"({self.eval(a)})" for a in x.elts)
        r = 'MApp (MBuiltin "empty_list") []'
        f = 'MBuiltin "push_list"'
        for a in args:
            r = f"MApp ({f}) [{r}, {a}]"
        return r

    dispatches[List] = v_List

    def v_Attr(self, x: Attr):
        return 'MAttr ({}) {}'.format(self.eval(x.subject), dumps(x.attr))

    dispatches[Attr] = v_Attr

    def v_Stmts(self, x: Stmts):
        args = list(f"({self.eval(a)})" for a in x.suite)
        if len(args) == 1:
            return args[0]
        return "MCombine [%s]" % (', '.join(args))

    dispatches[Stmts] = v_Stmts

    def v_MacroDef(self, x: MacroDef):
        self.macros[x.name] = x
        return []

    dispatches[MacroDef] = v_MacroDef

    def v_MacroUse(self, x: MacroUse):
        macro = self.macros[x.name]
        params = tuple(map(self.eval, x.args))
        assert len(params) == len(
            macro.args
        ), f"filling macro {x.name}'s parameter incorrectly."
        ident = (macro, params)
        n = self.macro_use_cache.get(ident, None)
        if n is None:
            n = f"rbnfmacro_{len(self.macro_use_cache)}"
            self.macro_use_cache[ident] = n
            it = self.sub(**dict(zip(macro.args, params)))
            it.eval(Def(n, macro.alts))
        return f"CNonTerm {dumps(n)}"

    dispatches[MacroUse] = v_MacroUse


def _find_n(s: str, ch, n: int):
    since = 0
    for i in range(0, n - 1):
        since = s.find(ch, since)

    return s[since : s.find(ch, since)]


def parse(text: str, filename: str = "unknown"):
    _parse = mk_parser()
    tokens = lexer(filename, text)

    res = _parse(State(), Tokens(tokens))
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


def build(filename: str, out_req: str, out_ff: str, lang: str = "python", parseronly: bool=False):
    with open(filename) as f:
        text = f.read()
    readable, includes, params = Interpreter.build(parse(text, filename=filename))
    if not parseronly:
        parent_dir = pathlib.Path(filename).parent
        with open(out_req, "w") as f:
            for required_lang, files in includes:
                if required_lang is None or required_lang == lang:
                    pass
                else:
                    continue
                for include in files:
                    if isinstance(include, tuple):
                        [include, _] = include
                        f.write(include[len("%%inline"):-len("%%")])
                        continue

                    include = parent_dir / include
                    try:
                        with include.open() as r:
                            f.write(r.read())
                            f.write("\n")
                    except FileNotFoundError:
                        warnings.warn(f"{include} not found")

    with open(out_ff, "w") as f:
        f.write(readable)
    
    return params


def entry():
    from wisepy2 import wise

    wise(build)()
