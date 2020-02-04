import attr
from json import dumps
from rbnf_rts.rts import Tokens, State
from rbnf_rts.routine import DQString as unesc
from rbnf_parser import mk_parser, run_lexer

@attr.s
class Terminal:
    kind = attr.ib()
    value = attr.ib()

LITERAL = 'LITERAL'
SYMBOL = 'SYMBOL'

@attr.s
class NonTerminal:
    name = attr.ib()

@attr.s
class Optional:
    rule = attr.ib()

@attr.s
class MacroUse:
    name = attr.ib()
    args = attr.ib()

@attr.s
class Bind:
    name = attr.ib()
    atom = attr.ib()

@attr.s
class Rewrite:
    rule = attr.ib()
    action = attr.ib()

Define = object()
Alias = object()


@attr.s
class MacroDef:
    name = attr.ib()
    args = attr.ib()
    defmode = attr.ib()
    alts = attr.ib()

@attr.s
class Def:
    name = attr.ib()
    defmode = attr.ib()
    alts = attr.ib()


@attr.s
class Call:
    f = attr.ib()
    args = attr.ib()

@attr.s
class Var:
    name = attr.ib()

@attr.s
class Int:
    i = attr.ib()

@attr.s
class List:
    elts = attr.ib()

@attr.s
class Tuple:
    elts = attr.ib()

def maybeTuple(elts):
    if len(elts) is 1:
        return elts[0]
    return Tuple(elts)

@attr.s
class Slot:
    i = attr.ib()


class Interpreter:
    dispatches = {}

    def __init__(self):
        self.scope = {}

    @classmethod
    def eval(cls, x):
        return cls.dispatches[type(x)](x)

    @classmethod
    def build(cls, xs):
        for each in xs:
            print(each)

    def v_Terminal(self, x: Terminal):
        if x.kind is LITERAL:
            lit = dumps(f"quote {x.value}")
        elif x.kind is SYMBOL:
            lit = dumps(x.value)
        else:
            raise Exception
        return f'CTerm {lit}'
    dispatches[Terminal] = v_Terminal
    
    def v_NonTerminal(self, x: NonTerminal):
        return f'CNomTerm {x.name}'
    dispatches[NonTerminal] = v_NonTerminal

    def v_Optional(self, x: Optional):
        return f'COpt ({self.eval(x.rule)})'
    dispatches[Optional] = v_Optional
    
    def v_Bind(self, x: Bind):
        return f'CBind {dumps(x.name)} ({self.eval(x.atom)})'
    dispatches[Bind] = v_Bind

    def v_Rewrite(self, x: Rewrite):
        if x.action:
            action = self.eval(x.action)
        else:
            action = 'Nothing'
        return self.eval(x.rule), action
    dispatches[Rewrite] = v_Rewrite

    def v_Def(self, x: Def):
        defmode = x.defmode
        name = x.name
        if defmode is Define:
            alts = list(map(self.eval, x.alts))
            return [f'({name}, ({alt}), {action})' for alt, action in alts]
        elif defmode is Alias:
            expr = x.alts
            expr = self.eval(expr)
            return [f'({name}, ({expr}), MSlot 0)']
        raise Exception
    dispatches[Def] = v_Def

    def v_Call(self, x: Call):
        f = self.eval(x.f)
        args = (f'({self.eval(a)})' for a in x.args)
        return f'MApp ({f}) [{", ".join(args)}]'
    dispatches[Call] = v_Call

    def v_Int(self, x: Int):
        return f'MInt {x.i}'
    dispatches[Int] = v_Int

    def v_Slot(self, x: Int):
        return f'MInt {x.i}'
    dispatches[Slot] = v_Slot

    def v_Var(self, x: Var):
        return f'MTerm {x.name}'

    def v_Tuple(self, x: Tuple):
        args = (f'({self.eval(a)})' for a in x.elts)
        return f'MTuple [{", ".join(args)}]'
    
    def v_List(self, x: List):
        args = list(f'({self.eval(a)})' for a in x.elts)
        r = 'MBuiltin "empty_list"'
        f = 'MBuiltin "push_list"'
        for a in args:
            r = f'MApp {f} [{r}, {a}]'
        return r    

co = mk_parser.__code__
requires = co.co_varnames[:co.co_argcount]
ctx = {}

g = globals()

for each in requires:
    if each not in ctx:
        ctx[each] = g[each]
del g

_parse = mk_parser(**ctx)


def _find_n(s: str, ch, n: int):
    since = 0
    for i in range(0, n - 1):
        since = s.find(ch, since)

    return s[since:s.find(ch, since)]


def parse(text: str, filename: str = "unknown"):
    tokens = list(run_lexer(filename, text))

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
    e.msg = '\n'.join(msgs)
    e.filename = filename
    off = token.offset
    e.offset = off
    e.text = text[:text.find('\n', off)]
    raise e

def build(filename: str):
    with open(filename) as f:
        text = f.read()
    Interpreter.build(parse(text, filename=filename))

if __name__ == '__main__':
    from argser import call
    call(build)