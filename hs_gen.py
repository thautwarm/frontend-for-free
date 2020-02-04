import attr, sys
sys.setrecursionlimit(100)
from json import dumps
from rbnf_rts.rts import Tokens, State
from rbnf_rts.routine import DQString as unesc
from rbnf_parser import mk_parser, run_lexer

@attr.s(hash=True)
class Terminal:
    kind = attr.ib()
    value = attr.ib()

LITERAL = 'LITERAL'
SYMBOL = 'SYMBOL'

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
    defmode = attr.ib()
    alts = attr.ib(converter=_cond_conv)

@attr.s(hash=True)
class Def:
    name = attr.ib()
    defmode = attr.ib()
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
    
    def sub(self, **scope):
        it = Interpreter(**scope)
        it.macros = self.macros
        it.macro_use_cache = self.macro_use_cache
        it.expanded_rules = self.expanded_rules
        return it

    def eval(self, x):
        return self.dispatches[type(x)](self, x)

    @classmethod
    def build(cls, xs):
        top = cls()
        for each in xs:
            top.eval(each)
        for each in top.expanded_rules:
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
        if x.name in self.scope:
            return self.scope[x.name]
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
            alts = map(self.eval, x.alts)
            return self.expanded_rules.extend(
                f'({dumps(name)}, ({alt}), {action})' for alt, action in alts)
        elif defmode is Alias:
            expr = x.alts
            expr = self.eval(expr)
            return self.expanded_rules.append(
                f'({dumps(name)}, ({expr}), Just (MSlot 0))')
        
        raise Exception
    dispatches[Def] = v_Def

    def v_Seq(self, x: Seq):
        ps = ', '.join(f'({self.eval(p)})' for p in x.ps)
        return f'CSeq [{ps}]'

    def v_Alt(self, x: Alt):
        ps = ', '.join(f'({self.eval(p)})' for p in x.ps)
        return f'CAlt [{ps}]'        

    dispatches[Seq] = v_Seq
    dispatches[Alt] = v_Alt

    def v_Call(self, x: Call):
        f = self.eval(x.f)
        args = (f'({self.eval(a)})' for a in x.args)
        return f'MApp ({f}) [{", ".join(args)}]'
    dispatches[Call] = v_Call

    def v_Int(self, x: Int):
        return f'MInt {x.i}'
    dispatches[Int] = v_Int

    def v_Slot(self, x: Int):
        return f'MSlot {x.i}'
    dispatches[Slot] = v_Slot

    def v_Var(self, x: Var):
        return f'MTerm {x.name}'
    dispatches[Var] = v_Var

    def v_Tuple(self, x: Tuple):
        args = (f'({self.eval(a)})' for a in x.elts)
        return f'MTuple [{", ".join(args)}]'    
    dispatches[Tuple] = v_Tuple

    def v_List(self, x: List):
        args = list(f'({self.eval(a)})' for a in x.elts)
        r = 'MBuiltin "empty_list"'
        f = 'MBuiltin "push_list"'
        for a in args:
            r = f'MApp {f} [{r}, {a}]'
        return r
    dispatches[List] = v_List

    def v_MacroDef(self, x: MacroDef):
        self.macros[x.name] = x
        return []
    dispatches[MacroDef] = v_MacroDef

    def v_MacroUse(self, x: MacroUse):
        macro = self.macros[x.name]
        params = tuple(map(self.eval, x.args))
        assert len(params) == len(macro.args), f"filling macro {x.name}'s parameter incorrectly."
        ident = (macro, params)
        n = self.macro_use_cache.get(ident)
        if n:
            return n
        n = f'rbnfmacro_{len(self.macro_use_cache)}'
        self.macro_use_cache[ident] = n
        it = self.sub(**dict(zip(macro.args, params)))
        it.eval(Def(n, macro.defmode, macro.alts))
        return n
    
    dispatches[MacroUse] = v_MacroUse

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
