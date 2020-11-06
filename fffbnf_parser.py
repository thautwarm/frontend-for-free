
"""
Copyright thautwarm (c) 2019

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of thautwarm nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""
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


from json.decoder import py_scanstring
def unesc(s, py_scanstring=py_scanstring):
  if s[0] == '"':
    return py_scanstring(s, 1)[0]
  return eval(s)
def ListUse(t):
  return MacroUse("list", [t])
from typing import Generic, TypeVar
T = TypeVar('T')

class Tokens():
    __slots__ = ['array', 'offset']

    def __init__(self, array):
        self.array = array
        self.offset = 0

class State():

    def __init__(self):
        pass

class AST(Generic[T]):
    __slots__ = ['tag', 'contents']

    def __init__(self, tag: str, contents: T):
        self.tag = tag
        self.contents = contents

class Nil():
    nil = None
    __slots__ = []

    def __init__(self):
        if (Nil.nil is None):
            Nil.nil = self
            return
        raise ValueError('Nil cannot get instantiated twice.')

    def __len__(self):
        return 0

    def __getitem__(self, n):
        raise IndexError('Out of bounds')

    @property
    def head(self):
        raise IndexError('Out of bounds')

    @property
    def tail(self):
        raise IndexError('Out of bounds')

    def __repr__(self):
        return '[]'
_nil = Nil()

class Cons():
    __slots__ = ['head', 'tail']

    def __init__(self, _head, _tail):
        self.head = _head
        self.tail = _tail

    def __len__(self):
        nil = _nil
        l = 0
        while (self is not nil):
            l += 1
            self = self.tail
        return l

    def __iter__(self):
        nil = _nil
        while (self is not nil):
            (yield self.head)
            self = self.tail

    def __getitem__(self, n):
        while (n != 0):
            self = self.tail
            n -= 1
        return self.head

    def __repr__(self):
        return repr(list(self))
try:

    def mk_pretty():
        from prettyprinter import register_pretty, pretty_call, pprint

        @register_pretty(Tokens)
        def pretty_tokens(value, ctx):
            return pretty_call(ctx, Tokens, offset=value.offset, array=value.array)

        @register_pretty(AST)
        def pretty_ast(value, ctx):
            return pretty_call(ctx, AST, tag=value.tag, contents=value.contents)
    mk_pretty()
    del mk_pretty
except ImportError:
    pass
del T, Generic, TypeVar
builtin_cons = Cons
builtin_nil = _nil
builtin_mk_ast = AST

def mk_parser():
    pass

    def rbnf_named_lr_step_lang(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 5):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_1 = lcl_0
        lcl_0 = (rbnf_tmp_1 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'quote ( not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_1 = lcl_1
            try:
                builtin_tokens.array[(builtin_tokens.offset + 0)]
                _rbnf_peek_tmp = True
            except IndexError:
                _rbnf_peek_tmp = False
            lcl_1 = _rbnf_peek_tmp
            if lcl_1:
                lcl_3 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                lcl_3 = lcl_3.idint
                if (lcl_3 == 7):
                    lcl_4 = rbnf_named_parse_lang_lst(builtin_state, builtin_tokens)
                    rbnf_named__check_2 = lcl_4
                    lcl_4 = rbnf_named__check_2[0]
                    lcl_4 = (lcl_4 == False)
                    if lcl_4:
                        lcl_4 = rbnf_named__check_2
                    else:
                        lcl_5 = rbnf_named__check_2[1]
                        rbnf_tmp_2 = lcl_5
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 6):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_3 = lcl_5
                        lcl_5 = (rbnf_tmp_3 is None)
                        if lcl_5:
                            lcl_6 = builtin_tokens.offset
                            lcl_6 = (lcl_6, 'quote ) not match')
                            lcl_6 = builtin_cons(lcl_6, builtin_nil)
                            lcl_6 = (False, lcl_6)
                            lcl_5 = lcl_6
                        else:
                            lcl_6 = Call(rbnf_tmp_0, rbnf_tmp_2)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 6):
                    _rbnf_old_offset = builtin_tokens.offset
                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = []
                    lcl_4 = Call(rbnf_tmp_0, lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_2 = lcl_4
                elif (lcl_3 == 5):
                    lcl_4 = rbnf_named_parse_lang_lst(builtin_state, builtin_tokens)
                    rbnf_named__check_2 = lcl_4
                    lcl_4 = rbnf_named__check_2[0]
                    lcl_4 = (lcl_4 == False)
                    if lcl_4:
                        lcl_4 = rbnf_named__check_2
                    else:
                        lcl_5 = rbnf_named__check_2[1]
                        rbnf_tmp_2 = lcl_5
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 6):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_3 = lcl_5
                        lcl_5 = (rbnf_tmp_3 is None)
                        if lcl_5:
                            lcl_6 = builtin_tokens.offset
                            lcl_6 = (lcl_6, 'quote ) not match')
                            lcl_6 = builtin_cons(lcl_6, builtin_nil)
                            lcl_6 = (False, lcl_6)
                            lcl_5 = lcl_6
                        else:
                            lcl_6 = Call(rbnf_tmp_0, rbnf_tmp_2)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 22):
                    lcl_4 = rbnf_named_parse_lang_lst(builtin_state, builtin_tokens)
                    rbnf_named__check_2 = lcl_4
                    lcl_4 = rbnf_named__check_2[0]
                    lcl_4 = (lcl_4 == False)
                    if lcl_4:
                        lcl_4 = rbnf_named__check_2
                    else:
                        lcl_5 = rbnf_named__check_2[1]
                        rbnf_tmp_2 = lcl_5
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 6):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_3 = lcl_5
                        lcl_5 = (rbnf_tmp_3 is None)
                        if lcl_5:
                            lcl_6 = builtin_tokens.offset
                            lcl_6 = (lcl_6, 'quote ) not match')
                            lcl_6 = builtin_cons(lcl_6, builtin_nil)
                            lcl_6 = (False, lcl_6)
                            lcl_5 = lcl_6
                        else:
                            lcl_6 = Call(rbnf_tmp_0, rbnf_tmp_2)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 21):
                    lcl_4 = rbnf_named_parse_lang_lst(builtin_state, builtin_tokens)
                    rbnf_named__check_2 = lcl_4
                    lcl_4 = rbnf_named__check_2[0]
                    lcl_4 = (lcl_4 == False)
                    if lcl_4:
                        lcl_4 = rbnf_named__check_2
                    else:
                        lcl_5 = rbnf_named__check_2[1]
                        rbnf_tmp_2 = lcl_5
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 6):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_3 = lcl_5
                        lcl_5 = (rbnf_tmp_3 is None)
                        if lcl_5:
                            lcl_6 = builtin_tokens.offset
                            lcl_6 = (lcl_6, 'quote ) not match')
                            lcl_6 = builtin_cons(lcl_6, builtin_nil)
                            lcl_6 = (False, lcl_6)
                            lcl_5 = lcl_6
                        else:
                            lcl_6 = Call(rbnf_tmp_0, rbnf_tmp_2)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 1):
                    lcl_4 = rbnf_named_parse_lang_lst(builtin_state, builtin_tokens)
                    rbnf_named__check_2 = lcl_4
                    lcl_4 = rbnf_named__check_2[0]
                    lcl_4 = (lcl_4 == False)
                    if lcl_4:
                        lcl_4 = rbnf_named__check_2
                    else:
                        lcl_5 = rbnf_named__check_2[1]
                        rbnf_tmp_2 = lcl_5
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 6):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_3 = lcl_5
                        lcl_5 = (rbnf_tmp_3 is None)
                        if lcl_5:
                            lcl_6 = builtin_tokens.offset
                            lcl_6 = (lcl_6, 'quote ) not match')
                            lcl_6 = builtin_cons(lcl_6, builtin_nil)
                            lcl_6 = (False, lcl_6)
                            lcl_5 = lcl_6
                        else:
                            lcl_6 = Call(rbnf_tmp_0, rbnf_tmp_2)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                else:
                    lcl_4 = (rbnf_named__off_1, 'lang lookahead failed')
                    lcl_4 = builtin_cons(lcl_4, builtin_nil)
                    lcl_4 = (False, lcl_4)
                    lcl_2 = lcl_4
                lcl_1 = lcl_2
            else:
                lcl_2 = (rbnf_named__off_1, 'lang got EOF')
                lcl_2 = builtin_cons(lcl_2, builtin_nil)
                lcl_2 = (False, lcl_2)
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_lang(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_lang_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_lang(rbnf_named_lr_lang_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_lang_try = lcl_0
        lcl_0 = rbnf_named_lr_lang_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_lang_try[1]
            rbnf_named_lr_lang_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_lang(rbnf_named_lr_lang_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_lang_try = lcl_1
            lcl_1 = rbnf_named_lr_lang_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_lang_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_lang_try
        return lcl_0

    def rbnf_named_lr_step_lang_atom(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 23):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_1 = lcl_0
        lcl_0 = (rbnf_tmp_1 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'quote . not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_named_parse_Ident(builtin_state, builtin_tokens)
            rbnf_named__check_2 = lcl_1
            lcl_1 = rbnf_named__check_2[0]
            lcl_1 = (lcl_1 == False)
            if lcl_1:
                lcl_1 = rbnf_named__check_2
            else:
                lcl_2 = rbnf_named__check_2[1]
                rbnf_tmp_2 = lcl_2
                lcl_2 = Attr(rbnf_tmp_0, rbnf_tmp_2)
                rbnf_tmp_1_ = lcl_2
                lcl_2 = (True, rbnf_tmp_1_)
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_lang_atom(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_lang_atom_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_lang_atom(rbnf_named_lr_lang_atom_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_lang_atom_try = lcl_0
        lcl_0 = rbnf_named_lr_lang_atom_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_lang_atom_try[1]
            rbnf_named_lr_lang_atom_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_lang_atom(rbnf_named_lr_lang_atom_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_lang_atom_try = lcl_1
            lcl_1 = rbnf_named_lr_lang_atom_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_lang_atom_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_lang_atom_try
        return lcl_0

    def rbnf_named_lr_step_rbnfmacro_0(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 0):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_1 = lcl_0
        lcl_0 = (rbnf_tmp_1 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'quote , not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_named_parse_expr(builtin_state, builtin_tokens)
            rbnf_named__check_2 = lcl_1
            lcl_1 = rbnf_named__check_2[0]
            lcl_1 = (lcl_1 == False)
            if lcl_1:
                lcl_1 = rbnf_named__check_2
            else:
                lcl_2 = rbnf_named__check_2[1]
                rbnf_tmp_2 = lcl_2
                lcl_2 = rbnf_tmp_0.append
                lcl_2 = lcl_2(rbnf_tmp_2)
                rbnf_tmp_1_ = rbnf_tmp_0
                lcl_3 = (True, rbnf_tmp_1_)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_0(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_0_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_0(rbnf_named_lr_rbnfmacro_0_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_0_try = lcl_0
        lcl_0 = rbnf_named_lr_rbnfmacro_0_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_0_try[1]
            rbnf_named_lr_rbnfmacro_0_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_rbnfmacro_0(rbnf_named_lr_rbnfmacro_0_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_0_try = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_0_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_0_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_0_try
        return lcl_0

    def rbnf_named_lr_step_rbnfmacro_1(rbnf_tmp_0, builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_atomExpr(builtin_state, builtin_tokens)
        rbnf_named__check_1 = lcl_0
        lcl_0 = rbnf_named__check_1[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_1
        else:
            lcl_1 = rbnf_named__check_1[1]
            rbnf_tmp_1 = lcl_1
            lcl_1 = rbnf_tmp_0.append
            lcl_1 = lcl_1(rbnf_tmp_1)
            rbnf_tmp_1_ = rbnf_tmp_0
            lcl_2 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_2
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_1(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_1_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_1(rbnf_named_lr_rbnfmacro_1_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_1_try = lcl_0
        lcl_0 = rbnf_named_lr_rbnfmacro_1_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_1_try[1]
            rbnf_named_lr_rbnfmacro_1_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_rbnfmacro_1(rbnf_named_lr_rbnfmacro_1_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_1_try = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_1_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_1_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_1_try
        return lcl_0

    def rbnf_named_lr_step_rbnfmacro_2(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 13):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_1 = lcl_0
        lcl_0 = (rbnf_tmp_1 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'quote | not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_named_parse_cseq(builtin_state, builtin_tokens)
            rbnf_named__check_2 = lcl_1
            lcl_1 = rbnf_named__check_2[0]
            lcl_1 = (lcl_1 == False)
            if lcl_1:
                lcl_1 = rbnf_named__check_2
            else:
                lcl_2 = rbnf_named__check_2[1]
                rbnf_tmp_2 = lcl_2
                lcl_2 = rbnf_tmp_0.append
                lcl_2 = lcl_2(rbnf_tmp_2)
                rbnf_tmp_1_ = rbnf_tmp_0
                lcl_3 = (True, rbnf_tmp_1_)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_2(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_2_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_2(rbnf_named_lr_rbnfmacro_2_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_2_try = lcl_0
        lcl_0 = rbnf_named_lr_rbnfmacro_2_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_2_try[1]
            rbnf_named_lr_rbnfmacro_2_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_rbnfmacro_2(rbnf_named_lr_rbnfmacro_2_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_2_try = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_2_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_2_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_2_try
        return lcl_0

    def rbnf_named_lr_step_rbnfmacro_3(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 13):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_1 = lcl_0
        lcl_0 = (rbnf_tmp_1 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'quote | not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_named_parse_rewrite(builtin_state, builtin_tokens)
            rbnf_named__check_2 = lcl_1
            lcl_1 = rbnf_named__check_2[0]
            lcl_1 = (lcl_1 == False)
            if lcl_1:
                lcl_1 = rbnf_named__check_2
            else:
                lcl_2 = rbnf_named__check_2[1]
                rbnf_tmp_2 = lcl_2
                lcl_2 = rbnf_tmp_0.append
                lcl_2 = lcl_2(rbnf_tmp_2)
                rbnf_tmp_1_ = rbnf_tmp_0
                lcl_3 = (True, rbnf_tmp_1_)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_3(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_3_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_3(rbnf_named_lr_rbnfmacro_3_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_3_try = lcl_0
        lcl_0 = rbnf_named_lr_rbnfmacro_3_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_3_try[1]
            rbnf_named_lr_rbnfmacro_3_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_rbnfmacro_3(rbnf_named_lr_rbnfmacro_3_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_3_try = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_3_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_3_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_3_try
        return lcl_0

    def rbnf_named_lr_step_rbnfmacro_4(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 0):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_1 = lcl_0
        lcl_0 = (rbnf_tmp_1 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'quote , not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_named_parse_Ident(builtin_state, builtin_tokens)
            rbnf_named__check_2 = lcl_1
            lcl_1 = rbnf_named__check_2[0]
            lcl_1 = (lcl_1 == False)
            if lcl_1:
                lcl_1 = rbnf_named__check_2
            else:
                lcl_2 = rbnf_named__check_2[1]
                rbnf_tmp_2 = lcl_2
                lcl_2 = rbnf_tmp_0.append
                lcl_2 = lcl_2(rbnf_tmp_2)
                rbnf_tmp_1_ = rbnf_tmp_0
                lcl_3 = (True, rbnf_tmp_1_)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_4(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_4_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_4(rbnf_named_lr_rbnfmacro_4_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_4_try = lcl_0
        lcl_0 = rbnf_named_lr_rbnfmacro_4_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_4_try[1]
            rbnf_named_lr_rbnfmacro_4_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_rbnfmacro_4(rbnf_named_lr_rbnfmacro_4_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_4_try = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_4_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_4_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_4_try
        return lcl_0

    def rbnf_named_lr_step_rbnfmacro_5(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 20):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_1 = lcl_0
        lcl_0 = (rbnf_tmp_1 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'quote ; not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_named_parse_lang(builtin_state, builtin_tokens)
            rbnf_named__check_2 = lcl_1
            lcl_1 = rbnf_named__check_2[0]
            lcl_1 = (lcl_1 == False)
            if lcl_1:
                lcl_1 = rbnf_named__check_2
            else:
                lcl_2 = rbnf_named__check_2[1]
                rbnf_tmp_2 = lcl_2
                lcl_2 = rbnf_tmp_0.append
                lcl_2 = lcl_2(rbnf_tmp_2)
                rbnf_tmp_1_ = rbnf_tmp_0
                lcl_3 = (True, rbnf_tmp_1_)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_5(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_5_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_5(rbnf_named_lr_rbnfmacro_5_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_5_try = lcl_0
        lcl_0 = rbnf_named_lr_rbnfmacro_5_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_5_try[1]
            rbnf_named_lr_rbnfmacro_5_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_rbnfmacro_5(rbnf_named_lr_rbnfmacro_5_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_5_try = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_5_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_5_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_5_try
        return lcl_0

    def rbnf_named_lr_step_rbnfmacro_6(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 0):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_1 = lcl_0
        lcl_0 = (rbnf_tmp_1 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'quote , not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_named_parse_lang(builtin_state, builtin_tokens)
            rbnf_named__check_2 = lcl_1
            lcl_1 = rbnf_named__check_2[0]
            lcl_1 = (lcl_1 == False)
            if lcl_1:
                lcl_1 = rbnf_named__check_2
            else:
                lcl_2 = rbnf_named__check_2[1]
                rbnf_tmp_2 = lcl_2
                lcl_2 = rbnf_tmp_0.append
                lcl_2 = lcl_2(rbnf_tmp_2)
                rbnf_tmp_1_ = rbnf_tmp_0
                lcl_3 = (True, rbnf_tmp_1_)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_6(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_6_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_6(rbnf_named_lr_rbnfmacro_6_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_6_try = lcl_0
        lcl_0 = rbnf_named_lr_rbnfmacro_6_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_6_try[1]
            rbnf_named_lr_rbnfmacro_6_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_rbnfmacro_6(rbnf_named_lr_rbnfmacro_6_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_6_try = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_6_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_6_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_6_try
        return lcl_0

    def rbnf_named_lr_step_rbnfmacro_7(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 0):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_1 = lcl_0
        lcl_0 = (rbnf_tmp_1 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'quote , not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_named_parse_filename(builtin_state, builtin_tokens)
            rbnf_named__check_2 = lcl_1
            lcl_1 = rbnf_named__check_2[0]
            lcl_1 = (lcl_1 == False)
            if lcl_1:
                lcl_1 = rbnf_named__check_2
            else:
                lcl_2 = rbnf_named__check_2[1]
                rbnf_tmp_2 = lcl_2
                lcl_2 = rbnf_tmp_0.append
                lcl_2 = lcl_2(rbnf_tmp_2)
                rbnf_tmp_1_ = rbnf_tmp_0
                lcl_3 = (True, rbnf_tmp_1_)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_7(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_7_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_7(rbnf_named_lr_rbnfmacro_7_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_7_try = lcl_0
        lcl_0 = rbnf_named_lr_rbnfmacro_7_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_7_try[1]
            rbnf_named_lr_rbnfmacro_7_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_rbnfmacro_7(rbnf_named_lr_rbnfmacro_7_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_7_try = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_7_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_7_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_7_try
        return lcl_0

    def rbnf_named_lr_step_rbnfmacro_8(rbnf_tmp_0, builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        try:
            builtin_tokens.array[(builtin_tokens.offset + 0)]
            _rbnf_peek_tmp = True
        except IndexError:
            _rbnf_peek_tmp = False
        lcl_0 = _rbnf_peek_tmp
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 25):
                lcl_3 = rbnf_named_parse_pragma(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_4 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = rbnf_tmp_0.append
                    lcl_4 = lcl_4(rbnf_tmp_1)
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_5 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_5
                lcl_1 = lcl_3
            elif (lcl_2 == 24):
                lcl_3 = rbnf_named_parse_pragma(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_5 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_5
                    lcl_5 = rbnf_tmp_0.append
                    lcl_5 = lcl_5(rbnf_tmp_1)
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_6 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_6
                lcl_1 = lcl_3
            elif (lcl_2 == 1):
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                try:
                    builtin_tokens.array[(builtin_tokens.offset + 1)]
                    _rbnf_peek_tmp = True
                except IndexError:
                    _rbnf_peek_tmp = False
                lcl_3 = _rbnf_peek_tmp
                if lcl_3:
                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 1)]
                    lcl_7 = lcl_7.idint
                    if (lcl_7 == 7):
                        lcl_8 = rbnf_named_parse_prod(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_8
                        lcl_8 = rbnf_named__check_1[0]
                        lcl_8 = (lcl_8 == False)
                        if lcl_8:
                            lcl_8 = rbnf_named__check_1
                        else:
                            lcl_9 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_9
                            lcl_9 = rbnf_tmp_0.append
                            lcl_9 = lcl_9(rbnf_tmp_1)
                            rbnf_tmp_1_ = rbnf_tmp_0
                            lcl_10 = (True, rbnf_tmp_1_)
                            lcl_8 = lcl_10
                        lcl_6 = lcl_8
                    elif (lcl_7 == 19):
                        lcl_10 = rbnf_named_parse_prod(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_10
                        lcl_10 = rbnf_named__check_1[0]
                        lcl_10 = (lcl_10 == False)
                        if lcl_10:
                            lcl_10 = rbnf_named__check_1
                        else:
                            lcl_8 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_8
                            lcl_8 = rbnf_tmp_0.append
                            lcl_8 = lcl_8(rbnf_tmp_1)
                            rbnf_tmp_1_ = rbnf_tmp_0
                            lcl_11 = (True, rbnf_tmp_1_)
                            lcl_10 = lcl_11
                        lcl_6 = lcl_10
                    elif (lcl_7 == 17):
                        lcl_10 = rbnf_named_parse_prod(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_10
                        lcl_10 = rbnf_named__check_1[0]
                        lcl_10 = (lcl_10 == False)
                        if lcl_10:
                            lcl_10 = rbnf_named__check_1
                        else:
                            lcl_11 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_11
                            lcl_11 = rbnf_tmp_0.append
                            lcl_11 = lcl_11(rbnf_tmp_1)
                            rbnf_tmp_1_ = rbnf_tmp_0
                            lcl_12 = (True, rbnf_tmp_1_)
                            lcl_10 = lcl_12
                        lcl_6 = lcl_10
                    elif (lcl_7 == 18):
                        lcl_10 = rbnf_named_parse_prod(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_10
                        lcl_10 = rbnf_named__check_1[0]
                        lcl_10 = (lcl_10 == False)
                        if lcl_10:
                            lcl_10 = rbnf_named__check_1
                        else:
                            lcl_12 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_12
                            lcl_12 = rbnf_tmp_0.append
                            lcl_12 = lcl_12(rbnf_tmp_1)
                            rbnf_tmp_1_ = rbnf_tmp_0
                            lcl_13 = (True, rbnf_tmp_1_)
                            lcl_10 = lcl_13
                        lcl_6 = lcl_10
                    elif (lcl_7 == 16):
                        lcl_10 = rbnf_named_parse_prod(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_10
                        lcl_10 = rbnf_named__check_1[0]
                        lcl_10 = (lcl_10 == False)
                        if lcl_10:
                            lcl_10 = rbnf_named__check_1
                        else:
                            lcl_13 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_13
                            lcl_13 = rbnf_tmp_0.append
                            lcl_13 = lcl_13(rbnf_tmp_1)
                            rbnf_tmp_1_ = rbnf_tmp_0
                            lcl_14 = (True, rbnf_tmp_1_)
                            lcl_10 = lcl_14
                        lcl_6 = lcl_10
                    elif (lcl_7 == 26):
                        lcl_10 = rbnf_named_parse_pragma(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_10
                        lcl_10 = rbnf_named__check_1[0]
                        lcl_10 = (lcl_10 == False)
                        if lcl_10:
                            lcl_10 = rbnf_named__check_1
                        else:
                            lcl_14 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_14
                            lcl_14 = rbnf_tmp_0.append
                            lcl_14 = lcl_14(rbnf_tmp_1)
                            rbnf_tmp_1_ = rbnf_tmp_0
                            lcl_15 = (True, rbnf_tmp_1_)
                            lcl_10 = lcl_15
                        lcl_6 = lcl_10
                    else:
                        lcl_10 = rbnf_named_parse_prod(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_10
                        lcl_10 = rbnf_named__check_1[0]
                        lcl_10 = (lcl_10 == False)
                        if lcl_10:
                            lcl_10 = rbnf_named__check_1
                        else:
                            lcl_15 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_15
                            lcl_15 = rbnf_tmp_0.append
                            lcl_15 = lcl_15(rbnf_tmp_1)
                            rbnf_tmp_1_ = rbnf_tmp_0
                            lcl_16 = (True, rbnf_tmp_1_)
                            lcl_10 = lcl_16
                        lcl_6 = lcl_10
                    lcl_3 = lcl_6
                else:
                    lcl_10 = (rbnf_named__off_1, 'rbnfmacro_8 got EOF')
                    lcl_10 = builtin_cons(lcl_10, builtin_nil)
                    lcl_10 = (False, lcl_10)
                    lcl_3 = lcl_10
                lcl_1 = lcl_3
            elif (lcl_2 == 26):
                lcl_10 = rbnf_named_parse_pragma(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_10
                lcl_10 = rbnf_named__check_1[0]
                lcl_10 = (lcl_10 == False)
                if lcl_10:
                    lcl_10 = rbnf_named__check_1
                else:
                    lcl_16 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_16
                    lcl_16 = rbnf_tmp_0.append
                    lcl_16 = lcl_16(rbnf_tmp_1)
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_3 = (True, rbnf_tmp_1_)
                    lcl_10 = lcl_3
                lcl_1 = lcl_10
            else:
                lcl_10 = (rbnf_named__off_0, 'rbnfmacro_8 lookahead failed')
                lcl_10 = builtin_cons(lcl_10, builtin_nil)
                lcl_10 = (False, lcl_10)
                lcl_1 = lcl_10
            lcl_0 = lcl_1
        else:
            lcl_1 = (rbnf_named__off_0, 'rbnfmacro_8 got EOF')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_8(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_8_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_8(rbnf_named_lr_rbnfmacro_8_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_8_try = lcl_0
        lcl_0 = rbnf_named_lr_rbnfmacro_8_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_8_try[1]
            rbnf_named_lr_rbnfmacro_8_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_rbnfmacro_8(rbnf_named_lr_rbnfmacro_8_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_8_try = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_8_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_8_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_8_try
        return lcl_0

    def rbnf_named_parse_Ident(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 1):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_0 = lcl_0
        lcl_0 = (rbnf_tmp_0 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'Ident not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_tmp_0.value
            rbnf_tmp_1_ = lcl_1
            lcl_1 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_IdentList(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_rbnfmacro_4(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            rbnf_tmp_1_ = rbnf_tmp_0
            lcl_1 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_START(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 27):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_0 = lcl_0
        lcl_0 = (rbnf_tmp_0 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'BOF not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_named_parse_rbnfmacro_8(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = rbnf_named__check_1[0]
            lcl_1 = (lcl_1 == False)
            if lcl_1:
                lcl_1 = rbnf_named__check_1
            else:
                lcl_2 = rbnf_named__check_1[1]
                rbnf_tmp_1 = lcl_2
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 28):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_2 = _rbnf_cur_token
                rbnf_tmp_2 = lcl_2
                lcl_2 = (rbnf_tmp_2 is None)
                if lcl_2:
                    lcl_3 = builtin_tokens.offset
                    lcl_3 = (lcl_3, 'EOF not match')
                    lcl_3 = builtin_cons(lcl_3, builtin_nil)
                    lcl_3 = (False, lcl_3)
                    lcl_2 = lcl_3
                else:
                    rbnf_tmp_1_ = rbnf_tmp_1
                    lcl_3 = (True, rbnf_tmp_1_)
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_alts(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_rbnfmacro_3(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            rbnf_tmp_1_ = rbnf_tmp_0
            lcl_1 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_atom(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        try:
            builtin_tokens.array[(builtin_tokens.offset + 0)]
            _rbnf_peek_tmp = True
        except IndexError:
            _rbnf_peek_tmp = False
        lcl_0 = _rbnf_peek_tmp
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 3):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_Ident(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_4 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_4
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 4):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = (rbnf_tmp_2 is None)
                    if lcl_4:
                        lcl_5 = builtin_tokens.offset
                        lcl_5 = (lcl_5, 'quote > not match')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    else:
                        lcl_5 = Terminal(SYMBOL, rbnf_tmp_1)
                        rbnf_tmp_1_ = lcl_5
                        lcl_5 = (True, rbnf_tmp_1_)
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 5):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_expr(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_4 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_4
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 6):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = (rbnf_tmp_2 is None)
                    if lcl_4:
                        lcl_5 = builtin_tokens.offset
                        lcl_5 = (lcl_5, 'quote ) not match')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    else:
                        rbnf_tmp_1_ = rbnf_tmp_1
                        lcl_5 = (True, rbnf_tmp_1_)
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 2):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_tmp_0.value
                lcl_3 = unesc(lcl_3)
                lcl_3 = Terminal(LITERAL, lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_3 = (True, rbnf_tmp_1_)
                lcl_1 = lcl_3
            elif (lcl_2 == 1):
                lcl_3 = rbnf_named_parse_Ident(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    try:
                        builtin_tokens.array[(builtin_tokens.offset + 0)]
                        _rbnf_peek_tmp = True
                    except IndexError:
                        _rbnf_peek_tmp = False
                    lcl_4 = _rbnf_peek_tmp
                    if lcl_4:
                        lcl_6 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_6 = lcl_6.idint
                        if (lcl_6 == 7):
                            _rbnf_old_offset = builtin_tokens.offset
                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_1 = lcl_7
                            lcl_7 = rbnf_named_parse_expr_lst(builtin_state, builtin_tokens)
                            rbnf_named__check_2 = lcl_7
                            lcl_7 = rbnf_named__check_2[0]
                            lcl_7 = (lcl_7 == False)
                            if lcl_7:
                                lcl_7 = rbnf_named__check_2
                            else:
                                lcl_8 = rbnf_named__check_2[1]
                                rbnf_tmp_2 = lcl_8
                                try:
                                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                    if (_rbnf_cur_token.idint is 8):
                                        builtin_tokens.offset += 1
                                    else:
                                        _rbnf_cur_token = None
                                except IndexError:
                                    _rbnf_cur_token = None
                                lcl_8 = _rbnf_cur_token
                                rbnf_tmp_3 = lcl_8
                                lcl_8 = (rbnf_tmp_3 is None)
                                if lcl_8:
                                    lcl_9 = builtin_tokens.offset
                                    lcl_9 = (lcl_9, 'quote ] not match')
                                    lcl_9 = builtin_cons(lcl_9, builtin_nil)
                                    lcl_9 = (False, lcl_9)
                                    lcl_8 = lcl_9
                                else:
                                    lcl_9 = MacroUse(rbnf_tmp_0, rbnf_tmp_2)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_9 = (True, rbnf_tmp_1_)
                                    lcl_8 = lcl_9
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 9):
                            _rbnf_old_offset = builtin_tokens.offset
                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_1 = lcl_7
                            lcl_7 = rbnf_named_parse_atomExpr(builtin_state, builtin_tokens)
                            rbnf_named__check_2 = lcl_7
                            lcl_7 = rbnf_named__check_2[0]
                            lcl_7 = (lcl_7 == False)
                            if lcl_7:
                                lcl_7 = rbnf_named__check_2
                            else:
                                lcl_8 = rbnf_named__check_2[1]
                                rbnf_tmp_2 = lcl_8
                                lcl_8 = Bind(rbnf_tmp_0, rbnf_tmp_2)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (True, rbnf_tmp_1_)
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        else:
                            lcl_7 = NonTerminal(rbnf_tmp_0)
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_7
                        lcl_4 = lcl_5
                    else:
                        lcl_5 = (rbnf_named__off_1, 'atom got EOF')
                        lcl_5 = builtin_cons(lcl_5, builtin_nil)
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_3 = (rbnf_named__off_0, 'atom lookahead failed')
                lcl_3 = builtin_cons(lcl_3, builtin_nil)
                lcl_3 = (False, lcl_3)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        else:
            lcl_1 = (rbnf_named__off_0, 'atom got EOF')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_atomExpr(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_atom(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            try:
                builtin_tokens.array[(builtin_tokens.offset + 0)]
                _rbnf_peek_tmp = True
            except IndexError:
                _rbnf_peek_tmp = False
            lcl_1 = _rbnf_peek_tmp
            if lcl_1:
                lcl_3 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                lcl_3 = lcl_3.idint
                if (lcl_3 == 10):
                    _rbnf_old_offset = builtin_tokens.offset
                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = Optional(rbnf_tmp_0)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_2 = lcl_4
                elif (lcl_3 == 12):
                    _rbnf_old_offset = builtin_tokens.offset
                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = ListUse(rbnf_tmp_0)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_2 = lcl_4
                elif (lcl_3 == 11):
                    _rbnf_old_offset = builtin_tokens.offset
                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = ListUse(rbnf_tmp_0)
                    lcl_4 = Optional(lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_2 = lcl_4
                else:
                    rbnf_tmp_1_ = rbnf_tmp_0
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_2 = lcl_4
                lcl_1 = lcl_2
            else:
                lcl_2 = (rbnf_named__off_0, 'atomExpr got EOF')
                lcl_2 = builtin_cons(lcl_2, builtin_nil)
                lcl_2 = (False, lcl_2)
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_cseq(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_seq(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = seq(rbnf_tmp_0)
            rbnf_tmp_1_ = lcl_1
            lcl_1 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_def(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        try:
            builtin_tokens.array[(builtin_tokens.offset + 0)]
            _rbnf_peek_tmp = True
        except IndexError:
            _rbnf_peek_tmp = False
        lcl_0 = _rbnf_peek_tmp
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 19):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_expr(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_4 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_1
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 17):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_alts(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_4 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_1
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 18):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_alts(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_4 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_1
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 16):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_alts(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_4 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_4
                    rbnf_tmp_1_ = rbnf_tmp_1
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_3 = (rbnf_named__off_0, 'def lookahead failed')
                lcl_3 = builtin_cons(lcl_3, builtin_nil)
                lcl_3 = (False, lcl_3)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        else:
            lcl_1 = (rbnf_named__off_0, 'def got EOF')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_expr(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_rbnfmacro_2(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = alt(rbnf_tmp_0)
            rbnf_tmp_1_ = lcl_1
            lcl_1 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_expr_lst(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_rbnfmacro_0(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            rbnf_tmp_1_ = rbnf_tmp_0
            lcl_1 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_filename(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 2):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_0 = lcl_0
        lcl_0 = (rbnf_tmp_0 is None)
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'QuotedStr not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_tmp_0.value
            lcl_1 = unesc(lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_1 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_lang(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_lang_atom(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            rbnf_tmp_1_ = rbnf_tmp_0
            lcl_1 = rbnf_named_lr_loop_lang(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_lang_atom(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        try:
            builtin_tokens.array[(builtin_tokens.offset + 0)]
            _rbnf_peek_tmp = True
        except IndexError:
            _rbnf_peek_tmp = False
        lcl_0 = _rbnf_peek_tmp
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 7):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                try:
                    builtin_tokens.array[(builtin_tokens.offset + 0)]
                    _rbnf_peek_tmp = True
                except IndexError:
                    _rbnf_peek_tmp = False
                lcl_3 = _rbnf_peek_tmp
                if lcl_3:
                    lcl_5 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                    lcl_5 = lcl_5.idint
                    if (lcl_5 == 8):
                        _rbnf_old_offset = builtin_tokens.offset
                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                        lcl_6 = _rbnf_cur_token
                        rbnf_tmp_1 = lcl_6
                        lcl_6 = []
                        lcl_6 = List(lcl_6)
                        rbnf_tmp_1_ = lcl_6
                        lcl_6 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                        lcl_4 = lcl_6
                    elif (lcl_5 == 7):
                        lcl_6 = rbnf_named_parse_lang_lst(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 8):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ] not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = List(rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 5):
                        lcl_6 = rbnf_named_parse_lang_lst(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 8):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ] not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = List(rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 22):
                        lcl_6 = rbnf_named_parse_lang_lst(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 8):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ] not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = List(rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 21):
                        lcl_6 = rbnf_named_parse_lang_lst(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 8):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ] not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = List(rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 1):
                        lcl_6 = rbnf_named_parse_lang_lst(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 8):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_8 = builtin_tokens.offset
                                lcl_8 = (lcl_8, 'quote ] not match')
                                lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                lcl_8 = (False, lcl_8)
                                lcl_7 = lcl_8
                            else:
                                lcl_8 = List(rbnf_tmp_1)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    else:
                        lcl_6 = (rbnf_named__off_1, 'lang_atom lookahead failed')
                        lcl_6 = builtin_cons(lcl_6, builtin_nil)
                        lcl_6 = (False, lcl_6)
                        lcl_4 = lcl_6
                    lcl_3 = lcl_4
                else:
                    lcl_4 = (rbnf_named__off_1, 'lang_atom got EOF')
                    lcl_4 = builtin_cons(lcl_4, builtin_nil)
                    lcl_4 = (False, lcl_4)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 5):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                try:
                    builtin_tokens.array[(builtin_tokens.offset + 0)]
                    _rbnf_peek_tmp = True
                except IndexError:
                    _rbnf_peek_tmp = False
                lcl_3 = _rbnf_peek_tmp
                if lcl_3:
                    lcl_5 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                    lcl_5 = lcl_5.idint
                    if (lcl_5 == 7):
                        lcl_6 = rbnf_named_parse_lang_lst(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            lcl_7 = builtin_tokens.offset
                            rbnf_named__off_2 = lcl_7
                            try:
                                builtin_tokens.array[(builtin_tokens.offset + 0)]
                                _rbnf_peek_tmp = True
                            except IndexError:
                                _rbnf_peek_tmp = False
                            lcl_7 = _rbnf_peek_tmp
                            if lcl_7:
                                lcl_9 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                lcl_9 = lcl_9.idint
                                if (lcl_9 == 0):
                                    _rbnf_old_offset = builtin_tokens.offset
                                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                                    lcl_10 = _rbnf_cur_token
                                    rbnf_tmp_2 = lcl_10
                                    try:
                                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                        if (_rbnf_cur_token.idint is 6):
                                            builtin_tokens.offset += 1
                                        else:
                                            _rbnf_cur_token = None
                                    except IndexError:
                                        _rbnf_cur_token = None
                                    lcl_10 = _rbnf_cur_token
                                    rbnf_tmp_3 = lcl_10
                                    lcl_10 = (rbnf_tmp_3 is None)
                                    if lcl_10:
                                        lcl_11 = builtin_tokens.offset
                                        lcl_11 = (lcl_11, 'quote ) not match')
                                        lcl_11 = builtin_cons(lcl_11, builtin_nil)
                                        lcl_11 = (False, lcl_11)
                                        lcl_10 = lcl_11
                                    else:
                                        lcl_11 = Tuple(rbnf_tmp_1)
                                        rbnf_tmp_1_ = lcl_11
                                        lcl_11 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                        lcl_10 = lcl_11
                                    lcl_8 = lcl_10
                                elif (lcl_9 == 6):
                                    _rbnf_old_offset = builtin_tokens.offset
                                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                                    lcl_10 = _rbnf_cur_token
                                    rbnf_tmp_2 = lcl_10
                                    lcl_10 = maybeTuple(rbnf_tmp_1)
                                    rbnf_tmp_1_ = lcl_10
                                    lcl_10 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                    lcl_8 = lcl_10
                                else:
                                    lcl_10 = (rbnf_named__off_2, 'lang_atom lookahead failed')
                                    lcl_10 = builtin_cons(lcl_10, builtin_nil)
                                    lcl_10 = (False, lcl_10)
                                    lcl_8 = lcl_10
                                lcl_7 = lcl_8
                            else:
                                lcl_10 = (rbnf_named__off_2, 'lang_atom got EOF')
                                lcl_10 = builtin_cons(lcl_10, builtin_nil)
                                lcl_10 = (False, lcl_10)
                                lcl_7 = lcl_10
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 6):
                        _rbnf_old_offset = builtin_tokens.offset
                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                        lcl_10 = _rbnf_cur_token
                        rbnf_tmp_1 = lcl_10
                        lcl_10 = []
                        lcl_10 = Tuple(lcl_10)
                        rbnf_tmp_1_ = lcl_10
                        lcl_10 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                        lcl_4 = lcl_10
                    elif (lcl_5 == 5):
                        lcl_10 = rbnf_named_parse_lang_lst(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_10
                        lcl_10 = rbnf_named__check_1[0]
                        lcl_10 = (lcl_10 == False)
                        if lcl_10:
                            lcl_10 = rbnf_named__check_1
                        else:
                            lcl_11 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_11
                            lcl_11 = builtin_tokens.offset
                            rbnf_named__off_2 = lcl_11
                            try:
                                builtin_tokens.array[(builtin_tokens.offset + 0)]
                                _rbnf_peek_tmp = True
                            except IndexError:
                                _rbnf_peek_tmp = False
                            lcl_11 = _rbnf_peek_tmp
                            if lcl_11:
                                lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                lcl_7 = lcl_7.idint
                                if (lcl_7 == 0):
                                    _rbnf_old_offset = builtin_tokens.offset
                                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_2 = lcl_8
                                    try:
                                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                        if (_rbnf_cur_token.idint is 6):
                                            builtin_tokens.offset += 1
                                        else:
                                            _rbnf_cur_token = None
                                    except IndexError:
                                        _rbnf_cur_token = None
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_3 = lcl_8
                                    lcl_8 = (rbnf_tmp_3 is None)
                                    if lcl_8:
                                        lcl_9 = builtin_tokens.offset
                                        lcl_9 = (lcl_9, 'quote ) not match')
                                        lcl_9 = builtin_cons(lcl_9, builtin_nil)
                                        lcl_9 = (False, lcl_9)
                                        lcl_8 = lcl_9
                                    else:
                                        lcl_9 = Tuple(rbnf_tmp_1)
                                        rbnf_tmp_1_ = lcl_9
                                        lcl_9 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                        lcl_8 = lcl_9
                                    lcl_6 = lcl_8
                                elif (lcl_7 == 6):
                                    _rbnf_old_offset = builtin_tokens.offset
                                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_2 = lcl_8
                                    lcl_8 = maybeTuple(rbnf_tmp_1)
                                    rbnf_tmp_1_ = lcl_8
                                    lcl_8 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                    lcl_6 = lcl_8
                                else:
                                    lcl_8 = (rbnf_named__off_2, 'lang_atom lookahead failed')
                                    lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                    lcl_8 = (False, lcl_8)
                                    lcl_6 = lcl_8
                                lcl_11 = lcl_6
                            else:
                                lcl_6 = (rbnf_named__off_2, 'lang_atom got EOF')
                                lcl_6 = builtin_cons(lcl_6, builtin_nil)
                                lcl_6 = (False, lcl_6)
                                lcl_11 = lcl_6
                            lcl_10 = lcl_11
                        lcl_4 = lcl_10
                    elif (lcl_5 == 22):
                        lcl_10 = rbnf_named_parse_lang_lst(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_10
                        lcl_10 = rbnf_named__check_1[0]
                        lcl_10 = (lcl_10 == False)
                        if lcl_10:
                            lcl_10 = rbnf_named__check_1
                        else:
                            lcl_11 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_11
                            lcl_11 = builtin_tokens.offset
                            rbnf_named__off_2 = lcl_11
                            try:
                                builtin_tokens.array[(builtin_tokens.offset + 0)]
                                _rbnf_peek_tmp = True
                            except IndexError:
                                _rbnf_peek_tmp = False
                            lcl_11 = _rbnf_peek_tmp
                            if lcl_11:
                                lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                lcl_7 = lcl_7.idint
                                if (lcl_7 == 0):
                                    _rbnf_old_offset = builtin_tokens.offset
                                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_2 = lcl_8
                                    try:
                                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                        if (_rbnf_cur_token.idint is 6):
                                            builtin_tokens.offset += 1
                                        else:
                                            _rbnf_cur_token = None
                                    except IndexError:
                                        _rbnf_cur_token = None
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_3 = lcl_8
                                    lcl_8 = (rbnf_tmp_3 is None)
                                    if lcl_8:
                                        lcl_9 = builtin_tokens.offset
                                        lcl_9 = (lcl_9, 'quote ) not match')
                                        lcl_9 = builtin_cons(lcl_9, builtin_nil)
                                        lcl_9 = (False, lcl_9)
                                        lcl_8 = lcl_9
                                    else:
                                        lcl_9 = Tuple(rbnf_tmp_1)
                                        rbnf_tmp_1_ = lcl_9
                                        lcl_9 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                        lcl_8 = lcl_9
                                    lcl_6 = lcl_8
                                elif (lcl_7 == 6):
                                    _rbnf_old_offset = builtin_tokens.offset
                                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_2 = lcl_8
                                    lcl_8 = maybeTuple(rbnf_tmp_1)
                                    rbnf_tmp_1_ = lcl_8
                                    lcl_8 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                    lcl_6 = lcl_8
                                else:
                                    lcl_8 = (rbnf_named__off_2, 'lang_atom lookahead failed')
                                    lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                    lcl_8 = (False, lcl_8)
                                    lcl_6 = lcl_8
                                lcl_11 = lcl_6
                            else:
                                lcl_6 = (rbnf_named__off_2, 'lang_atom got EOF')
                                lcl_6 = builtin_cons(lcl_6, builtin_nil)
                                lcl_6 = (False, lcl_6)
                                lcl_11 = lcl_6
                            lcl_10 = lcl_11
                        lcl_4 = lcl_10
                    elif (lcl_5 == 21):
                        lcl_10 = rbnf_named_parse_lang_lst(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_10
                        lcl_10 = rbnf_named__check_1[0]
                        lcl_10 = (lcl_10 == False)
                        if lcl_10:
                            lcl_10 = rbnf_named__check_1
                        else:
                            lcl_11 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_11
                            lcl_11 = builtin_tokens.offset
                            rbnf_named__off_2 = lcl_11
                            try:
                                builtin_tokens.array[(builtin_tokens.offset + 0)]
                                _rbnf_peek_tmp = True
                            except IndexError:
                                _rbnf_peek_tmp = False
                            lcl_11 = _rbnf_peek_tmp
                            if lcl_11:
                                lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                lcl_7 = lcl_7.idint
                                if (lcl_7 == 0):
                                    _rbnf_old_offset = builtin_tokens.offset
                                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_2 = lcl_8
                                    try:
                                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                        if (_rbnf_cur_token.idint is 6):
                                            builtin_tokens.offset += 1
                                        else:
                                            _rbnf_cur_token = None
                                    except IndexError:
                                        _rbnf_cur_token = None
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_3 = lcl_8
                                    lcl_8 = (rbnf_tmp_3 is None)
                                    if lcl_8:
                                        lcl_9 = builtin_tokens.offset
                                        lcl_9 = (lcl_9, 'quote ) not match')
                                        lcl_9 = builtin_cons(lcl_9, builtin_nil)
                                        lcl_9 = (False, lcl_9)
                                        lcl_8 = lcl_9
                                    else:
                                        lcl_9 = Tuple(rbnf_tmp_1)
                                        rbnf_tmp_1_ = lcl_9
                                        lcl_9 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                        lcl_8 = lcl_9
                                    lcl_6 = lcl_8
                                elif (lcl_7 == 6):
                                    _rbnf_old_offset = builtin_tokens.offset
                                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_2 = lcl_8
                                    lcl_8 = maybeTuple(rbnf_tmp_1)
                                    rbnf_tmp_1_ = lcl_8
                                    lcl_8 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                    lcl_6 = lcl_8
                                else:
                                    lcl_8 = (rbnf_named__off_2, 'lang_atom lookahead failed')
                                    lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                    lcl_8 = (False, lcl_8)
                                    lcl_6 = lcl_8
                                lcl_11 = lcl_6
                            else:
                                lcl_6 = (rbnf_named__off_2, 'lang_atom got EOF')
                                lcl_6 = builtin_cons(lcl_6, builtin_nil)
                                lcl_6 = (False, lcl_6)
                                lcl_11 = lcl_6
                            lcl_10 = lcl_11
                        lcl_4 = lcl_10
                    elif (lcl_5 == 1):
                        lcl_10 = rbnf_named_parse_lang_lst(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_10
                        lcl_10 = rbnf_named__check_1[0]
                        lcl_10 = (lcl_10 == False)
                        if lcl_10:
                            lcl_10 = rbnf_named__check_1
                        else:
                            lcl_11 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_11
                            lcl_11 = builtin_tokens.offset
                            rbnf_named__off_2 = lcl_11
                            try:
                                builtin_tokens.array[(builtin_tokens.offset + 0)]
                                _rbnf_peek_tmp = True
                            except IndexError:
                                _rbnf_peek_tmp = False
                            lcl_11 = _rbnf_peek_tmp
                            if lcl_11:
                                lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                lcl_7 = lcl_7.idint
                                if (lcl_7 == 0):
                                    _rbnf_old_offset = builtin_tokens.offset
                                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_2 = lcl_8
                                    try:
                                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                        if (_rbnf_cur_token.idint is 6):
                                            builtin_tokens.offset += 1
                                        else:
                                            _rbnf_cur_token = None
                                    except IndexError:
                                        _rbnf_cur_token = None
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_3 = lcl_8
                                    lcl_8 = (rbnf_tmp_3 is None)
                                    if lcl_8:
                                        lcl_9 = builtin_tokens.offset
                                        lcl_9 = (lcl_9, 'quote ) not match')
                                        lcl_9 = builtin_cons(lcl_9, builtin_nil)
                                        lcl_9 = (False, lcl_9)
                                        lcl_8 = lcl_9
                                    else:
                                        lcl_9 = Tuple(rbnf_tmp_1)
                                        rbnf_tmp_1_ = lcl_9
                                        lcl_9 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                        lcl_8 = lcl_9
                                    lcl_6 = lcl_8
                                elif (lcl_7 == 6):
                                    _rbnf_old_offset = builtin_tokens.offset
                                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_2 = lcl_8
                                    lcl_8 = maybeTuple(rbnf_tmp_1)
                                    rbnf_tmp_1_ = lcl_8
                                    lcl_8 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                                    lcl_6 = lcl_8
                                else:
                                    lcl_8 = (rbnf_named__off_2, 'lang_atom lookahead failed')
                                    lcl_8 = builtin_cons(lcl_8, builtin_nil)
                                    lcl_8 = (False, lcl_8)
                                    lcl_6 = lcl_8
                                lcl_11 = lcl_6
                            else:
                                lcl_6 = (rbnf_named__off_2, 'lang_atom got EOF')
                                lcl_6 = builtin_cons(lcl_6, builtin_nil)
                                lcl_6 = (False, lcl_6)
                                lcl_11 = lcl_6
                            lcl_10 = lcl_11
                        lcl_4 = lcl_10
                    else:
                        lcl_10 = (rbnf_named__off_1, 'lang_atom lookahead failed')
                        lcl_10 = builtin_cons(lcl_10, builtin_nil)
                        lcl_10 = (False, lcl_10)
                        lcl_4 = lcl_10
                    lcl_3 = lcl_4
                else:
                    lcl_10 = (rbnf_named__off_1, 'lang_atom got EOF')
                    lcl_10 = builtin_cons(lcl_10, builtin_nil)
                    lcl_10 = (False, lcl_10)
                    lcl_3 = lcl_10
                lcl_1 = lcl_3
            elif (lcl_2 == 22):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_10 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_10
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 21):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_10 = _rbnf_cur_token
                rbnf_tmp_1 = lcl_10
                lcl_10 = (rbnf_tmp_1 is None)
                if lcl_10:
                    lcl_11 = builtin_tokens.offset
                    lcl_11 = (lcl_11, 'Int not match')
                    lcl_11 = builtin_cons(lcl_11, builtin_nil)
                    lcl_11 = (False, lcl_11)
                    lcl_10 = lcl_11
                else:
                    lcl_11 = rbnf_tmp_1.value
                    lcl_11 = int(lcl_11)
                    lcl_11 = Slot(lcl_11)
                    rbnf_tmp_1_ = lcl_11
                    lcl_11 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                    lcl_10 = lcl_11
                lcl_1 = lcl_10
            elif (lcl_2 == 21):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_10 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_10
                lcl_10 = rbnf_tmp_0.value
                lcl_10 = int(lcl_10)
                lcl_10 = Int(lcl_10)
                rbnf_tmp_1_ = lcl_10
                lcl_10 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                lcl_1 = lcl_10
            elif (lcl_2 == 1):
                lcl_10 = rbnf_named_parse_Ident(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_10
                lcl_10 = rbnf_named__check_0[0]
                lcl_10 = (lcl_10 == False)
                if lcl_10:
                    lcl_10 = rbnf_named__check_0
                else:
                    lcl_11 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_11
                    lcl_11 = Var(rbnf_tmp_0)
                    rbnf_tmp_1_ = lcl_11
                    lcl_11 = rbnf_named_lr_loop_lang_atom(rbnf_tmp_1_, builtin_state, builtin_tokens)
                    lcl_10 = lcl_11
                lcl_1 = lcl_10
            else:
                lcl_10 = (rbnf_named__off_0, 'lang_atom lookahead failed')
                lcl_10 = builtin_cons(lcl_10, builtin_nil)
                lcl_10 = (False, lcl_10)
                lcl_1 = lcl_10
            lcl_0 = lcl_1
        else:
            lcl_1 = (rbnf_named__off_0, 'lang_atom got EOF')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_lang_lst(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_rbnfmacro_6(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            rbnf_tmp_1_ = rbnf_tmp_0
            lcl_1 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_lang_stmts(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_rbnfmacro_5(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = maybeStmts(rbnf_tmp_0)
            rbnf_tmp_1_ = lcl_1
            lcl_1 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_pragma(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        try:
            builtin_tokens.array[(builtin_tokens.offset + 0)]
            _rbnf_peek_tmp = True
        except IndexError:
            _rbnf_peek_tmp = False
        lcl_0 = _rbnf_peek_tmp
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 25):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_rbnfmacro_4(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_4 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = Params(rbnf_tmp_1)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 24):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                try:
                    builtin_tokens.array[(builtin_tokens.offset + 0)]
                    _rbnf_peek_tmp = True
                except IndexError:
                    _rbnf_peek_tmp = False
                lcl_3 = _rbnf_peek_tmp
                if lcl_3:
                    lcl_5 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                    lcl_5 = lcl_5.idint
                    if (lcl_5 == 2):
                        lcl_6 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            lcl_7 = Include(None, rbnf_tmp_1)
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = (True, rbnf_tmp_1_)
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 1):
                        lcl_6 = rbnf_named_parse_Ident(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = rbnf_named__check_1[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_1
                        else:
                            lcl_7 = rbnf_named__check_1[1]
                            rbnf_tmp_1 = lcl_7
                            lcl_7 = rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens)
                            rbnf_named__check_2 = lcl_7
                            lcl_7 = rbnf_named__check_2[0]
                            lcl_7 = (lcl_7 == False)
                            if lcl_7:
                                lcl_7 = rbnf_named__check_2
                            else:
                                lcl_8 = rbnf_named__check_2[1]
                                rbnf_tmp_2 = lcl_8
                                lcl_8 = Include(rbnf_tmp_1, rbnf_tmp_2)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (True, rbnf_tmp_1_)
                                lcl_7 = lcl_8
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    else:
                        lcl_6 = (rbnf_named__off_1, 'pragma lookahead failed')
                        lcl_6 = builtin_cons(lcl_6, builtin_nil)
                        lcl_6 = (False, lcl_6)
                        lcl_4 = lcl_6
                    lcl_3 = lcl_4
                else:
                    lcl_4 = (rbnf_named__off_1, 'pragma got EOF')
                    lcl_4 = builtin_cons(lcl_4, builtin_nil)
                    lcl_4 = (False, lcl_4)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 1):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 26):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_1 = lcl_3
                lcl_3 = (rbnf_tmp_1 is None)
                if lcl_3:
                    lcl_4 = builtin_tokens.offset
                    lcl_4 = (lcl_4, 'Code not match')
                    lcl_4 = builtin_cons(lcl_4, builtin_nil)
                    lcl_4 = (False, lcl_4)
                    lcl_3 = lcl_4
                else:
                    lcl_4 = rbnf_tmp_0.value
                    lcl_5 = []
                    lcl_6 = rbnf_tmp_1.value
                    lcl_6 = (lcl_6, None)
                    _rbnf_immediate_lst = lcl_5
                    _rbnf_immediate_lst.append(lcl_6)
                    lcl_5 = _rbnf_immediate_lst
                    lcl_4 = Include(lcl_4, lcl_5)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 26):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = []
                lcl_4 = rbnf_tmp_0.value
                lcl_4 = (lcl_4, None)
                _rbnf_immediate_lst = lcl_3
                _rbnf_immediate_lst.append(lcl_4)
                lcl_3 = _rbnf_immediate_lst
                lcl_3 = Include(None, lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_3 = (True, rbnf_tmp_1_)
                lcl_1 = lcl_3
            else:
                lcl_3 = (rbnf_named__off_0, 'pragma lookahead failed')
                lcl_3 = builtin_cons(lcl_3, builtin_nil)
                lcl_3 = (False, lcl_3)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        else:
            lcl_1 = (rbnf_named__off_0, 'pragma got EOF')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_prod(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_Ident(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            try:
                builtin_tokens.array[(builtin_tokens.offset + 0)]
                _rbnf_peek_tmp = True
            except IndexError:
                _rbnf_peek_tmp = False
            lcl_1 = _rbnf_peek_tmp
            if lcl_1:
                lcl_3 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                lcl_3 = lcl_3.idint
                if (lcl_3 == 7):
                    _rbnf_old_offset = builtin_tokens.offset
                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    try:
                        builtin_tokens.array[(builtin_tokens.offset + 0)]
                        _rbnf_peek_tmp = True
                    except IndexError:
                        _rbnf_peek_tmp = False
                    lcl_4 = _rbnf_peek_tmp
                    if lcl_4:
                        lcl_6 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_6 = lcl_6.idint
                        if (lcl_6 == 8):
                            _rbnf_old_offset = builtin_tokens.offset
                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = rbnf_named_parse_def(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_7
                            lcl_7 = rbnf_named__check_3[0]
                            lcl_7 = (lcl_7 == False)
                            if lcl_7:
                                lcl_7 = rbnf_named__check_3
                            else:
                                lcl_8 = rbnf_named__check_3[1]
                                rbnf_tmp_3 = lcl_8
                                try:
                                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                    if (_rbnf_cur_token.idint is 20):
                                        builtin_tokens.offset += 1
                                    else:
                                        _rbnf_cur_token = None
                                except IndexError:
                                    _rbnf_cur_token = None
                                lcl_8 = _rbnf_cur_token
                                rbnf_tmp_4 = lcl_8
                                lcl_8 = (rbnf_tmp_4 is None)
                                if lcl_8:
                                    lcl_9 = builtin_tokens.offset
                                    lcl_9 = (lcl_9, 'quote ; not match')
                                    lcl_9 = builtin_cons(lcl_9, builtin_nil)
                                    lcl_9 = (False, lcl_9)
                                    lcl_8 = lcl_9
                                else:
                                    lcl_9 = []
                                    lcl_9 = MacroDef(rbnf_tmp_0, lcl_9, rbnf_tmp_3)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_9 = (True, rbnf_tmp_1_)
                                    lcl_8 = lcl_9
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 1):
                            lcl_7 = rbnf_named_parse_IdentList(builtin_state, builtin_tokens)
                            rbnf_named__check_2 = lcl_7
                            lcl_7 = rbnf_named__check_2[0]
                            lcl_7 = (lcl_7 == False)
                            if lcl_7:
                                lcl_7 = rbnf_named__check_2
                            else:
                                lcl_8 = rbnf_named__check_2[1]
                                rbnf_tmp_2 = lcl_8
                                try:
                                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                    if (_rbnf_cur_token.idint is 8):
                                        builtin_tokens.offset += 1
                                    else:
                                        _rbnf_cur_token = None
                                except IndexError:
                                    _rbnf_cur_token = None
                                lcl_8 = _rbnf_cur_token
                                rbnf_tmp_3 = lcl_8
                                lcl_8 = (rbnf_tmp_3 is None)
                                if lcl_8:
                                    lcl_9 = builtin_tokens.offset
                                    lcl_9 = (lcl_9, 'quote ] not match')
                                    lcl_9 = builtin_cons(lcl_9, builtin_nil)
                                    lcl_9 = (False, lcl_9)
                                    lcl_8 = lcl_9
                                else:
                                    lcl_9 = rbnf_named_parse_def(builtin_state, builtin_tokens)
                                    rbnf_named__check_4 = lcl_9
                                    lcl_9 = rbnf_named__check_4[0]
                                    lcl_9 = (lcl_9 == False)
                                    if lcl_9:
                                        lcl_9 = rbnf_named__check_4
                                    else:
                                        lcl_10 = rbnf_named__check_4[1]
                                        rbnf_tmp_4 = lcl_10
                                        try:
                                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                            if (_rbnf_cur_token.idint is 20):
                                                builtin_tokens.offset += 1
                                            else:
                                                _rbnf_cur_token = None
                                        except IndexError:
                                            _rbnf_cur_token = None
                                        lcl_10 = _rbnf_cur_token
                                        rbnf_tmp_5 = lcl_10
                                        lcl_10 = (rbnf_tmp_5 is None)
                                        if lcl_10:
                                            lcl_11 = builtin_tokens.offset
                                            lcl_11 = (lcl_11, 'quote ; not match')
                                            lcl_11 = builtin_cons(lcl_11, builtin_nil)
                                            lcl_11 = (False, lcl_11)
                                            lcl_10 = lcl_11
                                        else:
                                            lcl_11 = MacroDef(rbnf_tmp_0, rbnf_tmp_2, rbnf_tmp_4)
                                            rbnf_tmp_1_ = lcl_11
                                            lcl_11 = (True, rbnf_tmp_1_)
                                            lcl_10 = lcl_11
                                        lcl_9 = lcl_10
                                    lcl_8 = lcl_9
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        else:
                            lcl_10 = (rbnf_named__off_1, 'prod lookahead failed')
                            lcl_10 = builtin_cons(lcl_10, builtin_nil)
                            lcl_10 = (False, lcl_10)
                            lcl_5 = lcl_10
                        lcl_4 = lcl_5
                    else:
                        lcl_10 = (rbnf_named__off_1, 'prod got EOF')
                        lcl_10 = builtin_cons(lcl_10, builtin_nil)
                        lcl_10 = (False, lcl_10)
                        lcl_4 = lcl_10
                    lcl_2 = lcl_4
                elif (lcl_3 == 19):
                    lcl_10 = rbnf_named_parse_def(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = rbnf_named__check_1[0]
                    lcl_10 = (lcl_10 == False)
                    if lcl_10:
                        lcl_10 = rbnf_named__check_1
                    else:
                        lcl_11 = rbnf_named__check_1[1]
                        rbnf_tmp_1 = lcl_11
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 20):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_4 = builtin_tokens.offset
                            lcl_4 = (lcl_4, 'quote ; not match')
                            lcl_4 = builtin_cons(lcl_4, builtin_nil)
                            lcl_4 = (False, lcl_4)
                            lcl_11 = lcl_4
                        else:
                            lcl_4 = Def(rbnf_tmp_0, rbnf_tmp_1)
                            rbnf_tmp_1_ = lcl_4
                            lcl_4 = (True, rbnf_tmp_1_)
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 17):
                    lcl_10 = rbnf_named_parse_def(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = rbnf_named__check_1[0]
                    lcl_10 = (lcl_10 == False)
                    if lcl_10:
                        lcl_10 = rbnf_named__check_1
                    else:
                        lcl_11 = rbnf_named__check_1[1]
                        rbnf_tmp_1 = lcl_11
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 20):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_4 = builtin_tokens.offset
                            lcl_4 = (lcl_4, 'quote ; not match')
                            lcl_4 = builtin_cons(lcl_4, builtin_nil)
                            lcl_4 = (False, lcl_4)
                            lcl_11 = lcl_4
                        else:
                            lcl_4 = Def(rbnf_tmp_0, rbnf_tmp_1)
                            rbnf_tmp_1_ = lcl_4
                            lcl_4 = (True, rbnf_tmp_1_)
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 18):
                    lcl_10 = rbnf_named_parse_def(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = rbnf_named__check_1[0]
                    lcl_10 = (lcl_10 == False)
                    if lcl_10:
                        lcl_10 = rbnf_named__check_1
                    else:
                        lcl_11 = rbnf_named__check_1[1]
                        rbnf_tmp_1 = lcl_11
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 20):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_4 = builtin_tokens.offset
                            lcl_4 = (lcl_4, 'quote ; not match')
                            lcl_4 = builtin_cons(lcl_4, builtin_nil)
                            lcl_4 = (False, lcl_4)
                            lcl_11 = lcl_4
                        else:
                            lcl_4 = Def(rbnf_tmp_0, rbnf_tmp_1)
                            rbnf_tmp_1_ = lcl_4
                            lcl_4 = (True, rbnf_tmp_1_)
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 16):
                    lcl_10 = rbnf_named_parse_def(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = rbnf_named__check_1[0]
                    lcl_10 = (lcl_10 == False)
                    if lcl_10:
                        lcl_10 = rbnf_named__check_1
                    else:
                        lcl_11 = rbnf_named__check_1[1]
                        rbnf_tmp_1 = lcl_11
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 20):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_4 = builtin_tokens.offset
                            lcl_4 = (lcl_4, 'quote ; not match')
                            lcl_4 = builtin_cons(lcl_4, builtin_nil)
                            lcl_4 = (False, lcl_4)
                            lcl_11 = lcl_4
                        else:
                            lcl_4 = Def(rbnf_tmp_0, rbnf_tmp_1)
                            rbnf_tmp_1_ = lcl_4
                            lcl_4 = (True, rbnf_tmp_1_)
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                else:
                    lcl_10 = (rbnf_named__off_0, 'prod lookahead failed')
                    lcl_10 = builtin_cons(lcl_10, builtin_nil)
                    lcl_10 = (False, lcl_10)
                    lcl_2 = lcl_10
                lcl_1 = lcl_2
            else:
                lcl_10 = (rbnf_named__off_0, 'prod got EOF')
                lcl_10 = builtin_cons(lcl_10, builtin_nil)
                lcl_10 = (False, lcl_10)
                lcl_1 = lcl_10
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rbnfmacro_0(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_expr(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = []
            _rbnf_immediate_lst = lcl_1
            _rbnf_immediate_lst.append(rbnf_tmp_0)
            lcl_1 = _rbnf_immediate_lst
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_rbnfmacro_0(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rbnfmacro_1(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_atomExpr(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = []
            _rbnf_immediate_lst = lcl_1
            _rbnf_immediate_lst.append(rbnf_tmp_0)
            lcl_1 = _rbnf_immediate_lst
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_rbnfmacro_1(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rbnfmacro_2(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_cseq(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = []
            _rbnf_immediate_lst = lcl_1
            _rbnf_immediate_lst.append(rbnf_tmp_0)
            lcl_1 = _rbnf_immediate_lst
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_rbnfmacro_2(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rbnfmacro_3(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_rewrite(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = []
            _rbnf_immediate_lst = lcl_1
            _rbnf_immediate_lst.append(rbnf_tmp_0)
            lcl_1 = _rbnf_immediate_lst
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_rbnfmacro_3(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rbnfmacro_4(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_Ident(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = []
            _rbnf_immediate_lst = lcl_1
            _rbnf_immediate_lst.append(rbnf_tmp_0)
            lcl_1 = _rbnf_immediate_lst
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_rbnfmacro_4(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rbnfmacro_5(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_lang(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = []
            _rbnf_immediate_lst = lcl_1
            _rbnf_immediate_lst.append(rbnf_tmp_0)
            lcl_1 = _rbnf_immediate_lst
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_rbnfmacro_5(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rbnfmacro_6(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_lang(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = []
            _rbnf_immediate_lst = lcl_1
            _rbnf_immediate_lst.append(rbnf_tmp_0)
            lcl_1 = _rbnf_immediate_lst
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_rbnfmacro_6(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rbnfmacro_7(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_filename(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = []
            _rbnf_immediate_lst = lcl_1
            _rbnf_immediate_lst.append(rbnf_tmp_0)
            lcl_1 = _rbnf_immediate_lst
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_rbnfmacro_7(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rbnfmacro_8(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        try:
            builtin_tokens.array[(builtin_tokens.offset + 0)]
            _rbnf_peek_tmp = True
        except IndexError:
            _rbnf_peek_tmp = False
        lcl_0 = _rbnf_peek_tmp
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 25):
                lcl_3 = rbnf_named_parse_pragma(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    lcl_4 = []
                    _rbnf_immediate_lst = lcl_4
                    _rbnf_immediate_lst.append(rbnf_tmp_0)
                    lcl_4 = _rbnf_immediate_lst
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_lr_loop_rbnfmacro_8(rbnf_tmp_1_, builtin_state, builtin_tokens)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 24):
                lcl_3 = rbnf_named_parse_pragma(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    lcl_4 = []
                    _rbnf_immediate_lst = lcl_4
                    _rbnf_immediate_lst.append(rbnf_tmp_0)
                    lcl_4 = _rbnf_immediate_lst
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_lr_loop_rbnfmacro_8(rbnf_tmp_1_, builtin_state, builtin_tokens)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 1):
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                try:
                    builtin_tokens.array[(builtin_tokens.offset + 1)]
                    _rbnf_peek_tmp = True
                except IndexError:
                    _rbnf_peek_tmp = False
                lcl_3 = _rbnf_peek_tmp
                if lcl_3:
                    lcl_5 = builtin_tokens.array[(builtin_tokens.offset + 1)]
                    lcl_5 = lcl_5.idint
                    if (lcl_5 == 7):
                        lcl_6 = rbnf_named_parse_prod(builtin_state, builtin_tokens)
                        rbnf_named__check_0 = lcl_6
                        lcl_6 = rbnf_named__check_0[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_0
                        else:
                            lcl_7 = rbnf_named__check_0[1]
                            rbnf_tmp_0 = lcl_7
                            lcl_7 = []
                            _rbnf_immediate_lst = lcl_7
                            _rbnf_immediate_lst.append(rbnf_tmp_0)
                            lcl_7 = _rbnf_immediate_lst
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = rbnf_named_lr_loop_rbnfmacro_8(rbnf_tmp_1_, builtin_state, builtin_tokens)
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 19):
                        lcl_6 = rbnf_named_parse_prod(builtin_state, builtin_tokens)
                        rbnf_named__check_0 = lcl_6
                        lcl_6 = rbnf_named__check_0[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_0
                        else:
                            lcl_7 = rbnf_named__check_0[1]
                            rbnf_tmp_0 = lcl_7
                            lcl_7 = []
                            _rbnf_immediate_lst = lcl_7
                            _rbnf_immediate_lst.append(rbnf_tmp_0)
                            lcl_7 = _rbnf_immediate_lst
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = rbnf_named_lr_loop_rbnfmacro_8(rbnf_tmp_1_, builtin_state, builtin_tokens)
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 17):
                        lcl_6 = rbnf_named_parse_prod(builtin_state, builtin_tokens)
                        rbnf_named__check_0 = lcl_6
                        lcl_6 = rbnf_named__check_0[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_0
                        else:
                            lcl_7 = rbnf_named__check_0[1]
                            rbnf_tmp_0 = lcl_7
                            lcl_7 = []
                            _rbnf_immediate_lst = lcl_7
                            _rbnf_immediate_lst.append(rbnf_tmp_0)
                            lcl_7 = _rbnf_immediate_lst
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = rbnf_named_lr_loop_rbnfmacro_8(rbnf_tmp_1_, builtin_state, builtin_tokens)
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 18):
                        lcl_6 = rbnf_named_parse_prod(builtin_state, builtin_tokens)
                        rbnf_named__check_0 = lcl_6
                        lcl_6 = rbnf_named__check_0[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_0
                        else:
                            lcl_7 = rbnf_named__check_0[1]
                            rbnf_tmp_0 = lcl_7
                            lcl_7 = []
                            _rbnf_immediate_lst = lcl_7
                            _rbnf_immediate_lst.append(rbnf_tmp_0)
                            lcl_7 = _rbnf_immediate_lst
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = rbnf_named_lr_loop_rbnfmacro_8(rbnf_tmp_1_, builtin_state, builtin_tokens)
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 16):
                        lcl_6 = rbnf_named_parse_prod(builtin_state, builtin_tokens)
                        rbnf_named__check_0 = lcl_6
                        lcl_6 = rbnf_named__check_0[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_0
                        else:
                            lcl_7 = rbnf_named__check_0[1]
                            rbnf_tmp_0 = lcl_7
                            lcl_7 = []
                            _rbnf_immediate_lst = lcl_7
                            _rbnf_immediate_lst.append(rbnf_tmp_0)
                            lcl_7 = _rbnf_immediate_lst
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = rbnf_named_lr_loop_rbnfmacro_8(rbnf_tmp_1_, builtin_state, builtin_tokens)
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 26):
                        lcl_6 = rbnf_named_parse_pragma(builtin_state, builtin_tokens)
                        rbnf_named__check_0 = lcl_6
                        lcl_6 = rbnf_named__check_0[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_0
                        else:
                            lcl_7 = rbnf_named__check_0[1]
                            rbnf_tmp_0 = lcl_7
                            lcl_7 = []
                            _rbnf_immediate_lst = lcl_7
                            _rbnf_immediate_lst.append(rbnf_tmp_0)
                            lcl_7 = _rbnf_immediate_lst
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = rbnf_named_lr_loop_rbnfmacro_8(rbnf_tmp_1_, builtin_state, builtin_tokens)
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    else:
                        lcl_6 = rbnf_named_parse_prod(builtin_state, builtin_tokens)
                        rbnf_named__check_0 = lcl_6
                        lcl_6 = rbnf_named__check_0[0]
                        lcl_6 = (lcl_6 == False)
                        if lcl_6:
                            lcl_6 = rbnf_named__check_0
                        else:
                            lcl_7 = rbnf_named__check_0[1]
                            rbnf_tmp_0 = lcl_7
                            lcl_7 = []
                            _rbnf_immediate_lst = lcl_7
                            _rbnf_immediate_lst.append(rbnf_tmp_0)
                            lcl_7 = _rbnf_immediate_lst
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = rbnf_named_lr_loop_rbnfmacro_8(rbnf_tmp_1_, builtin_state, builtin_tokens)
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    lcl_3 = lcl_4
                else:
                    lcl_4 = (rbnf_named__off_1, 'rbnfmacro_8 got EOF')
                    lcl_4 = builtin_cons(lcl_4, builtin_nil)
                    lcl_4 = (False, lcl_4)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 26):
                lcl_3 = rbnf_named_parse_pragma(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = rbnf_named__check_0[0]
                lcl_3 = (lcl_3 == False)
                if lcl_3:
                    lcl_3 = rbnf_named__check_0
                else:
                    lcl_4 = rbnf_named__check_0[1]
                    rbnf_tmp_0 = lcl_4
                    lcl_4 = []
                    _rbnf_immediate_lst = lcl_4
                    _rbnf_immediate_lst.append(rbnf_tmp_0)
                    lcl_4 = _rbnf_immediate_lst
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_lr_loop_rbnfmacro_8(rbnf_tmp_1_, builtin_state, builtin_tokens)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_3 = (rbnf_named__off_0, 'rbnfmacro_8 lookahead failed')
                lcl_3 = builtin_cons(lcl_3, builtin_nil)
                lcl_3 = (False, lcl_3)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        else:
            lcl_1 = (rbnf_named__off_0, 'rbnfmacro_8 got EOF')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rewrite(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_seq(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            try:
                builtin_tokens.array[(builtin_tokens.offset + 0)]
                _rbnf_peek_tmp = True
            except IndexError:
                _rbnf_peek_tmp = False
            lcl_1 = _rbnf_peek_tmp
            if lcl_1:
                lcl_3 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                lcl_3 = lcl_3.idint
                if (lcl_3 == 14):
                    _rbnf_old_offset = builtin_tokens.offset
                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = rbnf_named_parse_lang_stmts(builtin_state, builtin_tokens)
                    rbnf_named__check_2 = lcl_4
                    lcl_4 = rbnf_named__check_2[0]
                    lcl_4 = (lcl_4 == False)
                    if lcl_4:
                        lcl_4 = rbnf_named__check_2
                    else:
                        lcl_5 = rbnf_named__check_2[1]
                        rbnf_tmp_2 = lcl_5
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 15):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_3 = lcl_5
                        lcl_5 = (rbnf_tmp_3 is None)
                        if lcl_5:
                            lcl_6 = builtin_tokens.offset
                            lcl_6 = (lcl_6, 'quote } not match')
                            lcl_6 = builtin_cons(lcl_6, builtin_nil)
                            lcl_6 = (False, lcl_6)
                            lcl_5 = lcl_6
                        else:
                            lcl_6 = seq(rbnf_tmp_0)
                            lcl_6 = Rewrite(lcl_6, rbnf_tmp_2)
                            rbnf_tmp_1_ = lcl_6
                            lcl_6 = (True, rbnf_tmp_1_)
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                else:
                    lcl_4 = seq(rbnf_tmp_0)
                    lcl_4 = Rewrite(lcl_4, None)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_2 = lcl_4
                lcl_1 = lcl_2
            else:
                lcl_2 = (rbnf_named__off_0, 'rewrite got EOF')
                lcl_2 = builtin_cons(lcl_2, builtin_nil)
                lcl_2 = (False, lcl_2)
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_seq(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_rbnfmacro_1(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = (lcl_0 == False)
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            rbnf_tmp_1_ = rbnf_tmp_0
            lcl_1 = (True, rbnf_tmp_1_)
            lcl_0 = lcl_1
        return lcl_0
    return rbnf_named_parse_START