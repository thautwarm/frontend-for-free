
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
import operator


def unwrap(x):
    return int(x.value)


ops = {
    '+': operator.add,
    '-': operator.sub,
    '*': operator.mul,
    '/': operator.floordiv,
}


def arith(op, lhs, rhs):
    return ops[op.value](lhs, rhs)
from typing import Generic, TypeVar
T = TypeVar('T')

class Tokens:
    __slots__ = ['array', 'offset']

    def __init__(self, array):
        self.array = array
        self.offset = 0

class State:

    def __init__(self):
        pass

class AST(Generic[T]):
    __slots__ = ['tag', 'contents']

    def __init__(self, tag: str, contents: T):
        self.tag = tag
        self.contents = contents

class Nil:
    nil = None
    __slots__ = []

    def __init__(self):
        if Nil.nil is None:
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

class Cons:
    __slots__ = ['head', 'tail']

    def __init__(self, _head, _tail):
        self.head = _head
        self.tail = _tail

    def __len__(self):
        nil = _nil
        l = 0
        while self is not nil:
            l += 1
            self = self.tail
        return l

    def __iter__(self):
        nil = _nil
        while self is not nil:
            yield self.head
            self = self.tail

    def __getitem__(self, n):
        while n != 0:
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

    def rbnf_named_lr_step_rbnfmacro_0(rbnf_tmp_0, builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        try:
            builtin_tokens.array[builtin_tokens.offset + 0]
            _rbnf_peek_tmp = True
        except IndexError:
            _rbnf_peek_tmp = False
        lcl_0 = _rbnf_peek_tmp
        if lcl_0:
            lcl_2 = builtin_tokens.array[builtin_tokens.offset + 0]
            lcl_2 = lcl_2.idint
            if lcl_2 == 6:
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = _rbnf_old_offset + 1
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_1 = lcl_3
                lcl_3 = rbnf_named_parse_atom(builtin_state, builtin_tokens)
                rbnf_named__check_2 = lcl_3
                lcl_3 = rbnf_named__check_2[0]
                lcl_3 = lcl_3 == False
                if lcl_3:
                    lcl_3 = rbnf_named__check_2
                else:
                    lcl_4 = rbnf_named__check_2[1]
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = arith(rbnf_tmp_1, rbnf_tmp_0, rbnf_tmp_2)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif lcl_2 == 5:
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = _rbnf_old_offset + 1
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_1 = lcl_3
                lcl_3 = rbnf_named_parse_atom(builtin_state, builtin_tokens)
                rbnf_named__check_2 = lcl_3
                lcl_3 = rbnf_named__check_2[0]
                lcl_3 = lcl_3 == False
                if lcl_3:
                    lcl_3 = rbnf_named__check_2
                else:
                    lcl_4 = rbnf_named__check_2[1]
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = arith(rbnf_tmp_1, rbnf_tmp_0, rbnf_tmp_2)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_3 = (rbnf_named__off_0, 'rbnfmacro_0 lookahead failed')
                lcl_3 = builtin_cons(lcl_3, builtin_nil)
                lcl_3 = (False, lcl_3)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        else:
            lcl_1 = (rbnf_named__off_0, 'rbnfmacro_0 lookahead failed')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_0(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_0_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_0(rbnf_named_lr_rbnfmacro_0_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_0_try = lcl_0
        lcl_0 = rbnf_named_lr_rbnfmacro_0_try[0]
        lcl_0 = lcl_0 is not False
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_0_try[1]
            rbnf_named_lr_rbnfmacro_0_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_rbnfmacro_0(rbnf_named_lr_rbnfmacro_0_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_0_try = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_0_try[0]
            lcl_1 = lcl_1 is not False
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = lcl_0 == rbnf_named__off_0
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_0_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_0_try
        return lcl_0

    def rbnf_named_lr_step_rbnfmacro_1(rbnf_tmp_0, builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        try:
            builtin_tokens.array[builtin_tokens.offset + 0]
            _rbnf_peek_tmp = True
        except IndexError:
            _rbnf_peek_tmp = False
        lcl_0 = _rbnf_peek_tmp
        if lcl_0:
            lcl_2 = builtin_tokens.array[builtin_tokens.offset + 0]
            lcl_2 = lcl_2.idint
            if lcl_2 == 8:
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = _rbnf_old_offset + 1
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_1 = lcl_3
                lcl_3 = rbnf_named_parse_rbnfmacro_0(builtin_state, builtin_tokens)
                rbnf_named__check_2 = lcl_3
                lcl_3 = rbnf_named__check_2[0]
                lcl_3 = lcl_3 == False
                if lcl_3:
                    lcl_3 = rbnf_named__check_2
                else:
                    lcl_4 = rbnf_named__check_2[1]
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = arith(rbnf_tmp_1, rbnf_tmp_0, rbnf_tmp_2)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif lcl_2 == 7:
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = _rbnf_old_offset + 1
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_1 = lcl_3
                lcl_3 = rbnf_named_parse_rbnfmacro_0(builtin_state, builtin_tokens)
                rbnf_named__check_2 = lcl_3
                lcl_3 = rbnf_named__check_2[0]
                lcl_3 = lcl_3 == False
                if lcl_3:
                    lcl_3 = rbnf_named__check_2
                else:
                    lcl_4 = rbnf_named__check_2[1]
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = arith(rbnf_tmp_1, rbnf_tmp_0, rbnf_tmp_2)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (True, rbnf_tmp_1_)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_3 = (rbnf_named__off_0, 'rbnfmacro_1 lookahead failed')
                lcl_3 = builtin_cons(lcl_3, builtin_nil)
                lcl_3 = (False, lcl_3)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        else:
            lcl_1 = (rbnf_named__off_0, 'rbnfmacro_1 lookahead failed')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_1(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_1_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_1(rbnf_named_lr_rbnfmacro_1_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_1_try = lcl_0
        lcl_0 = rbnf_named_lr_rbnfmacro_1_try[0]
        lcl_0 = lcl_0 is not False
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_1_try[1]
            rbnf_named_lr_rbnfmacro_1_reduce = lcl_1
            lcl_1 = rbnf_named_lr_step_rbnfmacro_1(rbnf_named_lr_rbnfmacro_1_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_1_try = lcl_1
            lcl_1 = rbnf_named_lr_rbnfmacro_1_try[0]
            lcl_1 = lcl_1 is not False
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = lcl_0 == rbnf_named__off_0
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_1_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_1_try
        return lcl_0

    def rbnf_named_parse_START(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if _rbnf_cur_token.idint is 0:
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_0 = lcl_0
        lcl_0 = rbnf_tmp_0 is None
        if lcl_0:
            lcl_1 = builtin_tokens.offset
            lcl_1 = (lcl_1, 'BOF not match')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = rbnf_named_parse_arith(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = rbnf_named__check_1[0]
            lcl_1 = lcl_1 == False
            if lcl_1:
                lcl_1 = rbnf_named__check_1
            else:
                lcl_2 = rbnf_named__check_1[1]
                rbnf_tmp_1 = lcl_2
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if _rbnf_cur_token.idint is 1:
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_2 = _rbnf_cur_token
                rbnf_tmp_2 = lcl_2
                lcl_2 = rbnf_tmp_2 is None
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

    def rbnf_named_parse_arith(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_rbnfmacro_1(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = lcl_0 == False
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
            builtin_tokens.array[builtin_tokens.offset + 0]
            _rbnf_peek_tmp = True
        except IndexError:
            _rbnf_peek_tmp = False
        lcl_0 = _rbnf_peek_tmp
        if lcl_0:
            lcl_2 = builtin_tokens.array[builtin_tokens.offset + 0]
            lcl_2 = lcl_2.idint
            if lcl_2 == 3:
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = _rbnf_old_offset + 1
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_arith(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = rbnf_named__check_1[0]
                lcl_3 = lcl_3 == False
                if lcl_3:
                    lcl_3 = rbnf_named__check_1
                else:
                    lcl_4 = rbnf_named__check_1[1]
                    rbnf_tmp_1 = lcl_4
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if _rbnf_cur_token.idint is 4:
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = rbnf_tmp_2 is None
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
            elif lcl_2 == 2:
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = _rbnf_old_offset + 1
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = unwrap(rbnf_tmp_0)
                rbnf_tmp_1_ = lcl_3
                lcl_3 = (True, rbnf_tmp_1_)
                lcl_1 = lcl_3
            else:
                lcl_3 = (rbnf_named__off_0, 'atom lookahead failed')
                lcl_3 = builtin_cons(lcl_3, builtin_nil)
                lcl_3 = (False, lcl_3)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        else:
            lcl_1 = (rbnf_named__off_0, 'atom lookahead failed')
            lcl_1 = builtin_cons(lcl_1, builtin_nil)
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rbnfmacro_0(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_atom(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = lcl_0 == False
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            rbnf_tmp_1_ = rbnf_tmp_0
            lcl_1 = rbnf_named_lr_loop_rbnfmacro_0(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_rbnfmacro_1(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_rbnfmacro_0(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = rbnf_named__check_0[0]
        lcl_0 = lcl_0 == False
        if lcl_0:
            lcl_0 = rbnf_named__check_0
        else:
            lcl_1 = rbnf_named__check_0[1]
            rbnf_tmp_0 = lcl_1
            rbnf_tmp_1_ = rbnf_tmp_0
            lcl_1 = rbnf_named_lr_loop_rbnfmacro_1(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0
    return rbnf_named_parse_START