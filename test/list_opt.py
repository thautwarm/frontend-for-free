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

def append(x, b):
    x.append(b)
    return x

def mk_parser():
    pass

    def rbnf_named_lr_step_rbnfmacro_0(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 6):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_1 = lcl_0
        lcl_0 = (rbnf_tmp_1 is None)
        if lcl_0:
            lcl_0 = None
        else:
            lcl_1 = append(rbnf_tmp_0, rbnf_tmp_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_lr_loop_rbnfmacro_0(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_rbnfmacro_0_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_rbnfmacro_0(rbnf_named_lr_rbnfmacro_0_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_rbnfmacro_0_try = lcl_0
        lcl_0 = (rbnf_named_lr_rbnfmacro_0_try is not None)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            rbnf_named_lr_rbnfmacro_0_reduce = rbnf_named_lr_rbnfmacro_0_try
            lcl_1 = rbnf_named_lr_step_rbnfmacro_0(rbnf_named_lr_rbnfmacro_0_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_rbnfmacro_0_try = lcl_1
            lcl_1 = (rbnf_named_lr_rbnfmacro_0_try is not None)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_1 = (True, rbnf_named_lr_rbnfmacro_0_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = rbnf_named_lr_rbnfmacro_0_try
        return lcl_0

    def rbnf_named_parse_START(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_g(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('START', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_g(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 6):
                lcl_3 = rbnf_named_parse_rbnfmacro_0(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('g', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 5):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('g', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_rbnfmacro_0(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 6):
                builtin_tokens.offset += 1
            else:
                _rbnf_cur_token = None
        except IndexError:
            _rbnf_cur_token = None
        lcl_0 = _rbnf_cur_token
        rbnf_tmp_0 = lcl_0
        lcl_0 = (rbnf_tmp_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            lcl_1 = []
            _rbnf_immediate_lst = lcl_1
            _rbnf_immediate_lst.append(rbnf_tmp_0)
            lcl_1 = _rbnf_immediate_lst
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_rbnfmacro_0(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0
    return rbnf_named_parse_START