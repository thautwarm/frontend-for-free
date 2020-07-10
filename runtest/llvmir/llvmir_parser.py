
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

    def rbnf_named_lr_step_BasicBlockList(rbnf_tmp_0, builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_BasicBlock(builtin_state, builtin_tokens)
        rbnf_named__check_1 = lcl_0
        lcl_0 = (rbnf_named__check_1 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_1 = rbnf_named__check_1
            lcl_1 = (rbnf_tmp_0, rbnf_tmp_1)
            lcl_1 = builtin_mk_ast('BasicBlockList', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_lr_loop_BasicBlockList(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_BasicBlockList_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_BasicBlockList(rbnf_named_lr_BasicBlockList_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_BasicBlockList_try = lcl_0
        lcl_0 = (rbnf_named_lr_BasicBlockList_try is not None)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            rbnf_named_lr_BasicBlockList_reduce = rbnf_named_lr_BasicBlockList_try
            lcl_1 = rbnf_named_lr_step_BasicBlockList(rbnf_named_lr_BasicBlockList_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_BasicBlockList_try = lcl_1
            lcl_1 = (rbnf_named_lr_BasicBlockList_try is not None)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_0 = rbnf_named_lr_BasicBlockList_reduce
        else:
            lcl_0 = rbnf_named_lr_BasicBlockList_try
        return lcl_0

    def rbnf_named_lr_step_CaseList(rbnf_tmp_0, builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_Case(builtin_state, builtin_tokens)
        rbnf_named__check_1 = lcl_0
        lcl_0 = (rbnf_named__check_1 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_1 = rbnf_named__check_1
            lcl_1 = (rbnf_tmp_0, rbnf_tmp_1)
            lcl_1 = builtin_mk_ast('CaseList', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_lr_loop_CaseList(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_CaseList_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_CaseList(rbnf_named_lr_CaseList_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_CaseList_try = lcl_0
        lcl_0 = (rbnf_named_lr_CaseList_try is not None)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            rbnf_named_lr_CaseList_reduce = rbnf_named_lr_CaseList_try
            lcl_1 = rbnf_named_lr_step_CaseList(rbnf_named_lr_CaseList_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_CaseList_try = lcl_1
            lcl_1 = (rbnf_named_lr_CaseList_try is not None)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_0 = rbnf_named_lr_CaseList_reduce
        else:
            lcl_0 = rbnf_named_lr_CaseList_try
        return lcl_0

    def rbnf_named_lr_step_GEPConstIndexList(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 19):
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
            lcl_1 = rbnf_named_parse_GEPConstIndex(builtin_state, builtin_tokens)
            rbnf_named__check_2 = lcl_1
            lcl_1 = (rbnf_named__check_2 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_2 = rbnf_named__check_2
                lcl_2 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                lcl_2 = builtin_mk_ast('GEPConstIndexList', lcl_2)
                rbnf_tmp_1_ = lcl_2
                lcl_1 = rbnf_tmp_1_
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_GEPConstIndexList(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_GEPConstIndexList_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_GEPConstIndexList(rbnf_named_lr_GEPConstIndexList_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_GEPConstIndexList_try = lcl_0
        lcl_0 = (rbnf_named_lr_GEPConstIndexList_try is not None)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            rbnf_named_lr_GEPConstIndexList_reduce = rbnf_named_lr_GEPConstIndexList_try
            lcl_1 = rbnf_named_lr_step_GEPConstIndexList(rbnf_named_lr_GEPConstIndexList_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_GEPConstIndexList_try = lcl_1
            lcl_1 = (rbnf_named_lr_GEPConstIndexList_try is not None)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_0 = rbnf_named_lr_GEPConstIndexList_reduce
        else:
            lcl_0 = rbnf_named_lr_GEPConstIndexList_try
        return lcl_0

    def rbnf_named_lr_step_IncList(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 19):
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
            lcl_1 = (rbnf_tmp_0, rbnf_tmp_1)
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_parse_Inc(builtin_state, builtin_tokens)
            rbnf_named__check_2 = lcl_1
            lcl_1 = (rbnf_named__check_2 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_2 = rbnf_named__check_2
                lcl_2 = (rbnf_tmp_1_, rbnf_tmp_2)
                lcl_2 = builtin_mk_ast('IncList', lcl_2)
                rbnf_tmp_2_ = lcl_2
                lcl_1 = rbnf_tmp_2_
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_IncList(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_IncList_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_IncList(rbnf_named_lr_IncList_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_IncList_try = lcl_0
        lcl_0 = (rbnf_named_lr_IncList_try is not None)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            rbnf_named_lr_IncList_reduce = rbnf_named_lr_IncList_try
            lcl_1 = rbnf_named_lr_step_IncList(rbnf_named_lr_IncList_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_IncList_try = lcl_1
            lcl_1 = (rbnf_named_lr_IncList_try is not None)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_0 = rbnf_named_lr_IncList_reduce
        else:
            lcl_0 = rbnf_named_lr_IncList_try
        return lcl_0

    def rbnf_named_lr_step_IndexList(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 19):
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
            lcl_1 = rbnf_named_parse_IntLit(builtin_state, builtin_tokens)
            rbnf_named__check_2 = lcl_1
            lcl_1 = (rbnf_named__check_2 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_2 = rbnf_named__check_2
                lcl_2 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                lcl_2 = builtin_mk_ast('IndexList', lcl_2)
                rbnf_tmp_1_ = lcl_2
                lcl_1 = rbnf_tmp_1_
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_IndexList(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_IndexList_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_IndexList(rbnf_named_lr_IndexList_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_IndexList_try = lcl_0
        lcl_0 = (rbnf_named_lr_IndexList_try is not None)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            rbnf_named_lr_IndexList_reduce = rbnf_named_lr_IndexList_try
            lcl_1 = rbnf_named_lr_step_IndexList(rbnf_named_lr_IndexList_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_IndexList_try = lcl_1
            lcl_1 = (rbnf_named_lr_IndexList_try is not None)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_0 = rbnf_named_lr_IndexList_reduce
        else:
            lcl_0 = rbnf_named_lr_IndexList_try
        return lcl_0

    def rbnf_named_lr_step_InstructionList(rbnf_tmp_0, builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_Instruction(builtin_state, builtin_tokens)
        rbnf_named__check_1 = lcl_0
        lcl_0 = (rbnf_named__check_1 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_1 = rbnf_named__check_1
            lcl_1 = (rbnf_tmp_0, rbnf_tmp_1)
            lcl_1 = builtin_mk_ast('InstructionList', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_lr_loop_InstructionList(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_InstructionList_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_InstructionList(rbnf_named_lr_InstructionList_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_InstructionList_try = lcl_0
        lcl_0 = (rbnf_named_lr_InstructionList_try is not None)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            rbnf_named_lr_InstructionList_reduce = rbnf_named_lr_InstructionList_try
            lcl_1 = rbnf_named_lr_step_InstructionList(rbnf_named_lr_InstructionList_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_InstructionList_try = lcl_1
            lcl_1 = (rbnf_named_lr_InstructionList_try is not None)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_0 = rbnf_named_lr_InstructionList_reduce
        else:
            lcl_0 = rbnf_named_lr_InstructionList_try
        return lcl_0

    def rbnf_named_lr_step_OverflowFlags(rbnf_tmp_0, builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 50):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_1 = lcl_3
                lcl_3 = (rbnf_tmp_0, rbnf_tmp_1)
                lcl_3 = builtin_mk_ast('OverflowFlags', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 49):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_1 = lcl_3
                lcl_3 = (rbnf_tmp_0, rbnf_tmp_1)
                lcl_3 = builtin_mk_ast('OverflowFlags', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_lr_loop_OverflowFlags(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_OverflowFlags_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_OverflowFlags(rbnf_named_lr_OverflowFlags_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_OverflowFlags_try = lcl_0
        lcl_0 = (rbnf_named_lr_OverflowFlags_try is not None)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            rbnf_named_lr_OverflowFlags_reduce = rbnf_named_lr_OverflowFlags_try
            lcl_1 = rbnf_named_lr_step_OverflowFlags(rbnf_named_lr_OverflowFlags_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_OverflowFlags_try = lcl_1
            lcl_1 = (rbnf_named_lr_OverflowFlags_try is not None)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_0 = rbnf_named_lr_OverflowFlags_reduce
        else:
            lcl_0 = rbnf_named_lr_OverflowFlags_try
        return lcl_0

    def rbnf_named_lr_step_ParamAttrList(rbnf_tmp_0, builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_ParamAttr(builtin_state, builtin_tokens)
        rbnf_named__check_1 = lcl_0
        lcl_0 = (rbnf_named__check_1 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_1 = rbnf_named__check_1
            lcl_1 = (rbnf_tmp_0, rbnf_tmp_1)
            lcl_1 = builtin_mk_ast('ParamAttrList', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_lr_loop_ParamAttrList(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_ParamAttrList_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_ParamAttrList(rbnf_named_lr_ParamAttrList_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_ParamAttrList_try = lcl_0
        lcl_0 = (rbnf_named_lr_ParamAttrList_try is not None)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            rbnf_named_lr_ParamAttrList_reduce = rbnf_named_lr_ParamAttrList_try
            lcl_1 = rbnf_named_lr_step_ParamAttrList(rbnf_named_lr_ParamAttrList_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_ParamAttrList_try = lcl_1
            lcl_1 = (rbnf_named_lr_ParamAttrList_try is not None)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_0 = rbnf_named_lr_ParamAttrList_reduce
        else:
            lcl_0 = rbnf_named_lr_ParamAttrList_try
        return lcl_0

    def rbnf_named_lr_step_ParamList(rbnf_tmp_0, builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_Param(builtin_state, builtin_tokens)
        rbnf_named__check_1 = lcl_0
        lcl_0 = (rbnf_named__check_1 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_1 = rbnf_named__check_1
            lcl_1 = (rbnf_tmp_0, rbnf_tmp_1)
            lcl_1 = builtin_mk_ast('ParamList', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_lr_loop_ParamList(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_ParamList_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_ParamList(rbnf_named_lr_ParamList_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_ParamList_try = lcl_0
        lcl_0 = (rbnf_named_lr_ParamList_try is not None)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            rbnf_named_lr_ParamList_reduce = rbnf_named_lr_ParamList_try
            lcl_1 = rbnf_named_lr_step_ParamList(rbnf_named_lr_ParamList_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_ParamList_try = lcl_1
            lcl_1 = (rbnf_named_lr_ParamList_try is not None)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_0 = rbnf_named_lr_ParamList_reduce
        else:
            lcl_0 = rbnf_named_lr_ParamList_try
        return lcl_0

    def rbnf_named_lr_step_SepTypeValueList(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 19):
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
            lcl_1 = (rbnf_tmp_0, rbnf_tmp_1)
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
            rbnf_named__check_2 = lcl_1
            lcl_1 = (rbnf_named__check_2 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_2 = rbnf_named__check_2
                lcl_2 = (rbnf_tmp_1_, rbnf_tmp_2)
                lcl_2 = builtin_mk_ast('SepTypeValueList', lcl_2)
                rbnf_tmp_2_ = lcl_2
                lcl_1 = rbnf_tmp_2_
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_SepTypeValueList(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_SepTypeValueList_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_SepTypeValueList(rbnf_named_lr_SepTypeValueList_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_SepTypeValueList_try = lcl_0
        lcl_0 = (rbnf_named_lr_SepTypeValueList_try is not None)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            rbnf_named_lr_SepTypeValueList_reduce = rbnf_named_lr_SepTypeValueList_try
            lcl_1 = rbnf_named_lr_step_SepTypeValueList(rbnf_named_lr_SepTypeValueList_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_SepTypeValueList_try = lcl_1
            lcl_1 = (rbnf_named_lr_SepTypeValueList_try is not None)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_0 = rbnf_named_lr_SepTypeValueList_reduce
        else:
            lcl_0 = rbnf_named_lr_SepTypeValueList_try
        return lcl_0

    def rbnf_named_lr_step_TopLevelEntityList(rbnf_tmp_0, builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_TopLevelEntity(builtin_state, builtin_tokens)
        rbnf_named__check_1 = lcl_0
        lcl_0 = (rbnf_named__check_1 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_1 = rbnf_named__check_1
            lcl_1 = (rbnf_tmp_0, rbnf_tmp_1)
            lcl_1 = builtin_mk_ast('TopLevelEntityList', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_lr_loop_TopLevelEntityList(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_TopLevelEntityList_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_TopLevelEntityList(rbnf_named_lr_TopLevelEntityList_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_TopLevelEntityList_try = lcl_0
        lcl_0 = (rbnf_named_lr_TopLevelEntityList_try is not None)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            rbnf_named_lr_TopLevelEntityList_reduce = rbnf_named_lr_TopLevelEntityList_try
            lcl_1 = rbnf_named_lr_step_TopLevelEntityList(rbnf_named_lr_TopLevelEntityList_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_TopLevelEntityList_try = lcl_1
            lcl_1 = (rbnf_named_lr_TopLevelEntityList_try is not None)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_0 = rbnf_named_lr_TopLevelEntityList_reduce
        else:
            lcl_0 = rbnf_named_lr_TopLevelEntityList_try
        return lcl_0

    def rbnf_named_lr_step_Type(rbnf_tmp_0, builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 31):
                lcl_3 = rbnf_named_parse_AddrSpace(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = (rbnf_named__check_1 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_1 = rbnf_named__check_1
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 30):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = (rbnf_tmp_2 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        lcl_5 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                        lcl_5 = builtin_mk_ast('PtrType', lcl_5)
                        rbnf_tmp_1_ = lcl_5
                        lcl_5 = (rbnf_tmp_1_,)
                        lcl_5 = builtin_mk_ast('FirstClassType', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_5 = (rbnf_tmp_2_,)
                        lcl_5 = builtin_mk_ast('Type', lcl_5)
                        rbnf_tmp_3_ = lcl_5
                        lcl_4 = rbnf_tmp_3_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 30):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_1 = lcl_3
                lcl_3 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                lcl_3 = builtin_mk_ast('PtrType', lcl_3)
                rbnf_tmp_2_ = lcl_3
                lcl_3 = (rbnf_tmp_2_,)
                lcl_3 = builtin_mk_ast('FirstClassType', lcl_3)
                rbnf_tmp_3_ = lcl_3
                lcl_3 = (rbnf_tmp_3_,)
                lcl_3 = builtin_mk_ast('Type', lcl_3)
                rbnf_tmp_4_ = lcl_3
                lcl_1 = rbnf_tmp_4_
            elif (lcl_2 == 16):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_1 = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                if lcl_3:
                    lcl_5 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                    lcl_5 = lcl_5.idint
                    if (lcl_5 == 36):
                        lcl_6 = rbnf_named_parse_Params(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_6
                        lcl_6 = (rbnf_named__check_2 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 17):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_7
                            lcl_7 = (rbnf_tmp_3 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_8 = builtin_mk_ast('FuncType', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_1_,)
                                lcl_8 = builtin_mk_ast('Type', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 15):
                        lcl_6 = rbnf_named_parse_Params(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_6
                        lcl_6 = (rbnf_named__check_2 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 17):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_7
                            lcl_7 = (rbnf_tmp_3 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_8 = builtin_mk_ast('FuncType', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_1_,)
                                lcl_8 = builtin_mk_ast('Type', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 32):
                        lcl_6 = rbnf_named_parse_Params(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_6
                        lcl_6 = (rbnf_named__check_2 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 17):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_7
                            lcl_7 = (rbnf_tmp_3 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_8 = builtin_mk_ast('FuncType', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_1_,)
                                lcl_8 = builtin_mk_ast('Type', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 24):
                        lcl_6 = rbnf_named_parse_Params(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_6
                        lcl_6 = (rbnf_named__check_2 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 17):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_7
                            lcl_7 = (rbnf_tmp_3 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_8 = builtin_mk_ast('FuncType', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_1_,)
                                lcl_8 = builtin_mk_ast('Type', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 27):
                        lcl_6 = rbnf_named_parse_Params(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_6
                        lcl_6 = (rbnf_named__check_2 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 17):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_7
                            lcl_7 = (rbnf_tmp_3 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_8 = builtin_mk_ast('FuncType', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_1_,)
                                lcl_8 = builtin_mk_ast('Type', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 26):
                        lcl_6 = rbnf_named_parse_Params(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_6
                        lcl_6 = (rbnf_named__check_2 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 17):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_7
                            lcl_7 = (rbnf_tmp_3 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_8 = builtin_mk_ast('FuncType', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_1_,)
                                lcl_8 = builtin_mk_ast('Type', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 25):
                        lcl_6 = rbnf_named_parse_Params(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_6
                        lcl_6 = (rbnf_named__check_2 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 17):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_7
                            lcl_7 = (rbnf_tmp_3 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_8 = builtin_mk_ast('FuncType', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_1_,)
                                lcl_8 = builtin_mk_ast('Type', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 23):
                        lcl_6 = rbnf_named_parse_Params(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_6
                        lcl_6 = (rbnf_named__check_2 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 17):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_7
                            lcl_7 = (rbnf_tmp_3 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_8 = builtin_mk_ast('FuncType', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_1_,)
                                lcl_8 = builtin_mk_ast('Type', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 29):
                        lcl_6 = rbnf_named_parse_Params(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_6
                        lcl_6 = (rbnf_named__check_2 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 17):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_7
                            lcl_7 = (rbnf_tmp_3 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_8 = builtin_mk_ast('FuncType', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_1_,)
                                lcl_8 = builtin_mk_ast('Type', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 28):
                        lcl_6 = rbnf_named_parse_Params(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_6
                        lcl_6 = (rbnf_named__check_2 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 17):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_7
                            lcl_7 = (rbnf_tmp_3 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_8 = builtin_mk_ast('FuncType', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_1_,)
                                lcl_8 = builtin_mk_ast('Type', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 33):
                        lcl_6 = rbnf_named_parse_Params(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_6
                        lcl_6 = (rbnf_named__check_2 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 17):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_7
                            lcl_7 = (rbnf_tmp_3 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_8 = builtin_mk_ast('FuncType', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_1_,)
                                lcl_8 = builtin_mk_ast('Type', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 18):
                        lcl_6 = rbnf_named_parse_Params(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_6
                        lcl_6 = (rbnf_named__check_2 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 17):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_7
                            lcl_7 = (rbnf_tmp_3 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_8 = builtin_mk_ast('FuncType', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_1_,)
                                lcl_8 = builtin_mk_ast('Type', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 17):
                        lcl_6 = ()
                        rbnf_tmp_1_ = lcl_6
                        _rbnf_old_offset = builtin_tokens.offset
                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                        lcl_6 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_6
                        lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2)
                        lcl_6 = builtin_mk_ast('FuncType', lcl_6)
                        rbnf_tmp_2_ = lcl_6
                        lcl_6 = (rbnf_tmp_2_,)
                        lcl_6 = builtin_mk_ast('Type', lcl_6)
                        rbnf_tmp_3_ = lcl_6
                        lcl_4 = rbnf_tmp_3_
                    elif (lcl_5 == 4):
                        lcl_6 = rbnf_named_parse_Params(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_6
                        lcl_6 = (rbnf_named__check_2 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 17):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_7
                            lcl_7 = (rbnf_tmp_3 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_8 = builtin_mk_ast('FuncType', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_1_,)
                                lcl_8 = builtin_mk_ast('Type', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    else:
                        lcl_4 = None
                    lcl_3 = lcl_4
                else:
                    lcl_3 = None
                lcl_1 = lcl_3
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_lr_loop_Type(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_Type_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_Type(rbnf_named_lr_Type_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_Type_try = lcl_0
        lcl_0 = (rbnf_named_lr_Type_try is not None)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            rbnf_named_lr_Type_reduce = rbnf_named_lr_Type_try
            lcl_1 = rbnf_named_lr_step_Type(rbnf_named_lr_Type_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_Type_try = lcl_1
            lcl_1 = (rbnf_named_lr_Type_try is not None)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_0 = rbnf_named_lr_Type_reduce
        else:
            lcl_0 = rbnf_named_lr_Type_try
        return lcl_0

    def rbnf_named_lr_step_TypeConstList(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 19):
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
            lcl_1 = rbnf_named_parse_TypeConstant(builtin_state, builtin_tokens)
            rbnf_named__check_2 = lcl_1
            lcl_1 = (rbnf_named__check_2 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_2 = rbnf_named__check_2
                lcl_2 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                lcl_2 = builtin_mk_ast('TypeConstList', lcl_2)
                rbnf_tmp_1_ = lcl_2
                lcl_1 = rbnf_tmp_1_
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_TypeConstList(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_TypeConstList_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_TypeConstList(rbnf_named_lr_TypeConstList_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_TypeConstList_try = lcl_0
        lcl_0 = (rbnf_named_lr_TypeConstList_try is not None)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            rbnf_named_lr_TypeConstList_reduce = rbnf_named_lr_TypeConstList_try
            lcl_1 = rbnf_named_lr_step_TypeConstList(rbnf_named_lr_TypeConstList_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_TypeConstList_try = lcl_1
            lcl_1 = (rbnf_named_lr_TypeConstList_try is not None)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_0 = rbnf_named_lr_TypeConstList_reduce
        else:
            lcl_0 = rbnf_named_lr_TypeConstList_try
        return lcl_0

    def rbnf_named_lr_step_TypeList(rbnf_tmp_0, builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 19):
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
            lcl_1 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
            rbnf_named__check_2 = lcl_1
            lcl_1 = (rbnf_named__check_2 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_2 = rbnf_named__check_2
                lcl_2 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                lcl_2 = builtin_mk_ast('TypeList', lcl_2)
                rbnf_tmp_1_ = lcl_2
                lcl_1 = rbnf_tmp_1_
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_lr_loop_TypeList(rbnf_tmp_0, builtin_state, builtin_tokens):
        rbnf_named_lr_TypeList_reduce = rbnf_tmp_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = rbnf_named_lr_step_TypeList(rbnf_named_lr_TypeList_reduce, builtin_state, builtin_tokens)
        rbnf_named_lr_TypeList_try = lcl_0
        lcl_0 = (rbnf_named_lr_TypeList_try is not None)
        while lcl_0:
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            rbnf_named_lr_TypeList_reduce = rbnf_named_lr_TypeList_try
            lcl_1 = rbnf_named_lr_step_TypeList(rbnf_named_lr_TypeList_reduce, builtin_state, builtin_tokens)
            rbnf_named_lr_TypeList_try = lcl_1
            lcl_1 = (rbnf_named_lr_TypeList_try is not None)
            lcl_0 = lcl_1
        lcl_0 = builtin_tokens.offset
        lcl_0 = (lcl_0 == rbnf_named__off_0)
        if lcl_0:
            lcl_0 = rbnf_named_lr_TypeList_reduce
        else:
            lcl_0 = rbnf_named_lr_TypeList_try
        return lcl_0

    def rbnf_named_parse_AddrSpace(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 31):
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
            try:
                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                if (_rbnf_cur_token.idint is 16):
                    builtin_tokens.offset += 1
                else:
                    _rbnf_cur_token = None
            except IndexError:
                _rbnf_cur_token = None
            lcl_1 = _rbnf_cur_token
            rbnf_tmp_1 = lcl_1
            lcl_1 = (rbnf_tmp_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                lcl_2 = rbnf_named_parse_IntLit(builtin_state, builtin_tokens)
                rbnf_named__check_2 = lcl_2
                lcl_2 = (rbnf_named__check_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    rbnf_tmp_2 = rbnf_named__check_2
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 17):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_3 = _rbnf_cur_token
                    rbnf_tmp_3 = lcl_3
                    lcl_3 = (rbnf_tmp_3 is None)
                    if lcl_3:
                        lcl_3 = None
                    else:
                        lcl_4 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                        lcl_4 = builtin_mk_ast('AddrSpace', lcl_4)
                        rbnf_tmp_1_ = lcl_4
                        lcl_3 = rbnf_tmp_1_
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_Alignment(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 20):
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
            lcl_1 = rbnf_named_parse_IntLit(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                lcl_2 = (rbnf_tmp_0, rbnf_tmp_1)
                lcl_2 = builtin_mk_ast('Alignment', lcl_2)
                rbnf_tmp_1_ = lcl_2
                lcl_1 = rbnf_tmp_1_
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_AllocaInst(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 82):
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
            lcl_1 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                lcl_2 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_2
                lcl_2 = (len(builtin_tokens.array) > (builtin_tokens.offset + 1))
                if lcl_2:
                    lcl_4 = builtin_tokens.array[(builtin_tokens.offset + 1)]
                    lcl_4 = lcl_4.idint
                    if (lcl_4 == 36):
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = (rbnf_tmp_2, rbnf_tmp_3)
                                rbnf_tmp_1_ = lcl_7
                                lcl_7 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_7
                                lcl_7 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_7:
                                    lcl_9 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_9 = lcl_9.idint
                                    if (lcl_9 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_10 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_10
                                        lcl_10 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_10
                                        lcl_10 = (rbnf_named__check_5 is None)
                                        if lcl_10:
                                            lcl_10 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_11 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_11
                                            lcl_11 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                            lcl_11 = builtin_mk_ast('AllocaInst', lcl_11)
                                            rbnf_tmp_3_ = lcl_11
                                            lcl_10 = rbnf_tmp_3_
                                        lcl_8 = lcl_10
                                    else:
                                        lcl_10 = ()
                                        rbnf_tmp_2_ = lcl_10
                                        lcl_10 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                        lcl_10 = builtin_mk_ast('AllocaInst', lcl_10)
                                        rbnf_tmp_3_ = lcl_10
                                        lcl_8 = rbnf_tmp_3_
                                    lcl_7 = lcl_8
                                else:
                                    lcl_7 = None
                                lcl_6 = lcl_7
                            lcl_5 = lcl_6
                        lcl_3 = lcl_5
                    elif (lcl_4 == 15):
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_10 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_10
                        lcl_10 = (rbnf_tmp_2 is None)
                        if lcl_10:
                            lcl_10 = None
                        else:
                            lcl_11 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_11
                            lcl_11 = (rbnf_named__check_3 is None)
                            if lcl_11:
                                lcl_11 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = (rbnf_tmp_2, rbnf_tmp_3)
                                rbnf_tmp_1_ = lcl_5
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('AllocaInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('AllocaInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_11 = lcl_5
                            lcl_10 = lcl_11
                        lcl_3 = lcl_10
                    elif (lcl_4 == 32):
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_10 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_10
                        lcl_10 = (rbnf_tmp_2 is None)
                        if lcl_10:
                            lcl_10 = None
                        else:
                            lcl_11 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_11
                            lcl_11 = (rbnf_named__check_3 is None)
                            if lcl_11:
                                lcl_11 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = (rbnf_tmp_2, rbnf_tmp_3)
                                rbnf_tmp_1_ = lcl_5
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('AllocaInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('AllocaInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_11 = lcl_5
                            lcl_10 = lcl_11
                        lcl_3 = lcl_10
                    elif (lcl_4 == 24):
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_10 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_10
                        lcl_10 = (rbnf_tmp_2 is None)
                        if lcl_10:
                            lcl_10 = None
                        else:
                            lcl_11 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_11
                            lcl_11 = (rbnf_named__check_3 is None)
                            if lcl_11:
                                lcl_11 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = (rbnf_tmp_2, rbnf_tmp_3)
                                rbnf_tmp_1_ = lcl_5
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('AllocaInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('AllocaInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_11 = lcl_5
                            lcl_10 = lcl_11
                        lcl_3 = lcl_10
                    elif (lcl_4 == 27):
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_10 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_10
                        lcl_10 = (rbnf_tmp_2 is None)
                        if lcl_10:
                            lcl_10 = None
                        else:
                            lcl_11 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_11
                            lcl_11 = (rbnf_named__check_3 is None)
                            if lcl_11:
                                lcl_11 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = (rbnf_tmp_2, rbnf_tmp_3)
                                rbnf_tmp_1_ = lcl_5
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('AllocaInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('AllocaInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_11 = lcl_5
                            lcl_10 = lcl_11
                        lcl_3 = lcl_10
                    elif (lcl_4 == 26):
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_10 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_10
                        lcl_10 = (rbnf_tmp_2 is None)
                        if lcl_10:
                            lcl_10 = None
                        else:
                            lcl_11 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_11
                            lcl_11 = (rbnf_named__check_3 is None)
                            if lcl_11:
                                lcl_11 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = (rbnf_tmp_2, rbnf_tmp_3)
                                rbnf_tmp_1_ = lcl_5
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('AllocaInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('AllocaInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_11 = lcl_5
                            lcl_10 = lcl_11
                        lcl_3 = lcl_10
                    elif (lcl_4 == 25):
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_10 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_10
                        lcl_10 = (rbnf_tmp_2 is None)
                        if lcl_10:
                            lcl_10 = None
                        else:
                            lcl_11 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_11
                            lcl_11 = (rbnf_named__check_3 is None)
                            if lcl_11:
                                lcl_11 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = (rbnf_tmp_2, rbnf_tmp_3)
                                rbnf_tmp_1_ = lcl_5
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('AllocaInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('AllocaInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_11 = lcl_5
                            lcl_10 = lcl_11
                        lcl_3 = lcl_10
                    elif (lcl_4 == 23):
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_10 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_10
                        lcl_10 = (rbnf_tmp_2 is None)
                        if lcl_10:
                            lcl_10 = None
                        else:
                            lcl_11 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_11
                            lcl_11 = (rbnf_named__check_3 is None)
                            if lcl_11:
                                lcl_11 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = (rbnf_tmp_2, rbnf_tmp_3)
                                rbnf_tmp_1_ = lcl_5
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('AllocaInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('AllocaInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_11 = lcl_5
                            lcl_10 = lcl_11
                        lcl_3 = lcl_10
                    elif (lcl_4 == 29):
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_10 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_10
                        lcl_10 = (rbnf_tmp_2 is None)
                        if lcl_10:
                            lcl_10 = None
                        else:
                            lcl_11 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_11
                            lcl_11 = (rbnf_named__check_3 is None)
                            if lcl_11:
                                lcl_11 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = (rbnf_tmp_2, rbnf_tmp_3)
                                rbnf_tmp_1_ = lcl_5
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('AllocaInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('AllocaInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_11 = lcl_5
                            lcl_10 = lcl_11
                        lcl_3 = lcl_10
                    elif (lcl_4 == 28):
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_10 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_10
                        lcl_10 = (rbnf_tmp_2 is None)
                        if lcl_10:
                            lcl_10 = None
                        else:
                            lcl_11 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_11
                            lcl_11 = (rbnf_named__check_3 is None)
                            if lcl_11:
                                lcl_11 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = (rbnf_tmp_2, rbnf_tmp_3)
                                rbnf_tmp_1_ = lcl_5
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('AllocaInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('AllocaInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_11 = lcl_5
                            lcl_10 = lcl_11
                        lcl_3 = lcl_10
                    elif (lcl_4 == 20):
                        lcl_10 = ()
                        rbnf_tmp_1_ = lcl_10
                        lcl_10 = builtin_tokens.offset
                        rbnf_named__off_2 = lcl_10
                        lcl_10 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_10:
                            lcl_5 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_5 = lcl_5.idint
                            if (lcl_5 == 19):
                                _rbnf_old_offset = builtin_tokens.offset
                                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                builtin_tokens.offset = (_rbnf_old_offset + 1)
                                lcl_6 = _rbnf_cur_token
                                rbnf_tmp_2 = lcl_6
                                lcl_6 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                rbnf_named__check_3 = lcl_6
                                lcl_6 = (rbnf_named__check_3 is None)
                                if lcl_6:
                                    lcl_6 = None
                                else:
                                    rbnf_tmp_3 = rbnf_named__check_3
                                    lcl_7 = (rbnf_tmp_2, rbnf_tmp_3)
                                    rbnf_tmp_2_ = lcl_7
                                    lcl_7 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                    lcl_7 = builtin_mk_ast('AllocaInst', lcl_7)
                                    rbnf_tmp_3_ = lcl_7
                                    lcl_6 = rbnf_tmp_3_
                                lcl_11 = lcl_6
                            else:
                                lcl_6 = ()
                                rbnf_tmp_2_ = lcl_6
                                lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                lcl_6 = builtin_mk_ast('AllocaInst', lcl_6)
                                rbnf_tmp_3_ = lcl_6
                                lcl_11 = rbnf_tmp_3_
                            lcl_10 = lcl_11
                        else:
                            lcl_10 = None
                        lcl_3 = lcl_10
                    elif (lcl_4 == 33):
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_10 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_10
                        lcl_10 = (rbnf_tmp_2 is None)
                        if lcl_10:
                            lcl_10 = None
                        else:
                            lcl_11 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_11
                            lcl_11 = (rbnf_named__check_3 is None)
                            if lcl_11:
                                lcl_11 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = (rbnf_tmp_2, rbnf_tmp_3)
                                rbnf_tmp_1_ = lcl_5
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('AllocaInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('AllocaInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_11 = lcl_5
                            lcl_10 = lcl_11
                        lcl_3 = lcl_10
                    elif (lcl_4 == 4):
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_10 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_10
                        lcl_10 = (rbnf_tmp_2 is None)
                        if lcl_10:
                            lcl_10 = None
                        else:
                            lcl_11 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_11
                            lcl_11 = (rbnf_named__check_3 is None)
                            if lcl_11:
                                lcl_11 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = (rbnf_tmp_2, rbnf_tmp_3)
                                rbnf_tmp_1_ = lcl_5
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('AllocaInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('AllocaInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_11 = lcl_5
                            lcl_10 = lcl_11
                        lcl_3 = lcl_10
                    else:
                        lcl_10 = ()
                        rbnf_tmp_1_ = lcl_10
                        lcl_10 = builtin_tokens.offset
                        rbnf_named__off_2 = lcl_10
                        lcl_10 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_10:
                            lcl_5 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_5 = lcl_5.idint
                            if (lcl_5 == 19):
                                _rbnf_old_offset = builtin_tokens.offset
                                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                builtin_tokens.offset = (_rbnf_old_offset + 1)
                                lcl_6 = _rbnf_cur_token
                                rbnf_tmp_2 = lcl_6
                                lcl_6 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                rbnf_named__check_3 = lcl_6
                                lcl_6 = (rbnf_named__check_3 is None)
                                if lcl_6:
                                    lcl_6 = None
                                else:
                                    rbnf_tmp_3 = rbnf_named__check_3
                                    lcl_7 = (rbnf_tmp_2, rbnf_tmp_3)
                                    rbnf_tmp_2_ = lcl_7
                                    lcl_7 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                    lcl_7 = builtin_mk_ast('AllocaInst', lcl_7)
                                    rbnf_tmp_3_ = lcl_7
                                    lcl_6 = rbnf_tmp_3_
                                lcl_11 = lcl_6
                            else:
                                lcl_6 = ()
                                rbnf_tmp_2_ = lcl_6
                                lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2_)
                                lcl_6 = builtin_mk_ast('AllocaInst', lcl_6)
                                rbnf_tmp_3_ = lcl_6
                                lcl_11 = rbnf_tmp_3_
                            lcl_10 = lcl_11
                        else:
                            lcl_10 = None
                        lcl_3 = lcl_10
                    lcl_2 = lcl_3
                else:
                    lcl_2 = None
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_ArrayConst(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 33):
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
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_1 = lcl_1
            lcl_1 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
            if lcl_1:
                lcl_3 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                lcl_3 = lcl_3.idint
                if (lcl_3 == 36):
                    lcl_4 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 35):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ArrayConst', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 15):
                    lcl_4 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 35):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ArrayConst', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 32):
                    lcl_4 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 35):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ArrayConst', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 24):
                    lcl_4 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 35):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ArrayConst', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 27):
                    lcl_4 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 35):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ArrayConst', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 26):
                    lcl_4 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 35):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ArrayConst', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 25):
                    lcl_4 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 35):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ArrayConst', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 23):
                    lcl_4 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 35):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ArrayConst', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 29):
                    lcl_4 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 35):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ArrayConst', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 28):
                    lcl_4 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 35):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ArrayConst', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 35):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    _rbnf_old_offset = builtin_tokens.offset
                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                    lcl_4 = builtin_mk_ast('ArrayConst', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_2 = rbnf_tmp_2_
                elif (lcl_3 == 33):
                    lcl_4 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 35):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ArrayConst', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 4):
                    lcl_4 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 35):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ArrayConst', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                else:
                    lcl_2 = None
                lcl_1 = lcl_2
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_ArrayType(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 33):
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
            lcl_1 = rbnf_named_parse_IntLit(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 34):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_2 = _rbnf_cur_token
                rbnf_tmp_2 = lcl_2
                lcl_2 = (rbnf_tmp_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    lcl_3 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_3 = lcl_3
                    lcl_3 = (rbnf_named__check_3 is None)
                    if lcl_3:
                        lcl_3 = None
                    else:
                        rbnf_tmp_3 = rbnf_named__check_3
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 35):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_4 = _rbnf_cur_token
                        rbnf_tmp_4 = lcl_4
                        lcl_4 = (rbnf_tmp_4 is None)
                        if lcl_4:
                            lcl_4 = None
                        else:
                            lcl_5 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                            lcl_5 = builtin_mk_ast('ArrayType', lcl_5)
                            rbnf_tmp_1_ = lcl_5
                            lcl_4 = rbnf_tmp_1_
                        lcl_3 = lcl_4
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_BasicBlock(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 67):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 59):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 57):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 81):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = ()
                rbnf_tmp_2_ = lcl_3
                lcl_3 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_2_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('BasicBlock', lcl_4)
                    rbnf_tmp_3_ = lcl_4
                    lcl_3 = rbnf_tmp_3_
                lcl_1 = lcl_3
            elif (lcl_2 == 53):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 77):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 60):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 62):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 87):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 58):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 79):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = ()
                rbnf_tmp_2_ = lcl_3
                lcl_3 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_2_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('BasicBlock', lcl_4)
                    rbnf_tmp_3_ = lcl_4
                    lcl_3 = rbnf_tmp_3_
                lcl_1 = lcl_3
            elif (lcl_2 == 86):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 66):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 55):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 63):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 83):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 69):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 84):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 70):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 54):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 61):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 56):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 85):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 52):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 68):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 80):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = ()
                rbnf_tmp_2_ = lcl_3
                lcl_3 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_2_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('BasicBlock', lcl_4)
                    rbnf_tmp_3_ = lcl_4
                    lcl_3 = rbnf_tmp_3_
                lcl_1 = lcl_3
            elif (lcl_2 == 73):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 64):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 65):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 82):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 51):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 4):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('BasicBlock', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 3):
                lcl_3 = rbnf_named_parse_name(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_4:
                        lcl_6 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_6 = lcl_6.idint
                        if (lcl_6 == 67):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 59):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 57):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 81):
                            lcl_7 = ()
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                                lcl_8 = builtin_mk_ast('BasicBlock', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_5 = lcl_7
                        elif (lcl_6 == 53):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 77):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 60):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 62):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 87):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 58):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 79):
                            lcl_7 = ()
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                                lcl_8 = builtin_mk_ast('BasicBlock', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_5 = lcl_7
                        elif (lcl_6 == 86):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 66):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 55):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 63):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 83):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 69):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 84):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 70):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 54):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 61):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 56):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 85):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 52):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 68):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 80):
                            lcl_7 = ()
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                                lcl_8 = builtin_mk_ast('BasicBlock', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_5 = lcl_7
                        elif (lcl_6 == 73):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 64):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 65):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 82):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 51):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 4):
                            lcl_7 = rbnf_named_parse_InstructionList(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Terminator(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('BasicBlock', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    else:
                        lcl_4 = None
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens):
        lcl_0 = ()
        rbnf_tmp_1_ = lcl_0
        lcl_0 = rbnf_named_parse_BasicBlock(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_1_, rbnf_tmp_0)
            lcl_1 = builtin_mk_ast('BasicBlockList', lcl_1)
            rbnf_tmp_2_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_BasicBlockList(rbnf_tmp_2_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_BinInst(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_BinOp(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                lcl_2 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                rbnf_named__check_2 = lcl_2
                lcl_2 = (rbnf_named__check_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    rbnf_tmp_2 = rbnf_named__check_2
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 19):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_3 = _rbnf_cur_token
                    rbnf_tmp_3 = lcl_3
                    lcl_3 = (rbnf_tmp_3 is None)
                    if lcl_3:
                        lcl_3 = None
                    else:
                        lcl_4 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_4 = lcl_4
                        lcl_4 = (rbnf_named__check_4 is None)
                        if lcl_4:
                            lcl_4 = None
                        else:
                            rbnf_tmp_4 = rbnf_named__check_4
                            lcl_5 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                            lcl_5 = builtin_mk_ast('BinInst', lcl_5)
                            rbnf_tmp_1_ = lcl_5
                            lcl_4 = rbnf_tmp_1_
                        lcl_3 = lcl_4
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_BinOp(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 67):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BinOp', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 59):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BinOp', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 57):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BinOp', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 53):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BinOp', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 60):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BinOp', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 62):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BinOp', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 58):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BinOp', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 66):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BinOp', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 55):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BinOp', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 63):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BinOp', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 54):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BinOp', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 61):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BinOp', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 56):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BinOp', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 52):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BinOp', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 64):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BinOp', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 65):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BinOp', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 51):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BinOp', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 50):
                lcl_3 = rbnf_named_parse_OverflowFlags(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('BinOpQualifier', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 49):
                lcl_3 = rbnf_named_parse_OverflowFlags(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('BinOpQualifier', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 48):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BinOpQualifier', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_BitCastExpr(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 73):
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
            try:
                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                if (_rbnf_cur_token.idint is 16):
                    builtin_tokens.offset += 1
                else:
                    _rbnf_cur_token = None
            except IndexError:
                _rbnf_cur_token = None
            lcl_1 = _rbnf_cur_token
            rbnf_tmp_1 = lcl_1
            lcl_1 = (rbnf_tmp_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                lcl_2 = rbnf_named_parse_TypeConstant(builtin_state, builtin_tokens)
                rbnf_named__check_2 = lcl_2
                lcl_2 = (rbnf_named__check_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    rbnf_tmp_2 = rbnf_named__check_2
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 74):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_3 = _rbnf_cur_token
                    rbnf_tmp_3 = lcl_3
                    lcl_3 = (rbnf_tmp_3 is None)
                    if lcl_3:
                        lcl_3 = None
                    else:
                        lcl_4 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                        rbnf_named__check_4 = lcl_4
                        lcl_4 = (rbnf_named__check_4 is None)
                        if lcl_4:
                            lcl_4 = None
                        else:
                            rbnf_tmp_4 = rbnf_named__check_4
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 17):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_5 = _rbnf_cur_token
                            rbnf_tmp_5 = lcl_5
                            lcl_5 = (rbnf_tmp_5 is None)
                            if lcl_5:
                                lcl_5 = None
                            else:
                                lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5)
                                lcl_6 = builtin_mk_ast('BitCastExpr', lcl_6)
                                rbnf_tmp_1_ = lcl_6
                                lcl_5 = rbnf_tmp_1_
                            lcl_4 = lcl_5
                        lcl_3 = lcl_4
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_BitcastInst(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 73):
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
            lcl_1 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 74):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_2 = _rbnf_cur_token
                rbnf_tmp_2 = lcl_2
                lcl_2 = (rbnf_tmp_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    lcl_3 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_3 = lcl_3
                    lcl_3 = (rbnf_named__check_3 is None)
                    if lcl_3:
                        lcl_3 = None
                    else:
                        rbnf_tmp_3 = rbnf_named__check_3
                        lcl_4 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                        lcl_4 = builtin_mk_ast('BitcastInst', lcl_4)
                        rbnf_tmp_1_ = lcl_4
                        lcl_3 = rbnf_tmp_1_
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_BlockAddressConst(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 47):
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
            try:
                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                if (_rbnf_cur_token.idint is 16):
                    builtin_tokens.offset += 1
                else:
                    _rbnf_cur_token = None
            except IndexError:
                _rbnf_cur_token = None
            lcl_1 = _rbnf_cur_token
            rbnf_tmp_1 = lcl_1
            lcl_1 = (rbnf_tmp_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                lcl_2 = rbnf_named_parse_GlobalName(builtin_state, builtin_tokens)
                rbnf_named__check_2 = lcl_2
                lcl_2 = (rbnf_named__check_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    rbnf_tmp_2 = rbnf_named__check_2
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 19):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_3 = _rbnf_cur_token
                    rbnf_tmp_3 = lcl_3
                    lcl_3 = (rbnf_tmp_3 is None)
                    if lcl_3:
                        lcl_3 = None
                    else:
                        lcl_4 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                        rbnf_named__check_4 = lcl_4
                        lcl_4 = (rbnf_named__check_4 is None)
                        if lcl_4:
                            lcl_4 = None
                        else:
                            rbnf_tmp_4 = rbnf_named__check_4
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 17):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_5 = _rbnf_cur_token
                            rbnf_tmp_5 = lcl_5
                            lcl_5 = (rbnf_tmp_5 is None)
                            if lcl_5:
                                lcl_5 = None
                            else:
                                lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5)
                                lcl_6 = builtin_mk_ast('BlockAddressConst', lcl_6)
                                rbnf_tmp_1_ = lcl_6
                                lcl_5 = rbnf_tmp_1_
                            lcl_4 = lcl_5
                        lcl_3 = lcl_4
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_BoolConst(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_BoolLit(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('BoolConst', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_BoolLit(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 38):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BoolLit', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 39):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('BoolLit', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_BrTerm(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 80):
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
            lcl_1 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                lcl_2 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_2
                lcl_2 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                if lcl_2:
                    lcl_4 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                    lcl_4 = lcl_4.idint
                    if (lcl_4 == 19):
                        _rbnf_old_offset = builtin_tokens.offset
                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = rbnf_named_parse_LabelType(builtin_state, builtin_tokens)
                        rbnf_named__check_3 = lcl_5
                        lcl_5 = (rbnf_named__check_3 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_3 = rbnf_named__check_3
                            lcl_6 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                            rbnf_named__check_4 = lcl_6
                            lcl_6 = (rbnf_named__check_4 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_4 = rbnf_named__check_4
                                try:
                                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                    if (_rbnf_cur_token.idint is 19):
                                        builtin_tokens.offset += 1
                                    else:
                                        _rbnf_cur_token = None
                                except IndexError:
                                    _rbnf_cur_token = None
                                lcl_7 = _rbnf_cur_token
                                rbnf_tmp_5 = lcl_7
                                lcl_7 = (rbnf_tmp_5 is None)
                                if lcl_7:
                                    lcl_7 = None
                                else:
                                    lcl_8 = rbnf_named_parse_LabelType(builtin_state, builtin_tokens)
                                    rbnf_named__check_6 = lcl_8
                                    lcl_8 = (rbnf_named__check_6 is None)
                                    if lcl_8:
                                        lcl_8 = None
                                    else:
                                        rbnf_tmp_6 = rbnf_named__check_6
                                        lcl_9 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                        rbnf_named__check_7 = lcl_9
                                        lcl_9 = (rbnf_named__check_7 is None)
                                        if lcl_9:
                                            lcl_9 = None
                                        else:
                                            rbnf_tmp_7 = rbnf_named__check_7
                                            lcl_10 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_6, rbnf_tmp_7)
                                            lcl_10 = builtin_mk_ast('BrTerm', lcl_10)
                                            rbnf_tmp_1_ = lcl_10
                                            lcl_9 = rbnf_tmp_1_
                                        lcl_8 = lcl_9
                                    lcl_7 = lcl_8
                                lcl_6 = lcl_7
                            lcl_5 = lcl_6
                        lcl_3 = lcl_5
                    elif (lcl_4 == 4):
                        lcl_10 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_10
                        lcl_10 = (rbnf_named__check_2 is None)
                        if lcl_10:
                            lcl_10 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_5 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_5 = builtin_mk_ast('BrTerm', lcl_5)
                            rbnf_tmp_1_ = lcl_5
                            lcl_10 = rbnf_tmp_1_
                        lcl_3 = lcl_10
                    else:
                        lcl_3 = None
                    lcl_2 = lcl_3
                else:
                    lcl_2 = None
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_Case(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_IntType(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = rbnf_named_parse_IntConst(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 19):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_2 = _rbnf_cur_token
                rbnf_tmp_2 = lcl_2
                lcl_2 = (rbnf_tmp_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    lcl_3 = rbnf_named_parse_LabelType(builtin_state, builtin_tokens)
                    rbnf_named__check_3 = lcl_3
                    lcl_3 = (rbnf_named__check_3 is None)
                    if lcl_3:
                        lcl_3 = None
                    else:
                        rbnf_tmp_3 = rbnf_named__check_3
                        lcl_4 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                        rbnf_named__check_4 = lcl_4
                        lcl_4 = (rbnf_named__check_4 is None)
                        if lcl_4:
                            lcl_4 = None
                        else:
                            rbnf_tmp_4 = rbnf_named__check_4
                            lcl_5 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                            lcl_5 = builtin_mk_ast('Case', lcl_5)
                            rbnf_tmp_1_ = lcl_5
                            lcl_4 = rbnf_tmp_1_
                        lcl_3 = lcl_4
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_CaseList(builtin_state, builtin_tokens):
        lcl_0 = ()
        rbnf_tmp_1_ = lcl_0
        lcl_0 = rbnf_named_parse_Case(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_1_, rbnf_tmp_0)
            lcl_1 = builtin_mk_ast('CaseList', lcl_1)
            rbnf_tmp_2_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_CaseList(rbnf_tmp_2_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_Cases(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_CaseList(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('Cases', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_CharArrayConst(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 44):
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
            lcl_1 = rbnf_named_parse_StrLit(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                lcl_2 = (rbnf_tmp_0, rbnf_tmp_1)
                lcl_2 = builtin_mk_ast('CharArrayConst', lcl_2)
                rbnf_tmp_1_ = lcl_2
                lcl_1 = rbnf_tmp_1_
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_CmpInst(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 84):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_IPred(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = (rbnf_named__check_1 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_1 = rbnf_named__check_1
                    lcl_4 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_2 = lcl_4
                    lcl_4 = (rbnf_named__check_2 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_2 = rbnf_named__check_2
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_3 = lcl_5
                        lcl_5 = (rbnf_named__check_3 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_3 = rbnf_named__check_3
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 19):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_6 = _rbnf_cur_token
                            rbnf_tmp_4 = lcl_6
                            lcl_6 = (rbnf_tmp_4 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                lcl_7 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                                rbnf_named__check_5 = lcl_7
                                lcl_7 = (rbnf_named__check_5 is None)
                                if lcl_7:
                                    lcl_7 = None
                                else:
                                    rbnf_tmp_5 = rbnf_named__check_5
                                    lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5)
                                    lcl_8 = builtin_mk_ast('CmpInst', lcl_8)
                                    rbnf_tmp_1_ = lcl_8
                                    lcl_7 = rbnf_tmp_1_
                                lcl_6 = lcl_7
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 85):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_FPred(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = (rbnf_named__check_1 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_1 = rbnf_named__check_1
                    lcl_4 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_2 = lcl_4
                    lcl_4 = (rbnf_named__check_2 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_2 = rbnf_named__check_2
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_3 = lcl_5
                        lcl_5 = (rbnf_named__check_3 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_3 = rbnf_named__check_3
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 19):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_6 = _rbnf_cur_token
                            rbnf_tmp_4 = lcl_6
                            lcl_6 = (rbnf_tmp_4 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                lcl_7 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                                rbnf_named__check_5 = lcl_7
                                lcl_7 = (rbnf_named__check_5 is None)
                                if lcl_7:
                                    lcl_7 = None
                                else:
                                    rbnf_tmp_5 = rbnf_named__check_5
                                    lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5)
                                    lcl_8 = builtin_mk_ast('CmpInst', lcl_8)
                                    rbnf_tmp_1_ = lcl_8
                                    lcl_7 = rbnf_tmp_1_
                                lcl_6 = lcl_7
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_Constant(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 36):
                lcl_3 = rbnf_named_parse_StructConst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 45):
                lcl_3 = rbnf_named_parse_ZeroInitializerConst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 67):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 59):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 46):
                lcl_3 = rbnf_named_parse_UndefConst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 57):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 38):
                lcl_3 = rbnf_named_parse_BoolConst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 53):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 60):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 62):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 58):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 66):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 40):
                lcl_3 = rbnf_named_parse_NullConst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 41):
                lcl_3 = rbnf_named_parse_NoneConst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 55):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 63):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 69):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 70):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 54):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 61):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 56):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 39):
                lcl_3 = rbnf_named_parse_BoolConst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 52):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 68):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 44):
                lcl_3 = rbnf_named_parse_CharArrayConst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 47):
                lcl_3 = rbnf_named_parse_BlockAddressConst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 73):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 64):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 65):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 51):
                lcl_3 = rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 33):
                lcl_3 = rbnf_named_parse_ArrayConst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 42):
                lcl_3 = rbnf_named_parse_StructConst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 2):
                lcl_3 = rbnf_named_parse_IntConst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 5):
                lcl_3 = rbnf_named_parse_GlobalName(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 10):
                lcl_3 = rbnf_named_parse_FloatConst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Constant', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_ConstantExpr(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 67):
                lcl_3 = rbnf_named_parse_BinOp(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_ConstantOperand2(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ConstantExpr', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 59):
                lcl_3 = rbnf_named_parse_BinOp(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_ConstantOperand2(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ConstantExpr', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 57):
                lcl_3 = rbnf_named_parse_BinOp(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_ConstantOperand2(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ConstantExpr', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 53):
                lcl_3 = rbnf_named_parse_BinOp(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_ConstantOperand2(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ConstantExpr', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 60):
                lcl_3 = rbnf_named_parse_BinOp(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_ConstantOperand2(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ConstantExpr', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 62):
                lcl_3 = rbnf_named_parse_BinOp(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_ConstantOperand2(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ConstantExpr', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 58):
                lcl_3 = rbnf_named_parse_BinOp(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_ConstantOperand2(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ConstantExpr', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 66):
                lcl_3 = rbnf_named_parse_BinOp(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_ConstantOperand2(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ConstantExpr', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 55):
                lcl_3 = rbnf_named_parse_BinOp(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_ConstantOperand2(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ConstantExpr', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 63):
                lcl_3 = rbnf_named_parse_BinOp(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_ConstantOperand2(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ConstantExpr', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 69):
                lcl_3 = rbnf_named_parse_InsValExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ConstantExpr', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 70):
                lcl_3 = rbnf_named_parse_GEPExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ConstantExpr', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 54):
                lcl_3 = rbnf_named_parse_BinOp(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_ConstantOperand2(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ConstantExpr', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 61):
                lcl_3 = rbnf_named_parse_BinOp(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_ConstantOperand2(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ConstantExpr', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 56):
                lcl_3 = rbnf_named_parse_BinOp(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_ConstantOperand2(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ConstantExpr', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 52):
                lcl_3 = rbnf_named_parse_BinOp(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_ConstantOperand2(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ConstantExpr', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 68):
                lcl_3 = rbnf_named_parse_ExtValExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ConstantExpr', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 73):
                lcl_3 = rbnf_named_parse_BitCastExpr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ConstantExpr', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 64):
                lcl_3 = rbnf_named_parse_BinOp(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_ConstantOperand2(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ConstantExpr', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 65):
                lcl_3 = rbnf_named_parse_BinOp(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_ConstantOperand2(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ConstantExpr', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 51):
                lcl_3 = rbnf_named_parse_BinOp(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_BinOpQualifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_ConstantOperand2(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('ConstantExpr', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_ConstantOperand2(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 16):
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
            lcl_1 = rbnf_named_parse_TypeConstant(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 19):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_2 = _rbnf_cur_token
                rbnf_tmp_2 = lcl_2
                lcl_2 = (rbnf_tmp_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    lcl_3 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_3 = lcl_3
                    lcl_3 = (rbnf_named__check_3 is None)
                    if lcl_3:
                        lcl_3 = None
                    else:
                        rbnf_tmp_3 = rbnf_named__check_3
                        lcl_4 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                        rbnf_named__check_4 = lcl_4
                        lcl_4 = (rbnf_named__check_4 is None)
                        if lcl_4:
                            lcl_4 = None
                        else:
                            rbnf_tmp_4 = rbnf_named__check_4
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 17):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_5 = _rbnf_cur_token
                            rbnf_tmp_5 = lcl_5
                            lcl_5 = (rbnf_tmp_5 is None)
                            if lcl_5:
                                lcl_5 = None
                            else:
                                lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5)
                                lcl_6 = builtin_mk_ast('ConstantOperand2', lcl_6)
                                rbnf_tmp_1_ = lcl_6
                                lcl_5 = rbnf_tmp_1_
                            lcl_4 = lcl_5
                        lcl_3 = lcl_4
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_Dereferenceable(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 22):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 16):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_1 = lcl_3
                lcl_3 = (rbnf_tmp_1 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    lcl_4 = rbnf_named_parse_IntLit(builtin_state, builtin_tokens)
                    rbnf_named__check_2 = lcl_4
                    lcl_4 = (rbnf_named__check_2 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_2 = rbnf_named__check_2
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 17):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_3 = lcl_5
                        lcl_5 = (rbnf_tmp_3 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                            lcl_6 = builtin_mk_ast('Dereferenceable', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 21):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 16):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_1 = lcl_3
                lcl_3 = (rbnf_tmp_1 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    lcl_4 = rbnf_named_parse_IntLit(builtin_state, builtin_tokens)
                    rbnf_named__check_2 = lcl_4
                    lcl_4 = (rbnf_named__check_2 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_2 = rbnf_named__check_2
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 17):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_3 = lcl_5
                        lcl_5 = (rbnf_tmp_3 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                            lcl_6 = builtin_mk_ast('Dereferenceable', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_ExtValExpr(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 68):
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
            try:
                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                if (_rbnf_cur_token.idint is 16):
                    builtin_tokens.offset += 1
                else:
                    _rbnf_cur_token = None
            except IndexError:
                _rbnf_cur_token = None
            lcl_1 = _rbnf_cur_token
            rbnf_tmp_1 = lcl_1
            lcl_1 = (rbnf_tmp_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                lcl_2 = rbnf_named_parse_TypeConstant(builtin_state, builtin_tokens)
                rbnf_named__check_2 = lcl_2
                lcl_2 = (rbnf_named__check_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    rbnf_tmp_2 = rbnf_named__check_2
                    lcl_3 = builtin_tokens.offset
                    rbnf_named__off_2 = lcl_3
                    lcl_3 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_3:
                        lcl_5 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_5 = lcl_5.idint
                        if (lcl_5 == 19):
                            _rbnf_old_offset = builtin_tokens.offset
                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                            lcl_6 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_6
                            lcl_6 = rbnf_named_parse_IndexList(builtin_state, builtin_tokens)
                            rbnf_named__check_4 = lcl_6
                            lcl_6 = (rbnf_named__check_4 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_4 = rbnf_named__check_4
                                lcl_7 = (rbnf_tmp_3, rbnf_tmp_4)
                                rbnf_tmp_1_ = lcl_7
                                try:
                                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                    if (_rbnf_cur_token.idint is 17):
                                        builtin_tokens.offset += 1
                                    else:
                                        _rbnf_cur_token = None
                                except IndexError:
                                    _rbnf_cur_token = None
                                lcl_7 = _rbnf_cur_token
                                rbnf_tmp_5 = lcl_7
                                lcl_7 = (rbnf_tmp_5 is None)
                                if lcl_7:
                                    lcl_7 = None
                                else:
                                    lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_1_, rbnf_tmp_5)
                                    lcl_8 = builtin_mk_ast('ExtValExpr', lcl_8)
                                    rbnf_tmp_2_ = lcl_8
                                    lcl_7 = rbnf_tmp_2_
                                lcl_6 = lcl_7
                            lcl_4 = lcl_6
                        elif (lcl_5 == 17):
                            lcl_6 = ()
                            rbnf_tmp_1_ = lcl_6
                            _rbnf_old_offset = builtin_tokens.offset
                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                            lcl_6 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_6
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_1_, rbnf_tmp_3)
                            lcl_6 = builtin_mk_ast('ExtValExpr', lcl_6)
                            rbnf_tmp_2_ = lcl_6
                            lcl_4 = rbnf_tmp_2_
                        else:
                            lcl_4 = None
                        lcl_3 = lcl_4
                    else:
                        lcl_3 = None
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_ExtValInst(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 68):
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
            lcl_1 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 19):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_2 = _rbnf_cur_token
                rbnf_tmp_2 = lcl_2
                lcl_2 = (rbnf_tmp_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    lcl_3 = rbnf_named_parse_IndexList(builtin_state, builtin_tokens)
                    rbnf_named__check_3 = lcl_3
                    lcl_3 = (rbnf_named__check_3 is None)
                    if lcl_3:
                        lcl_3 = None
                    else:
                        rbnf_tmp_3 = rbnf_named__check_3
                        lcl_4 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                        lcl_4 = builtin_mk_ast('ExtValInst', lcl_4)
                        rbnf_tmp_1_ = lcl_4
                        lcl_3 = rbnf_tmp_1_
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_ExternLinkage(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 122):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('ExternLinkage', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 121):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('ExternLinkage', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_FPred(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 111):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('FPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 110):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('FPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 101):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('FPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 100):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('FPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 99):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('FPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 98):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('FPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 109):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('FPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 38):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('FPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 108):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('FPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 107):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('FPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 106):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('FPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 105):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('FPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 104):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('FPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 103):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('FPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 102):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('FPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 39):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('FPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_FloatConst(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_FloatLit(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('FloatConst', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_FloatLit(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 10):
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
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('FloatLit', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_FloatType(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 29):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('FloatType', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 28):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('FloatType', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_FunctionBody(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 36):
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
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_1 = lcl_1
            lcl_1 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
            if lcl_1:
                lcl_3 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                lcl_3 = lcl_3.idint
                if (lcl_3 == 37):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    _rbnf_old_offset = builtin_tokens.offset
                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                    lcl_4 = builtin_mk_ast('FunctionBody', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_2 = rbnf_tmp_2_
                elif (lcl_3 == 67):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 59):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 57):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 81):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 53):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 77):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 60):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 62):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 87):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 58):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 79):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 86):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 66):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 55):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 63):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 83):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 69):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 84):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 70):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 54):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 61):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 56):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 85):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 52):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 68):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 80):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 73):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 64):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 65):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 82):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 51):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 4):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 3):
                    lcl_4 = rbnf_named_parse_BasicBlockList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionBody', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                else:
                    lcl_2 = None
                lcl_1 = lcl_2
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_FunctionDecl(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 75):
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
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_1 = lcl_1
            lcl_1 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
            if lcl_1:
                lcl_3 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                lcl_3 = lcl_3.idint
                if (lcl_3 == 36):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_parse_FunctionHeader(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('FunctionDecl', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_2 = lcl_4
                elif (lcl_3 == 15):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_parse_FunctionHeader(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('FunctionDecl', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_2 = lcl_4
                elif (lcl_3 == 32):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_parse_FunctionHeader(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('FunctionDecl', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_2 = lcl_4
                elif (lcl_3 == 24):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_parse_FunctionHeader(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('FunctionDecl', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_2 = lcl_4
                elif (lcl_3 == 27):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_parse_FunctionHeader(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('FunctionDecl', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_2 = lcl_4
                elif (lcl_3 == 26):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_parse_FunctionHeader(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('FunctionDecl', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_2 = lcl_4
                elif (lcl_3 == 25):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_parse_FunctionHeader(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('FunctionDecl', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_2 = lcl_4
                elif (lcl_3 == 23):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_parse_FunctionHeader(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('FunctionDecl', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_2 = lcl_4
                elif (lcl_3 == 29):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_parse_FunctionHeader(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('FunctionDecl', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_2 = lcl_4
                elif (lcl_3 == 28):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_parse_FunctionHeader(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('FunctionDecl', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_2 = lcl_4
                elif (lcl_3 == 122):
                    lcl_4 = rbnf_named_parse_ExternLinkage(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_FunctionHeader(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionDecl', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 121):
                    lcl_4 = rbnf_named_parse_ExternLinkage(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_FunctionHeader(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('FunctionDecl', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 33):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_parse_FunctionHeader(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('FunctionDecl', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_2 = lcl_4
                elif (lcl_3 == 4):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_parse_FunctionHeader(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('FunctionDecl', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_2 = lcl_4
                else:
                    lcl_2 = None
                lcl_1 = lcl_2
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_FunctionDef(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 76):
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
            lcl_1 = rbnf_named_parse_FunctionHeader(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                lcl_2 = rbnf_named_parse_FunctionBody(builtin_state, builtin_tokens)
                rbnf_named__check_2 = lcl_2
                lcl_2 = (rbnf_named__check_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    rbnf_tmp_2 = rbnf_named__check_2
                    lcl_3 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                    lcl_3 = builtin_mk_ast('FunctionDef', lcl_3)
                    rbnf_tmp_1_ = lcl_3
                    lcl_2 = rbnf_tmp_1_
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_FunctionHeader(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = rbnf_named_parse_GlobalName(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 16):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_2 = _rbnf_cur_token
                rbnf_tmp_2 = lcl_2
                lcl_2 = (rbnf_tmp_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    lcl_3 = rbnf_named_parse_Params(builtin_state, builtin_tokens)
                    rbnf_named__check_3 = lcl_3
                    lcl_3 = (rbnf_named__check_3 is None)
                    if lcl_3:
                        lcl_3 = None
                    else:
                        rbnf_tmp_3 = rbnf_named__check_3
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 17):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_4 = _rbnf_cur_token
                        rbnf_tmp_4 = lcl_4
                        lcl_4 = (rbnf_tmp_4 is None)
                        if lcl_4:
                            lcl_4 = None
                        else:
                            lcl_5 = builtin_tokens.offset
                            rbnf_named__off_2 = lcl_5
                            lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                            if lcl_5:
                                lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                lcl_7 = lcl_7.idint
                                if (lcl_7 == 129):
                                    lcl_8 = rbnf_named_parse_UnnamedAddr(builtin_state, builtin_tokens)
                                    rbnf_named__check_5 = lcl_8
                                    lcl_8 = (rbnf_named__check_5 is None)
                                    if lcl_8:
                                        lcl_8 = None
                                    else:
                                        rbnf_tmp_5 = rbnf_named__check_5
                                        lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5)
                                        lcl_9 = builtin_mk_ast('FunctionHeader', lcl_9)
                                        rbnf_tmp_1_ = lcl_9
                                        lcl_8 = rbnf_tmp_1_
                                    lcl_6 = lcl_8
                                elif (lcl_7 == 128):
                                    lcl_8 = rbnf_named_parse_UnnamedAddr(builtin_state, builtin_tokens)
                                    rbnf_named__check_5 = lcl_8
                                    lcl_8 = (rbnf_named__check_5 is None)
                                    if lcl_8:
                                        lcl_8 = None
                                    else:
                                        rbnf_tmp_5 = rbnf_named__check_5
                                        lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5)
                                        lcl_9 = builtin_mk_ast('FunctionHeader', lcl_9)
                                        rbnf_tmp_1_ = lcl_9
                                        lcl_8 = rbnf_tmp_1_
                                    lcl_6 = lcl_8
                                else:
                                    lcl_8 = ()
                                    rbnf_tmp_1_ = lcl_8
                                    lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_1_)
                                    lcl_8 = builtin_mk_ast('FunctionHeader', lcl_8)
                                    rbnf_tmp_2_ = lcl_8
                                    lcl_6 = rbnf_tmp_2_
                                lcl_5 = lcl_6
                            else:
                                lcl_5 = None
                            lcl_4 = lcl_5
                        lcl_3 = lcl_4
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_GEPConstIndex(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 72):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = rbnf_named_parse_IntType(builtin_state, builtin_tokens)
                rbnf_named__check_1 = lcl_3
                lcl_3 = (rbnf_named__check_1 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_1 = rbnf_named__check_1
                    lcl_4 = rbnf_named_parse_IntLit(builtin_state, builtin_tokens)
                    rbnf_named__check_2 = lcl_4
                    lcl_4 = (rbnf_named__check_2 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_2 = rbnf_named__check_2
                        lcl_5 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                        lcl_5 = builtin_mk_ast('GEPConstIndex', lcl_5)
                        rbnf_tmp_1_ = lcl_5
                        lcl_4 = rbnf_tmp_1_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 24):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_IntType(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_IntLit(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('GEPConstIndex', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 27):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_IntType(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_IntLit(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('GEPConstIndex', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 26):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_IntType(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_IntLit(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('GEPConstIndex', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 25):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_IntType(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_IntLit(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('GEPConstIndex', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 23):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_IntType(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_IntLit(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('GEPConstIndex', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_GEPConstIndexList(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_GEPConstIndex(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('GEPConstIndexList', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_GEPConstIndexList(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_GEPExpr(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 70):
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
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_1 = lcl_1
            lcl_1 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
            if lcl_1:
                lcl_3 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                lcl_3 = lcl_3.idint
                if (lcl_3 == 71):
                    _rbnf_old_offset = builtin_tokens.offset
                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_4
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 16):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_2 = lcl_4
                    lcl_4 = (rbnf_tmp_2 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        lcl_5 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                        rbnf_named__check_3 = lcl_5
                        lcl_5 = (rbnf_named__check_3 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_3 = rbnf_named__check_3
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 19):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_6 = _rbnf_cur_token
                            rbnf_tmp_4 = lcl_6
                            lcl_6 = (rbnf_tmp_4 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                lcl_7 = rbnf_named_parse_TypeConstant(builtin_state, builtin_tokens)
                                rbnf_named__check_5 = lcl_7
                                lcl_7 = (rbnf_named__check_5 is None)
                                if lcl_7:
                                    lcl_7 = None
                                else:
                                    rbnf_tmp_5 = rbnf_named__check_5
                                    lcl_8 = builtin_tokens.offset
                                    rbnf_named__off_4 = lcl_8
                                    lcl_8 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                    if lcl_8:
                                        lcl_10 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                        lcl_10 = lcl_10.idint
                                        if (lcl_10 == 19):
                                            _rbnf_old_offset = builtin_tokens.offset
                                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                                            lcl_11 = _rbnf_cur_token
                                            rbnf_tmp_6 = lcl_11
                                            lcl_11 = rbnf_named_parse_GEPConstIndexList(builtin_state, builtin_tokens)
                                            rbnf_named__check_7 = lcl_11
                                            lcl_11 = (rbnf_named__check_7 is None)
                                            if lcl_11:
                                                lcl_11 = None
                                            else:
                                                rbnf_tmp_7 = rbnf_named__check_7
                                                lcl_12 = (rbnf_tmp_6, rbnf_tmp_7)
                                                rbnf_tmp_1_ = lcl_12
                                                try:
                                                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                                    if (_rbnf_cur_token.idint is 17):
                                                        builtin_tokens.offset += 1
                                                    else:
                                                        _rbnf_cur_token = None
                                                except IndexError:
                                                    _rbnf_cur_token = None
                                                lcl_12 = _rbnf_cur_token
                                                rbnf_tmp_8 = lcl_12
                                                lcl_12 = (rbnf_tmp_8 is None)
                                                if lcl_12:
                                                    lcl_12 = None
                                                else:
                                                    lcl_13 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_1_, rbnf_tmp_8)
                                                    lcl_13 = builtin_mk_ast('GEPExpr', lcl_13)
                                                    rbnf_tmp_2_ = lcl_13
                                                    lcl_12 = rbnf_tmp_2_
                                                lcl_11 = lcl_12
                                            lcl_9 = lcl_11
                                        elif (lcl_10 == 17):
                                            lcl_11 = ()
                                            rbnf_tmp_1_ = lcl_11
                                            _rbnf_old_offset = builtin_tokens.offset
                                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                                            lcl_11 = _rbnf_cur_token
                                            rbnf_tmp_6 = lcl_11
                                            lcl_11 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_1_, rbnf_tmp_6)
                                            lcl_11 = builtin_mk_ast('GEPExpr', lcl_11)
                                            rbnf_tmp_2_ = lcl_11
                                            lcl_9 = rbnf_tmp_2_
                                        else:
                                            lcl_9 = None
                                        lcl_8 = lcl_9
                                    else:
                                        lcl_8 = None
                                    lcl_7 = lcl_8
                                lcl_6 = lcl_7
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 16):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    _rbnf_old_offset = builtin_tokens.offset
                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                    lcl_10 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_2 = lcl_10
                    lcl_10 = (rbnf_named__check_2 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_2 = rbnf_named__check_2
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_3 = lcl_11
                        lcl_11 = (rbnf_tmp_3 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_12 = rbnf_named_parse_TypeConstant(builtin_state, builtin_tokens)
                            rbnf_named__check_4 = lcl_12
                            lcl_12 = (rbnf_named__check_4 is None)
                            if lcl_12:
                                lcl_12 = None
                            else:
                                rbnf_tmp_4 = rbnf_named__check_4
                                lcl_13 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_13
                                lcl_13 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_13:
                                    lcl_5 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_5 = lcl_5.idint
                                    if (lcl_5 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_6 = _rbnf_cur_token
                                        rbnf_tmp_5 = lcl_6
                                        lcl_6 = rbnf_named_parse_GEPConstIndexList(builtin_state, builtin_tokens)
                                        rbnf_named__check_6 = lcl_6
                                        lcl_6 = (rbnf_named__check_6 is None)
                                        if lcl_6:
                                            lcl_6 = None
                                        else:
                                            rbnf_tmp_6 = rbnf_named__check_6
                                            lcl_7 = (rbnf_tmp_5, rbnf_tmp_6)
                                            rbnf_tmp_2_ = lcl_7
                                            try:
                                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                                if (_rbnf_cur_token.idint is 17):
                                                    builtin_tokens.offset += 1
                                                else:
                                                    _rbnf_cur_token = None
                                            except IndexError:
                                                _rbnf_cur_token = None
                                            lcl_7 = _rbnf_cur_token
                                            rbnf_tmp_7 = lcl_7
                                            lcl_7 = (rbnf_tmp_7 is None)
                                            if lcl_7:
                                                lcl_7 = None
                                            else:
                                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_2_, rbnf_tmp_7)
                                                lcl_8 = builtin_mk_ast('GEPExpr', lcl_8)
                                                rbnf_tmp_3_ = lcl_8
                                                lcl_7 = rbnf_tmp_3_
                                            lcl_6 = lcl_7
                                        lcl_4 = lcl_6
                                    elif (lcl_5 == 17):
                                        lcl_6 = ()
                                        rbnf_tmp_2_ = lcl_6
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_6 = _rbnf_cur_token
                                        rbnf_tmp_5 = lcl_6
                                        lcl_6 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_2_, rbnf_tmp_5)
                                        lcl_6 = builtin_mk_ast('GEPExpr', lcl_6)
                                        rbnf_tmp_3_ = lcl_6
                                        lcl_4 = rbnf_tmp_3_
                                    else:
                                        lcl_4 = None
                                    lcl_13 = lcl_4
                                else:
                                    lcl_13 = None
                                lcl_12 = lcl_13
                            lcl_11 = lcl_12
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                else:
                    lcl_2 = None
                lcl_1 = lcl_2
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_GEPInst(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 70):
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
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_1 = lcl_1
            lcl_1 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
            if lcl_1:
                lcl_3 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                lcl_3 = lcl_3.idint
                if (lcl_3 == 36):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_7
                                lcl_7 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_7:
                                    lcl_9 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_9 = lcl_9.idint
                                    if (lcl_9 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_10 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_10
                                        lcl_10 = rbnf_named_parse_SepTypeValueList(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_10
                                        lcl_10 = (rbnf_named__check_5 is None)
                                        if lcl_10:
                                            lcl_10 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_11 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_11
                                            lcl_11 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_11 = builtin_mk_ast('GEPInst', lcl_11)
                                            rbnf_tmp_3_ = lcl_11
                                            lcl_10 = rbnf_tmp_3_
                                        lcl_8 = lcl_10
                                    else:
                                        lcl_10 = ()
                                        rbnf_tmp_2_ = lcl_10
                                        lcl_10 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_10 = builtin_mk_ast('GEPInst', lcl_10)
                                        rbnf_tmp_3_ = lcl_10
                                        lcl_8 = rbnf_tmp_3_
                                    lcl_7 = lcl_8
                                else:
                                    lcl_7 = None
                                lcl_6 = lcl_7
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 15):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_SepTypeValueList(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('GEPInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('GEPInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 32):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_SepTypeValueList(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('GEPInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('GEPInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 71):
                    _rbnf_old_offset = builtin_tokens.offset
                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                    lcl_10 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_2 = lcl_10
                    lcl_10 = (rbnf_named__check_2 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_2 = rbnf_named__check_2
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_3 = lcl_11
                        lcl_11 = (rbnf_tmp_3 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_4 = lcl_4
                            lcl_4 = (rbnf_named__check_4 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_4 = rbnf_named__check_4
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_5 = lcl_8
                                        lcl_8 = rbnf_named_parse_SepTypeValueList(builtin_state, builtin_tokens)
                                        rbnf_named__check_6 = lcl_8
                                        lcl_8 = (rbnf_named__check_6 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_6 = rbnf_named__check_6
                                            lcl_9 = (rbnf_tmp_5, rbnf_tmp_6)
                                            rbnf_tmp_1_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_1_)
                                            lcl_9 = builtin_mk_ast('GEPInst', lcl_9)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_8 = rbnf_tmp_2_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_1_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_1_)
                                        lcl_8 = builtin_mk_ast('GEPInst', lcl_8)
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_6 = rbnf_tmp_2_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 24):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_SepTypeValueList(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('GEPInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('GEPInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 27):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_SepTypeValueList(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('GEPInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('GEPInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 26):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_SepTypeValueList(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('GEPInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('GEPInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 25):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_SepTypeValueList(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('GEPInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('GEPInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 23):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_SepTypeValueList(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('GEPInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('GEPInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 29):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_SepTypeValueList(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('GEPInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('GEPInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 28):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_SepTypeValueList(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('GEPInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('GEPInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 33):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_SepTypeValueList(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('GEPInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('GEPInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 4):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_SepTypeValueList(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('GEPInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('GEPInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                else:
                    lcl_2 = None
                lcl_1 = lcl_2
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_Global(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_GlobalName(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            try:
                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                if (_rbnf_cur_token.idint is 12):
                    builtin_tokens.offset += 1
                else:
                    _rbnf_cur_token = None
            except IndexError:
                _rbnf_cur_token = None
            lcl_1 = _rbnf_cur_token
            rbnf_tmp_1 = lcl_1
            lcl_1 = (rbnf_tmp_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                lcl_2 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_2
                lcl_2 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                if lcl_2:
                    lcl_4 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                    lcl_4 = lcl_4.idint
                    if (lcl_4 == 129):
                        lcl_5 = rbnf_named_parse_GlobalDef(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('Global', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 128):
                        lcl_5 = rbnf_named_parse_GlobalDef(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('Global', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 132):
                        lcl_5 = rbnf_named_parse_GlobalDef(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('Global', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 122):
                        lcl_5 = rbnf_named_parse_GlobalDecl(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('Global', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 121):
                        lcl_5 = rbnf_named_parse_GlobalDecl(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('Global', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 124):
                        lcl_5 = rbnf_named_parse_GlobalDef(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('Global', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 123):
                        lcl_5 = rbnf_named_parse_GlobalDef(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('Global', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 131):
                        lcl_5 = rbnf_named_parse_GlobalDef(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('Global', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    else:
                        lcl_3 = None
                    lcl_2 = lcl_3
                else:
                    lcl_2 = None
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_GlobalDecl(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_ExternLinkage(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
            if lcl_1:
                lcl_3 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                lcl_3 = lcl_3.idint
                if (lcl_3 == 129):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = rbnf_named_parse_UnnamedAddr(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_7 = builtin_mk_ast('GlobalDecl', lcl_7)
                                rbnf_tmp_2_ = lcl_7
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 128):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = rbnf_named_parse_UnnamedAddr(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_7 = builtin_mk_ast('GlobalDecl', lcl_7)
                                rbnf_tmp_2_ = lcl_7
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 132):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = ()
                    rbnf_tmp_2_ = lcl_4
                    lcl_4 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_2_, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('GlobalDecl', lcl_6)
                            rbnf_tmp_3_ = lcl_6
                            lcl_5 = rbnf_tmp_3_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 124):
                    lcl_4 = rbnf_named_parse_PreemptionSpecifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 129):
                                lcl_8 = rbnf_named_parse_UnnamedAddr(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                                    rbnf_named__check_3 = lcl_9
                                    lcl_9 = (rbnf_named__check_3 is None)
                                    if lcl_9:
                                        lcl_9 = None
                                    else:
                                        rbnf_tmp_3 = rbnf_named__check_3
                                        lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                        rbnf_named__check_4 = lcl_10
                                        lcl_10 = (rbnf_named__check_4 is None)
                                        if lcl_10:
                                            lcl_10 = None
                                        else:
                                            rbnf_tmp_4 = rbnf_named__check_4
                                            lcl_11 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                            lcl_11 = builtin_mk_ast('GlobalDecl', lcl_11)
                                            rbnf_tmp_1_ = lcl_11
                                            lcl_10 = rbnf_tmp_1_
                                        lcl_9 = lcl_10
                                    lcl_8 = lcl_9
                                lcl_6 = lcl_8
                            elif (lcl_7 == 128):
                                lcl_10 = rbnf_named_parse_UnnamedAddr(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_10
                                lcl_10 = (rbnf_named__check_2 is None)
                                if lcl_10:
                                    lcl_10 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_11 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                                    rbnf_named__check_3 = lcl_11
                                    lcl_11 = (rbnf_named__check_3 is None)
                                    if lcl_11:
                                        lcl_11 = None
                                    else:
                                        rbnf_tmp_3 = rbnf_named__check_3
                                        lcl_8 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                        rbnf_named__check_4 = lcl_8
                                        lcl_8 = (rbnf_named__check_4 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_4 = rbnf_named__check_4
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                            lcl_9 = builtin_mk_ast('GlobalDecl', lcl_9)
                                            rbnf_tmp_1_ = lcl_9
                                            lcl_8 = rbnf_tmp_1_
                                        lcl_11 = lcl_8
                                    lcl_10 = lcl_11
                                lcl_6 = lcl_10
                            elif (lcl_7 == 132):
                                lcl_10 = ()
                                rbnf_tmp_1_ = lcl_10
                                lcl_10 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_10
                                lcl_10 = (rbnf_named__check_2 is None)
                                if lcl_10:
                                    lcl_10 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_11 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                    rbnf_named__check_3 = lcl_11
                                    lcl_11 = (rbnf_named__check_3 is None)
                                    if lcl_11:
                                        lcl_11 = None
                                    else:
                                        rbnf_tmp_3 = rbnf_named__check_3
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2, rbnf_tmp_3)
                                        lcl_8 = builtin_mk_ast('GlobalDecl', lcl_8)
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_11 = rbnf_tmp_2_
                                    lcl_10 = lcl_11
                                lcl_6 = lcl_10
                            elif (lcl_7 == 131):
                                lcl_10 = ()
                                rbnf_tmp_1_ = lcl_10
                                lcl_10 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_10
                                lcl_10 = (rbnf_named__check_2 is None)
                                if lcl_10:
                                    lcl_10 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_11 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                    rbnf_named__check_3 = lcl_11
                                    lcl_11 = (rbnf_named__check_3 is None)
                                    if lcl_11:
                                        lcl_11 = None
                                    else:
                                        rbnf_tmp_3 = rbnf_named__check_3
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2, rbnf_tmp_3)
                                        lcl_8 = builtin_mk_ast('GlobalDecl', lcl_8)
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_11 = rbnf_tmp_2_
                                    lcl_10 = lcl_11
                                lcl_6 = lcl_10
                            else:
                                lcl_6 = None
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 123):
                    lcl_10 = rbnf_named_parse_PreemptionSpecifier(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_11 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_11
                        lcl_11 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_11:
                            lcl_5 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_5 = lcl_5.idint
                            if (lcl_5 == 129):
                                lcl_6 = rbnf_named_parse_UnnamedAddr(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_6
                                lcl_6 = (rbnf_named__check_2 is None)
                                if lcl_6:
                                    lcl_6 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_7 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                                    rbnf_named__check_3 = lcl_7
                                    lcl_7 = (rbnf_named__check_3 is None)
                                    if lcl_7:
                                        lcl_7 = None
                                    else:
                                        rbnf_tmp_3 = rbnf_named__check_3
                                        lcl_8 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                        rbnf_named__check_4 = lcl_8
                                        lcl_8 = (rbnf_named__check_4 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_4 = rbnf_named__check_4
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                            lcl_9 = builtin_mk_ast('GlobalDecl', lcl_9)
                                            rbnf_tmp_1_ = lcl_9
                                            lcl_8 = rbnf_tmp_1_
                                        lcl_7 = lcl_8
                                    lcl_6 = lcl_7
                                lcl_4 = lcl_6
                            elif (lcl_5 == 128):
                                lcl_6 = rbnf_named_parse_UnnamedAddr(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_6
                                lcl_6 = (rbnf_named__check_2 is None)
                                if lcl_6:
                                    lcl_6 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_7 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                                    rbnf_named__check_3 = lcl_7
                                    lcl_7 = (rbnf_named__check_3 is None)
                                    if lcl_7:
                                        lcl_7 = None
                                    else:
                                        rbnf_tmp_3 = rbnf_named__check_3
                                        lcl_8 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                        rbnf_named__check_4 = lcl_8
                                        lcl_8 = (rbnf_named__check_4 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_4 = rbnf_named__check_4
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                            lcl_9 = builtin_mk_ast('GlobalDecl', lcl_9)
                                            rbnf_tmp_1_ = lcl_9
                                            lcl_8 = rbnf_tmp_1_
                                        lcl_7 = lcl_8
                                    lcl_6 = lcl_7
                                lcl_4 = lcl_6
                            elif (lcl_5 == 132):
                                lcl_6 = ()
                                rbnf_tmp_1_ = lcl_6
                                lcl_6 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_6
                                lcl_6 = (rbnf_named__check_2 is None)
                                if lcl_6:
                                    lcl_6 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_7 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                    rbnf_named__check_3 = lcl_7
                                    lcl_7 = (rbnf_named__check_3 is None)
                                    if lcl_7:
                                        lcl_7 = None
                                    else:
                                        rbnf_tmp_3 = rbnf_named__check_3
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2, rbnf_tmp_3)
                                        lcl_8 = builtin_mk_ast('GlobalDecl', lcl_8)
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_7 = rbnf_tmp_2_
                                    lcl_6 = lcl_7
                                lcl_4 = lcl_6
                            elif (lcl_5 == 131):
                                lcl_6 = ()
                                rbnf_tmp_1_ = lcl_6
                                lcl_6 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_6
                                lcl_6 = (rbnf_named__check_2 is None)
                                if lcl_6:
                                    lcl_6 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_7 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                    rbnf_named__check_3 = lcl_7
                                    lcl_7 = (rbnf_named__check_3 is None)
                                    if lcl_7:
                                        lcl_7 = None
                                    else:
                                        rbnf_tmp_3 = rbnf_named__check_3
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_, rbnf_tmp_2, rbnf_tmp_3)
                                        lcl_8 = builtin_mk_ast('GlobalDecl', lcl_8)
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_7 = rbnf_tmp_2_
                                    lcl_6 = lcl_7
                                lcl_4 = lcl_6
                            else:
                                lcl_4 = None
                            lcl_11 = lcl_4
                        else:
                            lcl_11 = None
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 131):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_10
                    lcl_10 = ()
                    rbnf_tmp_2_ = lcl_10
                    lcl_10 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_11 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_11
                        lcl_11 = (rbnf_named__check_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_4 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_2_, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_4 = builtin_mk_ast('GlobalDecl', lcl_4)
                            rbnf_tmp_3_ = lcl_4
                            lcl_11 = rbnf_tmp_3_
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                else:
                    lcl_2 = None
                lcl_1 = lcl_2
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_GlobalDef(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 129):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_UnnamedAddr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_7 = builtin_mk_ast('GlobalDef', lcl_7)
                                rbnf_tmp_2_ = lcl_7
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 128):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = rbnf_named_parse_UnnamedAddr(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = (rbnf_tmp_1_, rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_7 = builtin_mk_ast('GlobalDef', lcl_7)
                                rbnf_tmp_2_ = lcl_7
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 132):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = ()
                rbnf_tmp_2_ = lcl_3
                lcl_3 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_1_, rbnf_tmp_2_, rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('GlobalDef', lcl_6)
                            rbnf_tmp_3_ = lcl_6
                            lcl_5 = rbnf_tmp_3_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 124):
                lcl_3 = rbnf_named_parse_PreemptionSpecifier(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_4:
                        lcl_6 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_6 = lcl_6.idint
                        if (lcl_6 == 129):
                            lcl_7 = rbnf_named_parse_UnnamedAddr(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                    rbnf_named__check_3 = lcl_9
                                    lcl_9 = (rbnf_named__check_3 is None)
                                    if lcl_9:
                                        lcl_9 = None
                                    else:
                                        rbnf_tmp_3 = rbnf_named__check_3
                                        lcl_10 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                                        rbnf_named__check_4 = lcl_10
                                        lcl_10 = (rbnf_named__check_4 is None)
                                        if lcl_10:
                                            lcl_10 = None
                                        else:
                                            rbnf_tmp_4 = rbnf_named__check_4
                                            lcl_11 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                            lcl_11 = builtin_mk_ast('GlobalDef', lcl_11)
                                            rbnf_tmp_1_ = lcl_11
                                            lcl_10 = rbnf_tmp_1_
                                        lcl_9 = lcl_10
                                    lcl_8 = lcl_9
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 128):
                            lcl_10 = rbnf_named_parse_UnnamedAddr(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_10
                            lcl_10 = (rbnf_named__check_1 is None)
                            if lcl_10:
                                lcl_10 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_11 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_11
                                lcl_11 = (rbnf_named__check_2 is None)
                                if lcl_11:
                                    lcl_11 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_7 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                    rbnf_named__check_3 = lcl_7
                                    lcl_7 = (rbnf_named__check_3 is None)
                                    if lcl_7:
                                        lcl_7 = None
                                    else:
                                        rbnf_tmp_3 = rbnf_named__check_3
                                        lcl_8 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                                        rbnf_named__check_4 = lcl_8
                                        lcl_8 = (rbnf_named__check_4 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_4 = rbnf_named__check_4
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                            lcl_9 = builtin_mk_ast('GlobalDef', lcl_9)
                                            rbnf_tmp_1_ = lcl_9
                                            lcl_8 = rbnf_tmp_1_
                                        lcl_7 = lcl_8
                                    lcl_11 = lcl_7
                                lcl_10 = lcl_11
                            lcl_5 = lcl_10
                        elif (lcl_6 == 132):
                            lcl_10 = ()
                            rbnf_tmp_1_ = lcl_10
                            lcl_10 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_10
                            lcl_10 = (rbnf_named__check_1 is None)
                            if lcl_10:
                                lcl_10 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_11 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_11
                                lcl_11 = (rbnf_named__check_2 is None)
                                if lcl_11:
                                    lcl_11 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_7 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                                    rbnf_named__check_3 = lcl_7
                                    lcl_7 = (rbnf_named__check_3 is None)
                                    if lcl_7:
                                        lcl_7 = None
                                    else:
                                        rbnf_tmp_3 = rbnf_named__check_3
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                        lcl_8 = builtin_mk_ast('GlobalDef', lcl_8)
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_7 = rbnf_tmp_2_
                                    lcl_11 = lcl_7
                                lcl_10 = lcl_11
                            lcl_5 = lcl_10
                        elif (lcl_6 == 131):
                            lcl_10 = ()
                            rbnf_tmp_1_ = lcl_10
                            lcl_10 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_10
                            lcl_10 = (rbnf_named__check_1 is None)
                            if lcl_10:
                                lcl_10 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_11 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_11
                                lcl_11 = (rbnf_named__check_2 is None)
                                if lcl_11:
                                    lcl_11 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_7 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                                    rbnf_named__check_3 = lcl_7
                                    lcl_7 = (rbnf_named__check_3 is None)
                                    if lcl_7:
                                        lcl_7 = None
                                    else:
                                        rbnf_tmp_3 = rbnf_named__check_3
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                        lcl_8 = builtin_mk_ast('GlobalDef', lcl_8)
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_7 = rbnf_tmp_2_
                                    lcl_11 = lcl_7
                                lcl_10 = lcl_11
                            lcl_5 = lcl_10
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    else:
                        lcl_4 = None
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 123):
                lcl_10 = rbnf_named_parse_PreemptionSpecifier(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_10
                lcl_10 = (rbnf_named__check_0 is None)
                if lcl_10:
                    lcl_10 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_11 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_11
                    lcl_11 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_11:
                        lcl_4 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_4 = lcl_4.idint
                        if (lcl_4 == 129):
                            lcl_5 = rbnf_named_parse_UnnamedAddr(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_5
                            lcl_5 = (rbnf_named__check_1 is None)
                            if lcl_5:
                                lcl_5 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_6 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_6
                                lcl_6 = (rbnf_named__check_2 is None)
                                if lcl_6:
                                    lcl_6 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_7 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                    rbnf_named__check_3 = lcl_7
                                    lcl_7 = (rbnf_named__check_3 is None)
                                    if lcl_7:
                                        lcl_7 = None
                                    else:
                                        rbnf_tmp_3 = rbnf_named__check_3
                                        lcl_8 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                                        rbnf_named__check_4 = lcl_8
                                        lcl_8 = (rbnf_named__check_4 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_4 = rbnf_named__check_4
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                            lcl_9 = builtin_mk_ast('GlobalDef', lcl_9)
                                            rbnf_tmp_1_ = lcl_9
                                            lcl_8 = rbnf_tmp_1_
                                        lcl_7 = lcl_8
                                    lcl_6 = lcl_7
                                lcl_5 = lcl_6
                            lcl_3 = lcl_5
                        elif (lcl_4 == 128):
                            lcl_5 = rbnf_named_parse_UnnamedAddr(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_5
                            lcl_5 = (rbnf_named__check_1 is None)
                            if lcl_5:
                                lcl_5 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_6 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_6
                                lcl_6 = (rbnf_named__check_2 is None)
                                if lcl_6:
                                    lcl_6 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_7 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                    rbnf_named__check_3 = lcl_7
                                    lcl_7 = (rbnf_named__check_3 is None)
                                    if lcl_7:
                                        lcl_7 = None
                                    else:
                                        rbnf_tmp_3 = rbnf_named__check_3
                                        lcl_8 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                                        rbnf_named__check_4 = lcl_8
                                        lcl_8 = (rbnf_named__check_4 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_4 = rbnf_named__check_4
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                            lcl_9 = builtin_mk_ast('GlobalDef', lcl_9)
                                            rbnf_tmp_1_ = lcl_9
                                            lcl_8 = rbnf_tmp_1_
                                        lcl_7 = lcl_8
                                    lcl_6 = lcl_7
                                lcl_5 = lcl_6
                            lcl_3 = lcl_5
                        elif (lcl_4 == 132):
                            lcl_5 = ()
                            rbnf_tmp_1_ = lcl_5
                            lcl_5 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_5
                            lcl_5 = (rbnf_named__check_1 is None)
                            if lcl_5:
                                lcl_5 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_6 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_6
                                lcl_6 = (rbnf_named__check_2 is None)
                                if lcl_6:
                                    lcl_6 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_7 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                                    rbnf_named__check_3 = lcl_7
                                    lcl_7 = (rbnf_named__check_3 is None)
                                    if lcl_7:
                                        lcl_7 = None
                                    else:
                                        rbnf_tmp_3 = rbnf_named__check_3
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                        lcl_8 = builtin_mk_ast('GlobalDef', lcl_8)
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_7 = rbnf_tmp_2_
                                    lcl_6 = lcl_7
                                lcl_5 = lcl_6
                            lcl_3 = lcl_5
                        elif (lcl_4 == 131):
                            lcl_5 = ()
                            rbnf_tmp_1_ = lcl_5
                            lcl_5 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_5
                            lcl_5 = (rbnf_named__check_1 is None)
                            if lcl_5:
                                lcl_5 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_6 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_6
                                lcl_6 = (rbnf_named__check_2 is None)
                                if lcl_6:
                                    lcl_6 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_7 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                                    rbnf_named__check_3 = lcl_7
                                    lcl_7 = (rbnf_named__check_3 is None)
                                    if lcl_7:
                                        lcl_7 = None
                                    else:
                                        rbnf_tmp_3 = rbnf_named__check_3
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                        lcl_8 = builtin_mk_ast('GlobalDef', lcl_8)
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_7 = rbnf_tmp_2_
                                    lcl_6 = lcl_7
                                lcl_5 = lcl_6
                            lcl_3 = lcl_5
                        else:
                            lcl_3 = None
                        lcl_11 = lcl_3
                    else:
                        lcl_11 = None
                    lcl_10 = lcl_11
                lcl_1 = lcl_10
            elif (lcl_2 == 131):
                lcl_10 = ()
                rbnf_tmp_1_ = lcl_10
                lcl_10 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_10
                lcl_10 = ()
                rbnf_tmp_2_ = lcl_10
                lcl_10 = rbnf_named_parse_Immutable(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_10
                lcl_10 = (rbnf_named__check_0 is None)
                if lcl_10:
                    lcl_10 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_11 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_11
                    lcl_11 = (rbnf_named__check_1 is None)
                    if lcl_11:
                        lcl_11 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_3
                        lcl_3 = (rbnf_named__check_2 is None)
                        if lcl_3:
                            lcl_3 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_4 = (rbnf_tmp_1_, rbnf_tmp_2_, rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_4 = builtin_mk_ast('GlobalDef', lcl_4)
                            rbnf_tmp_3_ = lcl_4
                            lcl_3 = rbnf_tmp_3_
                        lcl_11 = lcl_3
                    lcl_10 = lcl_11
                lcl_1 = lcl_10
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_GlobalName(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 5):
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
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('GlobalName', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_IPred(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 101):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('IPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 100):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('IPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 99):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('IPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 98):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('IPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 97):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('IPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 96):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('IPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 95):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('IPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 94):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('IPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 93):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('IPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 92):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('IPred', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_Immutable(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 132):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('Immutable', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 131):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('Immutable', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_Inc(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 33):
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
            lcl_1 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 19):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_2 = _rbnf_cur_token
                rbnf_tmp_2 = lcl_2
                lcl_2 = (rbnf_tmp_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    lcl_3 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                    rbnf_named__check_3 = lcl_3
                    lcl_3 = (rbnf_named__check_3 is None)
                    if lcl_3:
                        lcl_3 = None
                    else:
                        rbnf_tmp_3 = rbnf_named__check_3
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 35):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_4 = _rbnf_cur_token
                        rbnf_tmp_4 = lcl_4
                        lcl_4 = (rbnf_tmp_4 is None)
                        if lcl_4:
                            lcl_4 = None
                        else:
                            lcl_5 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                            lcl_5 = builtin_mk_ast('Inc', lcl_5)
                            rbnf_tmp_1_ = lcl_5
                            lcl_4 = rbnf_tmp_1_
                        lcl_3 = lcl_4
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_IncList(builtin_state, builtin_tokens):
        lcl_0 = ()
        rbnf_tmp_1_ = lcl_0
        lcl_0 = rbnf_named_parse_Inc(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_1_, rbnf_tmp_0)
            lcl_1 = builtin_mk_ast('IncList', lcl_1)
            rbnf_tmp_2_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_IncList(rbnf_tmp_2_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_IndexList(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_IntLit(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('IndexList', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_IndexList(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_InsValExpr(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 69):
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
            try:
                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                if (_rbnf_cur_token.idint is 16):
                    builtin_tokens.offset += 1
                else:
                    _rbnf_cur_token = None
            except IndexError:
                _rbnf_cur_token = None
            lcl_1 = _rbnf_cur_token
            rbnf_tmp_1 = lcl_1
            lcl_1 = (rbnf_tmp_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                lcl_2 = rbnf_named_parse_TypeConstant(builtin_state, builtin_tokens)
                rbnf_named__check_2 = lcl_2
                lcl_2 = (rbnf_named__check_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    rbnf_tmp_2 = rbnf_named__check_2
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 19):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_3 = _rbnf_cur_token
                    rbnf_tmp_3 = lcl_3
                    lcl_3 = (rbnf_tmp_3 is None)
                    if lcl_3:
                        lcl_3 = None
                    else:
                        lcl_4 = rbnf_named_parse_TypeConstant(builtin_state, builtin_tokens)
                        rbnf_named__check_4 = lcl_4
                        lcl_4 = (rbnf_named__check_4 is None)
                        if lcl_4:
                            lcl_4 = None
                        else:
                            rbnf_tmp_4 = rbnf_named__check_4
                            lcl_5 = builtin_tokens.offset
                            rbnf_named__off_3 = lcl_5
                            lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                            if lcl_5:
                                lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                lcl_7 = lcl_7.idint
                                if (lcl_7 == 19):
                                    _rbnf_old_offset = builtin_tokens.offset
                                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_5 = lcl_8
                                    lcl_8 = rbnf_named_parse_IndexList(builtin_state, builtin_tokens)
                                    rbnf_named__check_6 = lcl_8
                                    lcl_8 = (rbnf_named__check_6 is None)
                                    if lcl_8:
                                        lcl_8 = None
                                    else:
                                        rbnf_tmp_6 = rbnf_named__check_6
                                        lcl_9 = (rbnf_tmp_5, rbnf_tmp_6)
                                        rbnf_tmp_1_ = lcl_9
                                        try:
                                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                            if (_rbnf_cur_token.idint is 17):
                                                builtin_tokens.offset += 1
                                            else:
                                                _rbnf_cur_token = None
                                        except IndexError:
                                            _rbnf_cur_token = None
                                        lcl_9 = _rbnf_cur_token
                                        rbnf_tmp_7 = lcl_9
                                        lcl_9 = (rbnf_tmp_7 is None)
                                        if lcl_9:
                                            lcl_9 = None
                                        else:
                                            lcl_10 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_1_, rbnf_tmp_7)
                                            lcl_10 = builtin_mk_ast('InsValExpr', lcl_10)
                                            rbnf_tmp_2_ = lcl_10
                                            lcl_9 = rbnf_tmp_2_
                                        lcl_8 = lcl_9
                                    lcl_6 = lcl_8
                                elif (lcl_7 == 17):
                                    lcl_10 = ()
                                    rbnf_tmp_1_ = lcl_10
                                    _rbnf_old_offset = builtin_tokens.offset
                                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                                    lcl_10 = _rbnf_cur_token
                                    rbnf_tmp_5 = lcl_10
                                    lcl_10 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_1_, rbnf_tmp_5)
                                    lcl_10 = builtin_mk_ast('InsValExpr', lcl_10)
                                    rbnf_tmp_2_ = lcl_10
                                    lcl_6 = rbnf_tmp_2_
                                else:
                                    lcl_6 = None
                                lcl_5 = lcl_6
                            else:
                                lcl_5 = None
                            lcl_4 = lcl_5
                        lcl_3 = lcl_4
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_InsValInst(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 69):
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
            lcl_1 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 19):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_2 = _rbnf_cur_token
                rbnf_tmp_2 = lcl_2
                lcl_2 = (rbnf_tmp_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    lcl_3 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                    rbnf_named__check_3 = lcl_3
                    lcl_3 = (rbnf_named__check_3 is None)
                    if lcl_3:
                        lcl_3 = None
                    else:
                        rbnf_tmp_3 = rbnf_named__check_3
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_4 = _rbnf_cur_token
                        rbnf_tmp_4 = lcl_4
                        lcl_4 = (rbnf_tmp_4 is None)
                        if lcl_4:
                            lcl_4 = None
                        else:
                            lcl_5 = rbnf_named_parse_IndexList(builtin_state, builtin_tokens)
                            rbnf_named__check_5 = lcl_5
                            lcl_5 = (rbnf_named__check_5 is None)
                            if lcl_5:
                                lcl_5 = None
                            else:
                                rbnf_tmp_5 = rbnf_named__check_5
                                lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5)
                                lcl_6 = builtin_mk_ast('InsValInst', lcl_6)
                                rbnf_tmp_1_ = lcl_6
                                lcl_5 = rbnf_tmp_1_
                            lcl_4 = lcl_5
                        lcl_3 = lcl_4
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_Instruction(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 67):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 59):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 57):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 53):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 77):
                lcl_3 = rbnf_named_parse_StoreInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 60):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 62):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 87):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 58):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 86):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 66):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 55):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 63):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 83):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 69):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 84):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 70):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 54):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 61):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 56):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 85):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 52):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 68):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 73):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 64):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 65):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 82):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 51):
                lcl_3 = ()
                rbnf_tmp_1_ = lcl_3
                lcl_3 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_1_, rbnf_tmp_0)
                    lcl_4 = builtin_mk_ast('Instruction', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_3 = rbnf_tmp_2_
                lcl_1 = lcl_3
            elif (lcl_2 == 4):
                lcl_3 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    try:
                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                        if (_rbnf_cur_token.idint is 12):
                            builtin_tokens.offset += 1
                        else:
                            _rbnf_cur_token = None
                    except IndexError:
                        _rbnf_cur_token = None
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = (rbnf_tmp_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        lcl_5 = (rbnf_tmp_0, rbnf_tmp_1)
                        rbnf_tmp_1_ = lcl_5
                        lcl_5 = rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_1_, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('Instruction', lcl_6)
                            rbnf_tmp_2_ = lcl_6
                            lcl_5 = rbnf_tmp_2_
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_InstructionList(builtin_state, builtin_tokens):
        lcl_0 = ()
        rbnf_tmp_1_ = lcl_0
        lcl_0 = rbnf_named_parse_Instruction(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_1_, rbnf_tmp_0)
            lcl_1 = builtin_mk_ast('InstructionList', lcl_1)
            rbnf_tmp_2_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_InstructionList(rbnf_tmp_2_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_IntConst(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_IntLit(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('IntConst', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_IntLit(builtin_state, builtin_tokens):
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
            lcl_0 = None
        else:
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('IntLit', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_IntType(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 24):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('IntType', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 27):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('IntType', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 26):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('IntType', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 25):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('IntType', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 23):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('IntType', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_LabelType(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 32):
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
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('LabelType', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_LoadInst(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 83):
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
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_1 = lcl_1
            lcl_1 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
            if lcl_1:
                lcl_3 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                lcl_3 = lcl_3.idint
                if (lcl_3 == 36):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_7
                                lcl_7 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_7:
                                    lcl_9 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_9 = lcl_9.idint
                                    if (lcl_9 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_10 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_10
                                        lcl_10 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_10
                                        lcl_10 = (rbnf_named__check_5 is None)
                                        if lcl_10:
                                            lcl_10 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_11 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_11
                                            lcl_11 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_11 = builtin_mk_ast('LoadInst', lcl_11)
                                            rbnf_tmp_3_ = lcl_11
                                            lcl_10 = rbnf_tmp_3_
                                        lcl_8 = lcl_10
                                    else:
                                        lcl_10 = ()
                                        rbnf_tmp_2_ = lcl_10
                                        lcl_10 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_10 = builtin_mk_ast('LoadInst', lcl_10)
                                        rbnf_tmp_3_ = lcl_10
                                        lcl_8 = rbnf_tmp_3_
                                    lcl_7 = lcl_8
                                else:
                                    lcl_7 = None
                                lcl_6 = lcl_7
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 78):
                    _rbnf_old_offset = builtin_tokens.offset
                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                    lcl_10 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_2 = lcl_10
                    lcl_10 = (rbnf_named__check_2 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_2 = rbnf_named__check_2
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_3 = lcl_11
                        lcl_11 = (rbnf_tmp_3 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_4 = lcl_4
                            lcl_4 = (rbnf_named__check_4 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_4 = rbnf_named__check_4
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_5 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_6 = lcl_8
                                        lcl_8 = (rbnf_named__check_6 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_6 = rbnf_named__check_6
                                            lcl_9 = (rbnf_tmp_5, rbnf_tmp_6)
                                            rbnf_tmp_1_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_1_)
                                            lcl_9 = builtin_mk_ast('LoadInst', lcl_9)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_8 = rbnf_tmp_2_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_1_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_1_)
                                        lcl_8 = builtin_mk_ast('LoadInst', lcl_8)
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_6 = rbnf_tmp_2_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 15):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('LoadInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('LoadInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 32):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('LoadInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('LoadInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 24):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('LoadInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('LoadInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 27):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('LoadInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('LoadInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 26):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('LoadInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('LoadInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 25):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('LoadInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('LoadInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 23):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('LoadInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('LoadInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 29):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('LoadInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('LoadInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 28):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('LoadInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('LoadInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 33):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('LoadInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('LoadInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 4):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_11 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_11
                        lcl_11 = (rbnf_tmp_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            lcl_4 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_4
                            lcl_4 = (rbnf_named__check_3 is None)
                            if lcl_4:
                                lcl_4 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_5 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_5
                                lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_5:
                                    lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_7 = lcl_7.idint
                                    if (lcl_7 == 19):
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_8 = _rbnf_cur_token
                                        rbnf_tmp_4 = lcl_8
                                        lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                        rbnf_named__check_5 = lcl_8
                                        lcl_8 = (rbnf_named__check_5 is None)
                                        if lcl_8:
                                            lcl_8 = None
                                        else:
                                            rbnf_tmp_5 = rbnf_named__check_5
                                            lcl_9 = (rbnf_tmp_4, rbnf_tmp_5)
                                            rbnf_tmp_2_ = lcl_9
                                            lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                            lcl_9 = builtin_mk_ast('LoadInst', lcl_9)
                                            rbnf_tmp_3_ = lcl_9
                                            lcl_8 = rbnf_tmp_3_
                                        lcl_6 = lcl_8
                                    else:
                                        lcl_8 = ()
                                        rbnf_tmp_2_ = lcl_8
                                        lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_2_)
                                        lcl_8 = builtin_mk_ast('LoadInst', lcl_8)
                                        rbnf_tmp_3_ = lcl_8
                                        lcl_6 = rbnf_tmp_3_
                                    lcl_5 = lcl_6
                                else:
                                    lcl_5 = None
                                lcl_4 = lcl_5
                            lcl_11 = lcl_4
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                else:
                    lcl_2 = None
                lcl_1 = lcl_2
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_LocalName(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 4):
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
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('LocalName', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_NamedType(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('NamedType', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_NoneConst(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 41):
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
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('NoneConst', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_NullConst(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 40):
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
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('NullConst', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_OpaqueType(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 13):
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
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('OpaqueType', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_OverflowFlags(builtin_state, builtin_tokens):
        lcl_0 = ()
        rbnf_tmp_1_ = lcl_0
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 50):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_1_, rbnf_tmp_0)
                lcl_3 = builtin_mk_ast('OverflowFlags', lcl_3)
                rbnf_tmp_2_ = lcl_3
                lcl_3 = rbnf_named_lr_loop_OverflowFlags(rbnf_tmp_2_, builtin_state, builtin_tokens)
                lcl_1 = lcl_3
            elif (lcl_2 == 49):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_1_, rbnf_tmp_0)
                lcl_3 = builtin_mk_ast('OverflowFlags', lcl_3)
                rbnf_tmp_2_ = lcl_3
                lcl_3 = rbnf_named_lr_loop_OverflowFlags(rbnf_tmp_2_, builtin_state, builtin_tokens)
                lcl_1 = lcl_3
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_Param(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_0 = lcl_1
            lcl_1 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
            if lcl_1:
                lcl_3 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                lcl_3 = lcl_3.idint
                if (lcl_3 == 8):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 154):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 153):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 152):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 151):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 150):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 149):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 148):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 147):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 146):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 145):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 144):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 143):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 142):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 141):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 140):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 22):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 21):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 139):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 20):
                    lcl_4 = rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = builtin_tokens.offset
                        rbnf_named__off_1 = lcl_5
                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                        if lcl_5:
                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                            lcl_7 = lcl_7.idint
                            if (lcl_7 == 4):
                                lcl_8 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                                rbnf_named__check_2 = lcl_8
                                lcl_8 = (rbnf_named__check_2 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    rbnf_tmp_2 = rbnf_named__check_2
                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                    lcl_9 = builtin_mk_ast('Param', lcl_9)
                                    rbnf_tmp_1_ = lcl_9
                                    lcl_8 = rbnf_tmp_1_
                                lcl_6 = lcl_8
                            else:
                                lcl_8 = ()
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_6 = rbnf_tmp_2_
                            lcl_5 = lcl_6
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 4):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                        lcl_5 = builtin_mk_ast('Param', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_4 = rbnf_tmp_2_
                    lcl_2 = lcl_4
                else:
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_4:
                        lcl_6 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_6 = lcl_6.idint
                        if (lcl_6 == 4):
                            lcl_7 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                            rbnf_named__check_1 = lcl_7
                            lcl_7 = (rbnf_named__check_1 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_1 = rbnf_named__check_1
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1)
                                lcl_8 = builtin_mk_ast('Param', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_5 = lcl_7
                        else:
                            lcl_7 = ()
                            rbnf_tmp_2_ = lcl_7
                            lcl_7 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_2_)
                            lcl_7 = builtin_mk_ast('Param', lcl_7)
                            rbnf_tmp_3_ = lcl_7
                            lcl_5 = rbnf_tmp_3_
                        lcl_4 = lcl_5
                    else:
                        lcl_4 = None
                    lcl_2 = lcl_4
                lcl_1 = lcl_2
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_ParamAttr(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 8):
                lcl_3 = rbnf_named_parse_StrLit(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ParamAttr', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 154):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('ParamAttr', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 153):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('ParamAttr', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 152):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('ParamAttr', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 151):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('ParamAttr', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 150):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('ParamAttr', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 149):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('ParamAttr', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 148):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('ParamAttr', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 147):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('ParamAttr', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 146):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('ParamAttr', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 145):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('ParamAttr', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 144):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('ParamAttr', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 143):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('ParamAttr', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 142):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('ParamAttr', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 141):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('ParamAttr', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 140):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('ParamAttr', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 22):
                lcl_3 = rbnf_named_parse_Dereferenceable(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ParamAttr', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 21):
                lcl_3 = rbnf_named_parse_Dereferenceable(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ParamAttr', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 139):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('ParamAttr', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 20):
                lcl_3 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ParamAttr', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_ParamAttrList(builtin_state, builtin_tokens):
        lcl_0 = ()
        rbnf_tmp_1_ = lcl_0
        lcl_0 = rbnf_named_parse_ParamAttr(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_1_, rbnf_tmp_0)
            lcl_1 = builtin_mk_ast('ParamAttrList', lcl_1)
            rbnf_tmp_2_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_ParamAttrList(rbnf_tmp_2_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_ParamAttrs(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_ParamAttrList(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('ParamAttrs', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_ParamList(builtin_state, builtin_tokens):
        lcl_0 = ()
        rbnf_tmp_1_ = lcl_0
        lcl_0 = rbnf_named_parse_Param(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_1_, rbnf_tmp_0)
            lcl_1 = builtin_mk_ast('ParamList', lcl_1)
            rbnf_tmp_2_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_ParamList(rbnf_tmp_2_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_Params(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 36):
                lcl_3 = rbnf_named_parse_ParamList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_4:
                        lcl_6 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_6 = lcl_6.idint
                        if (lcl_6 == 19):
                            _rbnf_old_offset = builtin_tokens.offset
                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 18):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_1, rbnf_tmp_2)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Params', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_5 = lcl_7
                        else:
                            lcl_7 = ()
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = (rbnf_tmp_0, rbnf_tmp_1_)
                            lcl_7 = builtin_mk_ast('Params', lcl_7)
                            rbnf_tmp_2_ = lcl_7
                            lcl_5 = rbnf_tmp_2_
                        lcl_4 = lcl_5
                    else:
                        lcl_4 = None
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 15):
                lcl_3 = rbnf_named_parse_ParamList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_4:
                        lcl_6 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_6 = lcl_6.idint
                        if (lcl_6 == 19):
                            _rbnf_old_offset = builtin_tokens.offset
                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 18):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_1, rbnf_tmp_2)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Params', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_5 = lcl_7
                        else:
                            lcl_7 = ()
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = (rbnf_tmp_0, rbnf_tmp_1_)
                            lcl_7 = builtin_mk_ast('Params', lcl_7)
                            rbnf_tmp_2_ = lcl_7
                            lcl_5 = rbnf_tmp_2_
                        lcl_4 = lcl_5
                    else:
                        lcl_4 = None
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 32):
                lcl_3 = rbnf_named_parse_ParamList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_4:
                        lcl_6 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_6 = lcl_6.idint
                        if (lcl_6 == 19):
                            _rbnf_old_offset = builtin_tokens.offset
                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 18):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_1, rbnf_tmp_2)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Params', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_5 = lcl_7
                        else:
                            lcl_7 = ()
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = (rbnf_tmp_0, rbnf_tmp_1_)
                            lcl_7 = builtin_mk_ast('Params', lcl_7)
                            rbnf_tmp_2_ = lcl_7
                            lcl_5 = rbnf_tmp_2_
                        lcl_4 = lcl_5
                    else:
                        lcl_4 = None
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 24):
                lcl_3 = rbnf_named_parse_ParamList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_4:
                        lcl_6 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_6 = lcl_6.idint
                        if (lcl_6 == 19):
                            _rbnf_old_offset = builtin_tokens.offset
                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 18):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_1, rbnf_tmp_2)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Params', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_5 = lcl_7
                        else:
                            lcl_7 = ()
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = (rbnf_tmp_0, rbnf_tmp_1_)
                            lcl_7 = builtin_mk_ast('Params', lcl_7)
                            rbnf_tmp_2_ = lcl_7
                            lcl_5 = rbnf_tmp_2_
                        lcl_4 = lcl_5
                    else:
                        lcl_4 = None
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 27):
                lcl_3 = rbnf_named_parse_ParamList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_4:
                        lcl_6 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_6 = lcl_6.idint
                        if (lcl_6 == 19):
                            _rbnf_old_offset = builtin_tokens.offset
                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 18):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_1, rbnf_tmp_2)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Params', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_5 = lcl_7
                        else:
                            lcl_7 = ()
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = (rbnf_tmp_0, rbnf_tmp_1_)
                            lcl_7 = builtin_mk_ast('Params', lcl_7)
                            rbnf_tmp_2_ = lcl_7
                            lcl_5 = rbnf_tmp_2_
                        lcl_4 = lcl_5
                    else:
                        lcl_4 = None
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 26):
                lcl_3 = rbnf_named_parse_ParamList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_4:
                        lcl_6 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_6 = lcl_6.idint
                        if (lcl_6 == 19):
                            _rbnf_old_offset = builtin_tokens.offset
                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 18):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_1, rbnf_tmp_2)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Params', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_5 = lcl_7
                        else:
                            lcl_7 = ()
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = (rbnf_tmp_0, rbnf_tmp_1_)
                            lcl_7 = builtin_mk_ast('Params', lcl_7)
                            rbnf_tmp_2_ = lcl_7
                            lcl_5 = rbnf_tmp_2_
                        lcl_4 = lcl_5
                    else:
                        lcl_4 = None
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 25):
                lcl_3 = rbnf_named_parse_ParamList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_4:
                        lcl_6 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_6 = lcl_6.idint
                        if (lcl_6 == 19):
                            _rbnf_old_offset = builtin_tokens.offset
                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 18):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_1, rbnf_tmp_2)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Params', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_5 = lcl_7
                        else:
                            lcl_7 = ()
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = (rbnf_tmp_0, rbnf_tmp_1_)
                            lcl_7 = builtin_mk_ast('Params', lcl_7)
                            rbnf_tmp_2_ = lcl_7
                            lcl_5 = rbnf_tmp_2_
                        lcl_4 = lcl_5
                    else:
                        lcl_4 = None
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 23):
                lcl_3 = rbnf_named_parse_ParamList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_4:
                        lcl_6 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_6 = lcl_6.idint
                        if (lcl_6 == 19):
                            _rbnf_old_offset = builtin_tokens.offset
                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 18):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_1, rbnf_tmp_2)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Params', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_5 = lcl_7
                        else:
                            lcl_7 = ()
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = (rbnf_tmp_0, rbnf_tmp_1_)
                            lcl_7 = builtin_mk_ast('Params', lcl_7)
                            rbnf_tmp_2_ = lcl_7
                            lcl_5 = rbnf_tmp_2_
                        lcl_4 = lcl_5
                    else:
                        lcl_4 = None
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 29):
                lcl_3 = rbnf_named_parse_ParamList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_4:
                        lcl_6 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_6 = lcl_6.idint
                        if (lcl_6 == 19):
                            _rbnf_old_offset = builtin_tokens.offset
                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 18):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_1, rbnf_tmp_2)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Params', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_5 = lcl_7
                        else:
                            lcl_7 = ()
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = (rbnf_tmp_0, rbnf_tmp_1_)
                            lcl_7 = builtin_mk_ast('Params', lcl_7)
                            rbnf_tmp_2_ = lcl_7
                            lcl_5 = rbnf_tmp_2_
                        lcl_4 = lcl_5
                    else:
                        lcl_4 = None
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 28):
                lcl_3 = rbnf_named_parse_ParamList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_4:
                        lcl_6 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_6 = lcl_6.idint
                        if (lcl_6 == 19):
                            _rbnf_old_offset = builtin_tokens.offset
                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 18):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_1, rbnf_tmp_2)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Params', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_5 = lcl_7
                        else:
                            lcl_7 = ()
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = (rbnf_tmp_0, rbnf_tmp_1_)
                            lcl_7 = builtin_mk_ast('Params', lcl_7)
                            rbnf_tmp_2_ = lcl_7
                            lcl_5 = rbnf_tmp_2_
                        lcl_4 = lcl_5
                    else:
                        lcl_4 = None
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 33):
                lcl_3 = rbnf_named_parse_ParamList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_4:
                        lcl_6 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_6 = lcl_6.idint
                        if (lcl_6 == 19):
                            _rbnf_old_offset = builtin_tokens.offset
                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 18):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_1, rbnf_tmp_2)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Params', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_5 = lcl_7
                        else:
                            lcl_7 = ()
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = (rbnf_tmp_0, rbnf_tmp_1_)
                            lcl_7 = builtin_mk_ast('Params', lcl_7)
                            rbnf_tmp_2_ = lcl_7
                            lcl_5 = rbnf_tmp_2_
                        lcl_4 = lcl_5
                    else:
                        lcl_4 = None
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 18):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('Params', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 4):
                lcl_3 = rbnf_named_parse_ParamList(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_1 = lcl_4
                    lcl_4 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_4:
                        lcl_6 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_6 = lcl_6.idint
                        if (lcl_6 == 19):
                            _rbnf_old_offset = builtin_tokens.offset
                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_1 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 18):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_1, rbnf_tmp_2)
                                rbnf_tmp_1_ = lcl_8
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_)
                                lcl_8 = builtin_mk_ast('Params', lcl_8)
                                rbnf_tmp_2_ = lcl_8
                                lcl_7 = rbnf_tmp_2_
                            lcl_5 = lcl_7
                        else:
                            lcl_7 = ()
                            rbnf_tmp_1_ = lcl_7
                            lcl_7 = (rbnf_tmp_0, rbnf_tmp_1_)
                            lcl_7 = builtin_mk_ast('Params', lcl_7)
                            rbnf_tmp_2_ = lcl_7
                            lcl_5 = rbnf_tmp_2_
                        lcl_4 = lcl_5
                    else:
                        lcl_4 = None
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_PhiInst(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 86):
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
            lcl_1 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                lcl_2 = rbnf_named_parse_IncList(builtin_state, builtin_tokens)
                rbnf_named__check_2 = lcl_2
                lcl_2 = (rbnf_named__check_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    rbnf_tmp_2 = rbnf_named__check_2
                    lcl_3 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                    lcl_3 = builtin_mk_ast('PhiInst', lcl_3)
                    rbnf_tmp_1_ = lcl_3
                    lcl_2 = rbnf_tmp_1_
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_PreemptionSpecifier(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 124):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('PreemptionSpecifier', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 123):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('PreemptionSpecifier', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_RetTerm(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 79):
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
            lcl_1 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                lcl_2 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_2
                lcl_2 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                if lcl_2:
                    lcl_4 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                    lcl_4 = lcl_4.idint
                    if (lcl_4 == 36):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 45):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 67):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 59):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 46):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 57):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 38):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 53):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 60):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 62):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 58):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 66):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 40):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 41):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 55):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 63):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 69):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 70):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 54):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 61):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 56):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 39):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 52):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 68):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 44):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 47):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 73):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 64):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 65):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 51):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 33):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 42):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 4):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 2):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 5):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    elif (lcl_4 == 10):
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('RetTerm', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_3 = lcl_5
                    else:
                        lcl_5 = ()
                        rbnf_tmp_1_ = lcl_5
                        lcl_5 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_1_)
                        lcl_5 = builtin_mk_ast('RetTerm', lcl_5)
                        rbnf_tmp_2_ = lcl_5
                        lcl_3 = rbnf_tmp_2_
                    lcl_2 = lcl_3
                else:
                    lcl_2 = None
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_START(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 0):
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
            lcl_1 = rbnf_named_parse_TopLevelEntityList(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 1):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_2 = _rbnf_cur_token
                rbnf_tmp_2 = lcl_2
                lcl_2 = (rbnf_tmp_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    lcl_3 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                    lcl_3 = builtin_mk_ast('START', lcl_3)
                    rbnf_tmp_1_ = lcl_3
                    lcl_2 = rbnf_tmp_1_
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_SelectInst(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 87):
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
            lcl_1 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 19):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_2 = _rbnf_cur_token
                rbnf_tmp_2 = lcl_2
                lcl_2 = (rbnf_tmp_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    lcl_3 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                    rbnf_named__check_3 = lcl_3
                    lcl_3 = (rbnf_named__check_3 is None)
                    if lcl_3:
                        lcl_3 = None
                    else:
                        rbnf_tmp_3 = rbnf_named__check_3
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 19):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_4 = _rbnf_cur_token
                        rbnf_tmp_4 = lcl_4
                        lcl_4 = (rbnf_tmp_4 is None)
                        if lcl_4:
                            lcl_4 = None
                        else:
                            lcl_5 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
                            rbnf_named__check_5 = lcl_5
                            lcl_5 = (rbnf_named__check_5 is None)
                            if lcl_5:
                                lcl_5 = None
                            else:
                                rbnf_tmp_5 = rbnf_named__check_5
                                lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5)
                                lcl_6 = builtin_mk_ast('SelectInst', lcl_6)
                                rbnf_tmp_1_ = lcl_6
                                lcl_5 = rbnf_tmp_1_
                            lcl_4 = lcl_5
                        lcl_3 = lcl_4
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_SepTypeValueList(builtin_state, builtin_tokens):
        lcl_0 = ()
        rbnf_tmp_1_ = lcl_0
        lcl_0 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_1_, rbnf_tmp_0)
            lcl_1 = builtin_mk_ast('SepTypeValueList', lcl_1)
            rbnf_tmp_2_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_SepTypeValueList(rbnf_tmp_2_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_SourceFilename(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 11):
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
            try:
                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                if (_rbnf_cur_token.idint is 12):
                    builtin_tokens.offset += 1
                else:
                    _rbnf_cur_token = None
            except IndexError:
                _rbnf_cur_token = None
            lcl_1 = _rbnf_cur_token
            rbnf_tmp_1 = lcl_1
            lcl_1 = (rbnf_tmp_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                lcl_2 = rbnf_named_parse_StrLit(builtin_state, builtin_tokens)
                rbnf_named__check_2 = lcl_2
                lcl_2 = (rbnf_named__check_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    rbnf_tmp_2 = rbnf_named__check_2
                    lcl_3 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                    lcl_3 = builtin_mk_ast('SourceFilename', lcl_3)
                    rbnf_tmp_1_ = lcl_3
                    lcl_2 = rbnf_tmp_1_
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_StoreInst(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 77):
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
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_1 = lcl_1
            lcl_1 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
            if lcl_1:
                lcl_3 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                lcl_3 = lcl_3.idint
                if (lcl_3 == 36):
                    lcl_4 = ()
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_5 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_5
                        lcl_5 = (rbnf_named__check_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 19):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_6 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_6
                            lcl_6 = (rbnf_tmp_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                lcl_7 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                rbnf_named__check_4 = lcl_7
                                lcl_7 = (rbnf_named__check_4 is None)
                                if lcl_7:
                                    lcl_7 = None
                                else:
                                    rbnf_tmp_4 = rbnf_named__check_4
                                    lcl_8 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                                    rbnf_named__check_5 = lcl_8
                                    lcl_8 = (rbnf_named__check_5 is None)
                                    if lcl_8:
                                        lcl_8 = None
                                    else:
                                        rbnf_tmp_5 = rbnf_named__check_5
                                        lcl_9 = builtin_tokens.offset
                                        rbnf_named__off_3 = lcl_9
                                        lcl_9 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                        if lcl_9:
                                            lcl_11 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                            lcl_11 = lcl_11.idint
                                            if (lcl_11 == 19):
                                                _rbnf_old_offset = builtin_tokens.offset
                                                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                                builtin_tokens.offset = (_rbnf_old_offset + 1)
                                                lcl_12 = _rbnf_cur_token
                                                rbnf_tmp_6 = lcl_12
                                                lcl_12 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                                rbnf_named__check_7 = lcl_12
                                                lcl_12 = (rbnf_named__check_7 is None)
                                                if lcl_12:
                                                    lcl_12 = None
                                                else:
                                                    rbnf_tmp_7 = rbnf_named__check_7
                                                    lcl_13 = (rbnf_tmp_6, rbnf_tmp_7)
                                                    rbnf_tmp_2_ = lcl_13
                                                    lcl_13 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                    lcl_13 = builtin_mk_ast('StoreInst', lcl_13)
                                                    rbnf_tmp_3_ = lcl_13
                                                    lcl_12 = rbnf_tmp_3_
                                                lcl_10 = lcl_12
                                            else:
                                                lcl_12 = ()
                                                rbnf_tmp_2_ = lcl_12
                                                lcl_12 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                lcl_12 = builtin_mk_ast('StoreInst', lcl_12)
                                                rbnf_tmp_3_ = lcl_12
                                                lcl_10 = rbnf_tmp_3_
                                            lcl_9 = lcl_10
                                        else:
                                            lcl_9 = None
                                        lcl_8 = lcl_9
                                    lcl_7 = lcl_8
                                lcl_6 = lcl_7
                            lcl_5 = lcl_6
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 78):
                    _rbnf_old_offset = builtin_tokens.offset
                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                    lcl_10 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_2 = lcl_10
                    lcl_10 = (rbnf_named__check_2 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_2 = rbnf_named__check_2
                        lcl_11 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_3 = lcl_11
                        lcl_11 = (rbnf_named__check_3 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            rbnf_tmp_3 = rbnf_named__check_3
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 19):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_12 = _rbnf_cur_token
                            rbnf_tmp_4 = lcl_12
                            lcl_12 = (rbnf_tmp_4 is None)
                            if lcl_12:
                                lcl_12 = None
                            else:
                                lcl_13 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                rbnf_named__check_5 = lcl_13
                                lcl_13 = (rbnf_named__check_5 is None)
                                if lcl_13:
                                    lcl_13 = None
                                else:
                                    rbnf_tmp_5 = rbnf_named__check_5
                                    lcl_4 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                                    rbnf_named__check_6 = lcl_4
                                    lcl_4 = (rbnf_named__check_6 is None)
                                    if lcl_4:
                                        lcl_4 = None
                                    else:
                                        rbnf_tmp_6 = rbnf_named__check_6
                                        lcl_5 = builtin_tokens.offset
                                        rbnf_named__off_3 = lcl_5
                                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                        if lcl_5:
                                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                            lcl_7 = lcl_7.idint
                                            if (lcl_7 == 19):
                                                _rbnf_old_offset = builtin_tokens.offset
                                                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                                builtin_tokens.offset = (_rbnf_old_offset + 1)
                                                lcl_8 = _rbnf_cur_token
                                                rbnf_tmp_7 = lcl_8
                                                lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                                rbnf_named__check_8 = lcl_8
                                                lcl_8 = (rbnf_named__check_8 is None)
                                                if lcl_8:
                                                    lcl_8 = None
                                                else:
                                                    rbnf_tmp_8 = rbnf_named__check_8
                                                    lcl_9 = (rbnf_tmp_7, rbnf_tmp_8)
                                                    rbnf_tmp_1_ = lcl_9
                                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_6, rbnf_tmp_1_)
                                                    lcl_9 = builtin_mk_ast('StoreInst', lcl_9)
                                                    rbnf_tmp_2_ = lcl_9
                                                    lcl_8 = rbnf_tmp_2_
                                                lcl_6 = lcl_8
                                            else:
                                                lcl_8 = ()
                                                rbnf_tmp_1_ = lcl_8
                                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_6, rbnf_tmp_1_)
                                                lcl_8 = builtin_mk_ast('StoreInst', lcl_8)
                                                rbnf_tmp_2_ = lcl_8
                                                lcl_6 = rbnf_tmp_2_
                                            lcl_5 = lcl_6
                                        else:
                                            lcl_5 = None
                                        lcl_4 = lcl_5
                                    lcl_13 = lcl_4
                                lcl_12 = lcl_13
                            lcl_11 = lcl_12
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 15):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_11 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_11
                        lcl_11 = (rbnf_named__check_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 19):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_12 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_12
                            lcl_12 = (rbnf_tmp_3 is None)
                            if lcl_12:
                                lcl_12 = None
                            else:
                                lcl_13 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                rbnf_named__check_4 = lcl_13
                                lcl_13 = (rbnf_named__check_4 is None)
                                if lcl_13:
                                    lcl_13 = None
                                else:
                                    rbnf_tmp_4 = rbnf_named__check_4
                                    lcl_4 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                                    rbnf_named__check_5 = lcl_4
                                    lcl_4 = (rbnf_named__check_5 is None)
                                    if lcl_4:
                                        lcl_4 = None
                                    else:
                                        rbnf_tmp_5 = rbnf_named__check_5
                                        lcl_5 = builtin_tokens.offset
                                        rbnf_named__off_3 = lcl_5
                                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                        if lcl_5:
                                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                            lcl_7 = lcl_7.idint
                                            if (lcl_7 == 19):
                                                _rbnf_old_offset = builtin_tokens.offset
                                                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                                builtin_tokens.offset = (_rbnf_old_offset + 1)
                                                lcl_8 = _rbnf_cur_token
                                                rbnf_tmp_6 = lcl_8
                                                lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                                rbnf_named__check_7 = lcl_8
                                                lcl_8 = (rbnf_named__check_7 is None)
                                                if lcl_8:
                                                    lcl_8 = None
                                                else:
                                                    rbnf_tmp_7 = rbnf_named__check_7
                                                    lcl_9 = (rbnf_tmp_6, rbnf_tmp_7)
                                                    rbnf_tmp_2_ = lcl_9
                                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                    lcl_9 = builtin_mk_ast('StoreInst', lcl_9)
                                                    rbnf_tmp_3_ = lcl_9
                                                    lcl_8 = rbnf_tmp_3_
                                                lcl_6 = lcl_8
                                            else:
                                                lcl_8 = ()
                                                rbnf_tmp_2_ = lcl_8
                                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                lcl_8 = builtin_mk_ast('StoreInst', lcl_8)
                                                rbnf_tmp_3_ = lcl_8
                                                lcl_6 = rbnf_tmp_3_
                                            lcl_5 = lcl_6
                                        else:
                                            lcl_5 = None
                                        lcl_4 = lcl_5
                                    lcl_13 = lcl_4
                                lcl_12 = lcl_13
                            lcl_11 = lcl_12
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 32):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_11 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_11
                        lcl_11 = (rbnf_named__check_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 19):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_12 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_12
                            lcl_12 = (rbnf_tmp_3 is None)
                            if lcl_12:
                                lcl_12 = None
                            else:
                                lcl_13 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                rbnf_named__check_4 = lcl_13
                                lcl_13 = (rbnf_named__check_4 is None)
                                if lcl_13:
                                    lcl_13 = None
                                else:
                                    rbnf_tmp_4 = rbnf_named__check_4
                                    lcl_4 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                                    rbnf_named__check_5 = lcl_4
                                    lcl_4 = (rbnf_named__check_5 is None)
                                    if lcl_4:
                                        lcl_4 = None
                                    else:
                                        rbnf_tmp_5 = rbnf_named__check_5
                                        lcl_5 = builtin_tokens.offset
                                        rbnf_named__off_3 = lcl_5
                                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                        if lcl_5:
                                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                            lcl_7 = lcl_7.idint
                                            if (lcl_7 == 19):
                                                _rbnf_old_offset = builtin_tokens.offset
                                                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                                builtin_tokens.offset = (_rbnf_old_offset + 1)
                                                lcl_8 = _rbnf_cur_token
                                                rbnf_tmp_6 = lcl_8
                                                lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                                rbnf_named__check_7 = lcl_8
                                                lcl_8 = (rbnf_named__check_7 is None)
                                                if lcl_8:
                                                    lcl_8 = None
                                                else:
                                                    rbnf_tmp_7 = rbnf_named__check_7
                                                    lcl_9 = (rbnf_tmp_6, rbnf_tmp_7)
                                                    rbnf_tmp_2_ = lcl_9
                                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                    lcl_9 = builtin_mk_ast('StoreInst', lcl_9)
                                                    rbnf_tmp_3_ = lcl_9
                                                    lcl_8 = rbnf_tmp_3_
                                                lcl_6 = lcl_8
                                            else:
                                                lcl_8 = ()
                                                rbnf_tmp_2_ = lcl_8
                                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                lcl_8 = builtin_mk_ast('StoreInst', lcl_8)
                                                rbnf_tmp_3_ = lcl_8
                                                lcl_6 = rbnf_tmp_3_
                                            lcl_5 = lcl_6
                                        else:
                                            lcl_5 = None
                                        lcl_4 = lcl_5
                                    lcl_13 = lcl_4
                                lcl_12 = lcl_13
                            lcl_11 = lcl_12
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 24):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_11 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_11
                        lcl_11 = (rbnf_named__check_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 19):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_12 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_12
                            lcl_12 = (rbnf_tmp_3 is None)
                            if lcl_12:
                                lcl_12 = None
                            else:
                                lcl_13 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                rbnf_named__check_4 = lcl_13
                                lcl_13 = (rbnf_named__check_4 is None)
                                if lcl_13:
                                    lcl_13 = None
                                else:
                                    rbnf_tmp_4 = rbnf_named__check_4
                                    lcl_4 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                                    rbnf_named__check_5 = lcl_4
                                    lcl_4 = (rbnf_named__check_5 is None)
                                    if lcl_4:
                                        lcl_4 = None
                                    else:
                                        rbnf_tmp_5 = rbnf_named__check_5
                                        lcl_5 = builtin_tokens.offset
                                        rbnf_named__off_3 = lcl_5
                                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                        if lcl_5:
                                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                            lcl_7 = lcl_7.idint
                                            if (lcl_7 == 19):
                                                _rbnf_old_offset = builtin_tokens.offset
                                                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                                builtin_tokens.offset = (_rbnf_old_offset + 1)
                                                lcl_8 = _rbnf_cur_token
                                                rbnf_tmp_6 = lcl_8
                                                lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                                rbnf_named__check_7 = lcl_8
                                                lcl_8 = (rbnf_named__check_7 is None)
                                                if lcl_8:
                                                    lcl_8 = None
                                                else:
                                                    rbnf_tmp_7 = rbnf_named__check_7
                                                    lcl_9 = (rbnf_tmp_6, rbnf_tmp_7)
                                                    rbnf_tmp_2_ = lcl_9
                                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                    lcl_9 = builtin_mk_ast('StoreInst', lcl_9)
                                                    rbnf_tmp_3_ = lcl_9
                                                    lcl_8 = rbnf_tmp_3_
                                                lcl_6 = lcl_8
                                            else:
                                                lcl_8 = ()
                                                rbnf_tmp_2_ = lcl_8
                                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                lcl_8 = builtin_mk_ast('StoreInst', lcl_8)
                                                rbnf_tmp_3_ = lcl_8
                                                lcl_6 = rbnf_tmp_3_
                                            lcl_5 = lcl_6
                                        else:
                                            lcl_5 = None
                                        lcl_4 = lcl_5
                                    lcl_13 = lcl_4
                                lcl_12 = lcl_13
                            lcl_11 = lcl_12
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 27):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_11 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_11
                        lcl_11 = (rbnf_named__check_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 19):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_12 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_12
                            lcl_12 = (rbnf_tmp_3 is None)
                            if lcl_12:
                                lcl_12 = None
                            else:
                                lcl_13 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                rbnf_named__check_4 = lcl_13
                                lcl_13 = (rbnf_named__check_4 is None)
                                if lcl_13:
                                    lcl_13 = None
                                else:
                                    rbnf_tmp_4 = rbnf_named__check_4
                                    lcl_4 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                                    rbnf_named__check_5 = lcl_4
                                    lcl_4 = (rbnf_named__check_5 is None)
                                    if lcl_4:
                                        lcl_4 = None
                                    else:
                                        rbnf_tmp_5 = rbnf_named__check_5
                                        lcl_5 = builtin_tokens.offset
                                        rbnf_named__off_3 = lcl_5
                                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                        if lcl_5:
                                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                            lcl_7 = lcl_7.idint
                                            if (lcl_7 == 19):
                                                _rbnf_old_offset = builtin_tokens.offset
                                                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                                builtin_tokens.offset = (_rbnf_old_offset + 1)
                                                lcl_8 = _rbnf_cur_token
                                                rbnf_tmp_6 = lcl_8
                                                lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                                rbnf_named__check_7 = lcl_8
                                                lcl_8 = (rbnf_named__check_7 is None)
                                                if lcl_8:
                                                    lcl_8 = None
                                                else:
                                                    rbnf_tmp_7 = rbnf_named__check_7
                                                    lcl_9 = (rbnf_tmp_6, rbnf_tmp_7)
                                                    rbnf_tmp_2_ = lcl_9
                                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                    lcl_9 = builtin_mk_ast('StoreInst', lcl_9)
                                                    rbnf_tmp_3_ = lcl_9
                                                    lcl_8 = rbnf_tmp_3_
                                                lcl_6 = lcl_8
                                            else:
                                                lcl_8 = ()
                                                rbnf_tmp_2_ = lcl_8
                                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                lcl_8 = builtin_mk_ast('StoreInst', lcl_8)
                                                rbnf_tmp_3_ = lcl_8
                                                lcl_6 = rbnf_tmp_3_
                                            lcl_5 = lcl_6
                                        else:
                                            lcl_5 = None
                                        lcl_4 = lcl_5
                                    lcl_13 = lcl_4
                                lcl_12 = lcl_13
                            lcl_11 = lcl_12
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 26):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_11 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_11
                        lcl_11 = (rbnf_named__check_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 19):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_12 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_12
                            lcl_12 = (rbnf_tmp_3 is None)
                            if lcl_12:
                                lcl_12 = None
                            else:
                                lcl_13 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                rbnf_named__check_4 = lcl_13
                                lcl_13 = (rbnf_named__check_4 is None)
                                if lcl_13:
                                    lcl_13 = None
                                else:
                                    rbnf_tmp_4 = rbnf_named__check_4
                                    lcl_4 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                                    rbnf_named__check_5 = lcl_4
                                    lcl_4 = (rbnf_named__check_5 is None)
                                    if lcl_4:
                                        lcl_4 = None
                                    else:
                                        rbnf_tmp_5 = rbnf_named__check_5
                                        lcl_5 = builtin_tokens.offset
                                        rbnf_named__off_3 = lcl_5
                                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                        if lcl_5:
                                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                            lcl_7 = lcl_7.idint
                                            if (lcl_7 == 19):
                                                _rbnf_old_offset = builtin_tokens.offset
                                                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                                builtin_tokens.offset = (_rbnf_old_offset + 1)
                                                lcl_8 = _rbnf_cur_token
                                                rbnf_tmp_6 = lcl_8
                                                lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                                rbnf_named__check_7 = lcl_8
                                                lcl_8 = (rbnf_named__check_7 is None)
                                                if lcl_8:
                                                    lcl_8 = None
                                                else:
                                                    rbnf_tmp_7 = rbnf_named__check_7
                                                    lcl_9 = (rbnf_tmp_6, rbnf_tmp_7)
                                                    rbnf_tmp_2_ = lcl_9
                                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                    lcl_9 = builtin_mk_ast('StoreInst', lcl_9)
                                                    rbnf_tmp_3_ = lcl_9
                                                    lcl_8 = rbnf_tmp_3_
                                                lcl_6 = lcl_8
                                            else:
                                                lcl_8 = ()
                                                rbnf_tmp_2_ = lcl_8
                                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                lcl_8 = builtin_mk_ast('StoreInst', lcl_8)
                                                rbnf_tmp_3_ = lcl_8
                                                lcl_6 = rbnf_tmp_3_
                                            lcl_5 = lcl_6
                                        else:
                                            lcl_5 = None
                                        lcl_4 = lcl_5
                                    lcl_13 = lcl_4
                                lcl_12 = lcl_13
                            lcl_11 = lcl_12
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 25):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_11 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_11
                        lcl_11 = (rbnf_named__check_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 19):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_12 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_12
                            lcl_12 = (rbnf_tmp_3 is None)
                            if lcl_12:
                                lcl_12 = None
                            else:
                                lcl_13 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                rbnf_named__check_4 = lcl_13
                                lcl_13 = (rbnf_named__check_4 is None)
                                if lcl_13:
                                    lcl_13 = None
                                else:
                                    rbnf_tmp_4 = rbnf_named__check_4
                                    lcl_4 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                                    rbnf_named__check_5 = lcl_4
                                    lcl_4 = (rbnf_named__check_5 is None)
                                    if lcl_4:
                                        lcl_4 = None
                                    else:
                                        rbnf_tmp_5 = rbnf_named__check_5
                                        lcl_5 = builtin_tokens.offset
                                        rbnf_named__off_3 = lcl_5
                                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                        if lcl_5:
                                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                            lcl_7 = lcl_7.idint
                                            if (lcl_7 == 19):
                                                _rbnf_old_offset = builtin_tokens.offset
                                                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                                builtin_tokens.offset = (_rbnf_old_offset + 1)
                                                lcl_8 = _rbnf_cur_token
                                                rbnf_tmp_6 = lcl_8
                                                lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                                rbnf_named__check_7 = lcl_8
                                                lcl_8 = (rbnf_named__check_7 is None)
                                                if lcl_8:
                                                    lcl_8 = None
                                                else:
                                                    rbnf_tmp_7 = rbnf_named__check_7
                                                    lcl_9 = (rbnf_tmp_6, rbnf_tmp_7)
                                                    rbnf_tmp_2_ = lcl_9
                                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                    lcl_9 = builtin_mk_ast('StoreInst', lcl_9)
                                                    rbnf_tmp_3_ = lcl_9
                                                    lcl_8 = rbnf_tmp_3_
                                                lcl_6 = lcl_8
                                            else:
                                                lcl_8 = ()
                                                rbnf_tmp_2_ = lcl_8
                                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                lcl_8 = builtin_mk_ast('StoreInst', lcl_8)
                                                rbnf_tmp_3_ = lcl_8
                                                lcl_6 = rbnf_tmp_3_
                                            lcl_5 = lcl_6
                                        else:
                                            lcl_5 = None
                                        lcl_4 = lcl_5
                                    lcl_13 = lcl_4
                                lcl_12 = lcl_13
                            lcl_11 = lcl_12
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 23):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_11 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_11
                        lcl_11 = (rbnf_named__check_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 19):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_12 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_12
                            lcl_12 = (rbnf_tmp_3 is None)
                            if lcl_12:
                                lcl_12 = None
                            else:
                                lcl_13 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                rbnf_named__check_4 = lcl_13
                                lcl_13 = (rbnf_named__check_4 is None)
                                if lcl_13:
                                    lcl_13 = None
                                else:
                                    rbnf_tmp_4 = rbnf_named__check_4
                                    lcl_4 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                                    rbnf_named__check_5 = lcl_4
                                    lcl_4 = (rbnf_named__check_5 is None)
                                    if lcl_4:
                                        lcl_4 = None
                                    else:
                                        rbnf_tmp_5 = rbnf_named__check_5
                                        lcl_5 = builtin_tokens.offset
                                        rbnf_named__off_3 = lcl_5
                                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                        if lcl_5:
                                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                            lcl_7 = lcl_7.idint
                                            if (lcl_7 == 19):
                                                _rbnf_old_offset = builtin_tokens.offset
                                                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                                builtin_tokens.offset = (_rbnf_old_offset + 1)
                                                lcl_8 = _rbnf_cur_token
                                                rbnf_tmp_6 = lcl_8
                                                lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                                rbnf_named__check_7 = lcl_8
                                                lcl_8 = (rbnf_named__check_7 is None)
                                                if lcl_8:
                                                    lcl_8 = None
                                                else:
                                                    rbnf_tmp_7 = rbnf_named__check_7
                                                    lcl_9 = (rbnf_tmp_6, rbnf_tmp_7)
                                                    rbnf_tmp_2_ = lcl_9
                                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                    lcl_9 = builtin_mk_ast('StoreInst', lcl_9)
                                                    rbnf_tmp_3_ = lcl_9
                                                    lcl_8 = rbnf_tmp_3_
                                                lcl_6 = lcl_8
                                            else:
                                                lcl_8 = ()
                                                rbnf_tmp_2_ = lcl_8
                                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                lcl_8 = builtin_mk_ast('StoreInst', lcl_8)
                                                rbnf_tmp_3_ = lcl_8
                                                lcl_6 = rbnf_tmp_3_
                                            lcl_5 = lcl_6
                                        else:
                                            lcl_5 = None
                                        lcl_4 = lcl_5
                                    lcl_13 = lcl_4
                                lcl_12 = lcl_13
                            lcl_11 = lcl_12
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 29):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_11 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_11
                        lcl_11 = (rbnf_named__check_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 19):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_12 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_12
                            lcl_12 = (rbnf_tmp_3 is None)
                            if lcl_12:
                                lcl_12 = None
                            else:
                                lcl_13 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                rbnf_named__check_4 = lcl_13
                                lcl_13 = (rbnf_named__check_4 is None)
                                if lcl_13:
                                    lcl_13 = None
                                else:
                                    rbnf_tmp_4 = rbnf_named__check_4
                                    lcl_4 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                                    rbnf_named__check_5 = lcl_4
                                    lcl_4 = (rbnf_named__check_5 is None)
                                    if lcl_4:
                                        lcl_4 = None
                                    else:
                                        rbnf_tmp_5 = rbnf_named__check_5
                                        lcl_5 = builtin_tokens.offset
                                        rbnf_named__off_3 = lcl_5
                                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                        if lcl_5:
                                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                            lcl_7 = lcl_7.idint
                                            if (lcl_7 == 19):
                                                _rbnf_old_offset = builtin_tokens.offset
                                                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                                builtin_tokens.offset = (_rbnf_old_offset + 1)
                                                lcl_8 = _rbnf_cur_token
                                                rbnf_tmp_6 = lcl_8
                                                lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                                rbnf_named__check_7 = lcl_8
                                                lcl_8 = (rbnf_named__check_7 is None)
                                                if lcl_8:
                                                    lcl_8 = None
                                                else:
                                                    rbnf_tmp_7 = rbnf_named__check_7
                                                    lcl_9 = (rbnf_tmp_6, rbnf_tmp_7)
                                                    rbnf_tmp_2_ = lcl_9
                                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                    lcl_9 = builtin_mk_ast('StoreInst', lcl_9)
                                                    rbnf_tmp_3_ = lcl_9
                                                    lcl_8 = rbnf_tmp_3_
                                                lcl_6 = lcl_8
                                            else:
                                                lcl_8 = ()
                                                rbnf_tmp_2_ = lcl_8
                                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                lcl_8 = builtin_mk_ast('StoreInst', lcl_8)
                                                rbnf_tmp_3_ = lcl_8
                                                lcl_6 = rbnf_tmp_3_
                                            lcl_5 = lcl_6
                                        else:
                                            lcl_5 = None
                                        lcl_4 = lcl_5
                                    lcl_13 = lcl_4
                                lcl_12 = lcl_13
                            lcl_11 = lcl_12
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 28):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_11 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_11
                        lcl_11 = (rbnf_named__check_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 19):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_12 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_12
                            lcl_12 = (rbnf_tmp_3 is None)
                            if lcl_12:
                                lcl_12 = None
                            else:
                                lcl_13 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                rbnf_named__check_4 = lcl_13
                                lcl_13 = (rbnf_named__check_4 is None)
                                if lcl_13:
                                    lcl_13 = None
                                else:
                                    rbnf_tmp_4 = rbnf_named__check_4
                                    lcl_4 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                                    rbnf_named__check_5 = lcl_4
                                    lcl_4 = (rbnf_named__check_5 is None)
                                    if lcl_4:
                                        lcl_4 = None
                                    else:
                                        rbnf_tmp_5 = rbnf_named__check_5
                                        lcl_5 = builtin_tokens.offset
                                        rbnf_named__off_3 = lcl_5
                                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                        if lcl_5:
                                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                            lcl_7 = lcl_7.idint
                                            if (lcl_7 == 19):
                                                _rbnf_old_offset = builtin_tokens.offset
                                                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                                builtin_tokens.offset = (_rbnf_old_offset + 1)
                                                lcl_8 = _rbnf_cur_token
                                                rbnf_tmp_6 = lcl_8
                                                lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                                rbnf_named__check_7 = lcl_8
                                                lcl_8 = (rbnf_named__check_7 is None)
                                                if lcl_8:
                                                    lcl_8 = None
                                                else:
                                                    rbnf_tmp_7 = rbnf_named__check_7
                                                    lcl_9 = (rbnf_tmp_6, rbnf_tmp_7)
                                                    rbnf_tmp_2_ = lcl_9
                                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                    lcl_9 = builtin_mk_ast('StoreInst', lcl_9)
                                                    rbnf_tmp_3_ = lcl_9
                                                    lcl_8 = rbnf_tmp_3_
                                                lcl_6 = lcl_8
                                            else:
                                                lcl_8 = ()
                                                rbnf_tmp_2_ = lcl_8
                                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                lcl_8 = builtin_mk_ast('StoreInst', lcl_8)
                                                rbnf_tmp_3_ = lcl_8
                                                lcl_6 = rbnf_tmp_3_
                                            lcl_5 = lcl_6
                                        else:
                                            lcl_5 = None
                                        lcl_4 = lcl_5
                                    lcl_13 = lcl_4
                                lcl_12 = lcl_13
                            lcl_11 = lcl_12
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 33):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_11 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_11
                        lcl_11 = (rbnf_named__check_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 19):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_12 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_12
                            lcl_12 = (rbnf_tmp_3 is None)
                            if lcl_12:
                                lcl_12 = None
                            else:
                                lcl_13 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                rbnf_named__check_4 = lcl_13
                                lcl_13 = (rbnf_named__check_4 is None)
                                if lcl_13:
                                    lcl_13 = None
                                else:
                                    rbnf_tmp_4 = rbnf_named__check_4
                                    lcl_4 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                                    rbnf_named__check_5 = lcl_4
                                    lcl_4 = (rbnf_named__check_5 is None)
                                    if lcl_4:
                                        lcl_4 = None
                                    else:
                                        rbnf_tmp_5 = rbnf_named__check_5
                                        lcl_5 = builtin_tokens.offset
                                        rbnf_named__off_3 = lcl_5
                                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                        if lcl_5:
                                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                            lcl_7 = lcl_7.idint
                                            if (lcl_7 == 19):
                                                _rbnf_old_offset = builtin_tokens.offset
                                                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                                builtin_tokens.offset = (_rbnf_old_offset + 1)
                                                lcl_8 = _rbnf_cur_token
                                                rbnf_tmp_6 = lcl_8
                                                lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                                rbnf_named__check_7 = lcl_8
                                                lcl_8 = (rbnf_named__check_7 is None)
                                                if lcl_8:
                                                    lcl_8 = None
                                                else:
                                                    rbnf_tmp_7 = rbnf_named__check_7
                                                    lcl_9 = (rbnf_tmp_6, rbnf_tmp_7)
                                                    rbnf_tmp_2_ = lcl_9
                                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                    lcl_9 = builtin_mk_ast('StoreInst', lcl_9)
                                                    rbnf_tmp_3_ = lcl_9
                                                    lcl_8 = rbnf_tmp_3_
                                                lcl_6 = lcl_8
                                            else:
                                                lcl_8 = ()
                                                rbnf_tmp_2_ = lcl_8
                                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                lcl_8 = builtin_mk_ast('StoreInst', lcl_8)
                                                rbnf_tmp_3_ = lcl_8
                                                lcl_6 = rbnf_tmp_3_
                                            lcl_5 = lcl_6
                                        else:
                                            lcl_5 = None
                                        lcl_4 = lcl_5
                                    lcl_13 = lcl_4
                                lcl_12 = lcl_13
                            lcl_11 = lcl_12
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                elif (lcl_3 == 4):
                    lcl_10 = ()
                    rbnf_tmp_1_ = lcl_10
                    lcl_10 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_10
                    lcl_10 = (rbnf_named__check_1 is None)
                    if lcl_10:
                        lcl_10 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        lcl_11 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                        rbnf_named__check_2 = lcl_11
                        lcl_11 = (rbnf_named__check_2 is None)
                        if lcl_11:
                            lcl_11 = None
                        else:
                            rbnf_tmp_2 = rbnf_named__check_2
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 19):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_12 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_12
                            lcl_12 = (rbnf_tmp_3 is None)
                            if lcl_12:
                                lcl_12 = None
                            else:
                                lcl_13 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                                rbnf_named__check_4 = lcl_13
                                lcl_13 = (rbnf_named__check_4 is None)
                                if lcl_13:
                                    lcl_13 = None
                                else:
                                    rbnf_tmp_4 = rbnf_named__check_4
                                    lcl_4 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
                                    rbnf_named__check_5 = lcl_4
                                    lcl_4 = (rbnf_named__check_5 is None)
                                    if lcl_4:
                                        lcl_4 = None
                                    else:
                                        rbnf_tmp_5 = rbnf_named__check_5
                                        lcl_5 = builtin_tokens.offset
                                        rbnf_named__off_3 = lcl_5
                                        lcl_5 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                        if lcl_5:
                                            lcl_7 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                            lcl_7 = lcl_7.idint
                                            if (lcl_7 == 19):
                                                _rbnf_old_offset = builtin_tokens.offset
                                                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                                builtin_tokens.offset = (_rbnf_old_offset + 1)
                                                lcl_8 = _rbnf_cur_token
                                                rbnf_tmp_6 = lcl_8
                                                lcl_8 = rbnf_named_parse_Alignment(builtin_state, builtin_tokens)
                                                rbnf_named__check_7 = lcl_8
                                                lcl_8 = (rbnf_named__check_7 is None)
                                                if lcl_8:
                                                    lcl_8 = None
                                                else:
                                                    rbnf_tmp_7 = rbnf_named__check_7
                                                    lcl_9 = (rbnf_tmp_6, rbnf_tmp_7)
                                                    rbnf_tmp_2_ = lcl_9
                                                    lcl_9 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                    lcl_9 = builtin_mk_ast('StoreInst', lcl_9)
                                                    rbnf_tmp_3_ = lcl_9
                                                    lcl_8 = rbnf_tmp_3_
                                                lcl_6 = lcl_8
                                            else:
                                                lcl_8 = ()
                                                rbnf_tmp_2_ = lcl_8
                                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1_, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_2_)
                                                lcl_8 = builtin_mk_ast('StoreInst', lcl_8)
                                                rbnf_tmp_3_ = lcl_8
                                                lcl_6 = rbnf_tmp_3_
                                            lcl_5 = lcl_6
                                        else:
                                            lcl_5 = None
                                        lcl_4 = lcl_5
                                    lcl_13 = lcl_4
                                lcl_12 = lcl_13
                            lcl_11 = lcl_12
                        lcl_10 = lcl_11
                    lcl_2 = lcl_10
                else:
                    lcl_2 = None
                lcl_1 = lcl_2
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_StrLit(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 8):
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
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('StrLit', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_StructConst(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 36):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = builtin_tokens.offset
                rbnf_named__off_1 = lcl_3
                lcl_3 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                if lcl_3:
                    lcl_5 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                    lcl_5 = lcl_5.idint
                    if (lcl_5 == 37):
                        _rbnf_old_offset = builtin_tokens.offset
                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                        lcl_6 = _rbnf_cur_token
                        rbnf_tmp_1 = lcl_6
                        lcl_6 = (rbnf_tmp_0, rbnf_tmp_1)
                        lcl_6 = builtin_mk_ast('StructConst', lcl_6)
                        rbnf_tmp_1_ = lcl_6
                        lcl_4 = rbnf_tmp_1_
                    elif (lcl_5 == 36):
                        lcl_6 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = (rbnf_named__check_1 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_1 = rbnf_named__check_1
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 37):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                lcl_8 = builtin_mk_ast('StructConst', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_7 = rbnf_tmp_1_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 15):
                        lcl_6 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = (rbnf_named__check_1 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_1 = rbnf_named__check_1
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 37):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                lcl_8 = builtin_mk_ast('StructConst', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_7 = rbnf_tmp_1_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 32):
                        lcl_6 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = (rbnf_named__check_1 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_1 = rbnf_named__check_1
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 37):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                lcl_8 = builtin_mk_ast('StructConst', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_7 = rbnf_tmp_1_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 24):
                        lcl_6 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = (rbnf_named__check_1 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_1 = rbnf_named__check_1
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 37):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                lcl_8 = builtin_mk_ast('StructConst', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_7 = rbnf_tmp_1_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 27):
                        lcl_6 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = (rbnf_named__check_1 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_1 = rbnf_named__check_1
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 37):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                lcl_8 = builtin_mk_ast('StructConst', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_7 = rbnf_tmp_1_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 26):
                        lcl_6 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = (rbnf_named__check_1 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_1 = rbnf_named__check_1
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 37):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                lcl_8 = builtin_mk_ast('StructConst', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_7 = rbnf_tmp_1_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 25):
                        lcl_6 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = (rbnf_named__check_1 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_1 = rbnf_named__check_1
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 37):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                lcl_8 = builtin_mk_ast('StructConst', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_7 = rbnf_tmp_1_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 23):
                        lcl_6 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = (rbnf_named__check_1 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_1 = rbnf_named__check_1
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 37):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                lcl_8 = builtin_mk_ast('StructConst', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_7 = rbnf_tmp_1_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 29):
                        lcl_6 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = (rbnf_named__check_1 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_1 = rbnf_named__check_1
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 37):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                lcl_8 = builtin_mk_ast('StructConst', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_7 = rbnf_tmp_1_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 28):
                        lcl_6 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = (rbnf_named__check_1 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_1 = rbnf_named__check_1
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 37):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                lcl_8 = builtin_mk_ast('StructConst', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_7 = rbnf_tmp_1_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 33):
                        lcl_6 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = (rbnf_named__check_1 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_1 = rbnf_named__check_1
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 37):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                lcl_8 = builtin_mk_ast('StructConst', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_7 = rbnf_tmp_1_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    elif (lcl_5 == 4):
                        lcl_6 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                        rbnf_named__check_1 = lcl_6
                        lcl_6 = (rbnf_named__check_1 is None)
                        if lcl_6:
                            lcl_6 = None
                        else:
                            rbnf_tmp_1 = rbnf_named__check_1
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 37):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            lcl_7 = (rbnf_tmp_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                                lcl_8 = builtin_mk_ast('StructConst', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_7 = rbnf_tmp_1_
                            lcl_6 = lcl_7
                        lcl_4 = lcl_6
                    else:
                        lcl_4 = None
                    lcl_3 = lcl_4
                else:
                    lcl_3 = None
                lcl_1 = lcl_3
            elif (lcl_2 == 42):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 36):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_1 = lcl_3
                lcl_3 = (rbnf_tmp_1 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    lcl_4 = builtin_tokens.offset
                    rbnf_named__off_2 = lcl_4
                    lcl_4 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_4:
                        lcl_6 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_6 = lcl_6.idint
                        if (lcl_6 == 37):
                            _rbnf_old_offset = builtin_tokens.offset
                            _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                            builtin_tokens.offset = (_rbnf_old_offset + 1)
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_2 = lcl_7
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 43):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_7 = _rbnf_cur_token
                            rbnf_tmp_3 = lcl_7
                            lcl_7 = (rbnf_tmp_3 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                lcl_8 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_8 = builtin_mk_ast('StructConst', lcl_8)
                                rbnf_tmp_1_ = lcl_8
                                lcl_7 = rbnf_tmp_1_
                            lcl_5 = lcl_7
                        elif (lcl_6 == 36):
                            lcl_7 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                            rbnf_named__check_2 = lcl_7
                            lcl_7 = (rbnf_named__check_2 is None)
                            if lcl_7:
                                lcl_7 = None
                            else:
                                rbnf_tmp_2 = rbnf_named__check_2
                                try:
                                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                    if (_rbnf_cur_token.idint is 37):
                                        builtin_tokens.offset += 1
                                    else:
                                        _rbnf_cur_token = None
                                except IndexError:
                                    _rbnf_cur_token = None
                                lcl_8 = _rbnf_cur_token
                                rbnf_tmp_3 = lcl_8
                                lcl_8 = (rbnf_tmp_3 is None)
                                if lcl_8:
                                    lcl_8 = None
                                else:
                                    try:
                                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                        if (_rbnf_cur_token.idint is 43):
                                            builtin_tokens.offset += 1
                                        else:
                                            _rbnf_cur_token = None
                                    except IndexError:
                                        _rbnf_cur_token = None
                                    lcl_9 = _rbnf_cur_token
                                    rbnf_tmp_4 = lcl_9
                                    lcl_9 = (rbnf_tmp_4 is None)
                                    if lcl_9:
                                        lcl_9 = None
                                    else:
                                        lcl_10 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                        lcl_10 = builtin_mk_ast('StructConst', lcl_10)
                                        rbnf_tmp_1_ = lcl_10
                                        lcl_9 = rbnf_tmp_1_
                                    lcl_8 = lcl_9
                                lcl_7 = lcl_8
                            lcl_5 = lcl_7
                        elif (lcl_6 == 15):
                            lcl_10 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                            rbnf_named__check_2 = lcl_10
                            lcl_10 = (rbnf_named__check_2 is None)
                            if lcl_10:
                                lcl_10 = None
                            else:
                                rbnf_tmp_2 = rbnf_named__check_2
                                try:
                                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                    if (_rbnf_cur_token.idint is 37):
                                        builtin_tokens.offset += 1
                                    else:
                                        _rbnf_cur_token = None
                                except IndexError:
                                    _rbnf_cur_token = None
                                lcl_7 = _rbnf_cur_token
                                rbnf_tmp_3 = lcl_7
                                lcl_7 = (rbnf_tmp_3 is None)
                                if lcl_7:
                                    lcl_7 = None
                                else:
                                    try:
                                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                        if (_rbnf_cur_token.idint is 43):
                                            builtin_tokens.offset += 1
                                        else:
                                            _rbnf_cur_token = None
                                    except IndexError:
                                        _rbnf_cur_token = None
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_4 = lcl_8
                                    lcl_8 = (rbnf_tmp_4 is None)
                                    if lcl_8:
                                        lcl_8 = None
                                    else:
                                        lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                        lcl_9 = builtin_mk_ast('StructConst', lcl_9)
                                        rbnf_tmp_1_ = lcl_9
                                        lcl_8 = rbnf_tmp_1_
                                    lcl_7 = lcl_8
                                lcl_10 = lcl_7
                            lcl_5 = lcl_10
                        elif (lcl_6 == 32):
                            lcl_10 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                            rbnf_named__check_2 = lcl_10
                            lcl_10 = (rbnf_named__check_2 is None)
                            if lcl_10:
                                lcl_10 = None
                            else:
                                rbnf_tmp_2 = rbnf_named__check_2
                                try:
                                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                    if (_rbnf_cur_token.idint is 37):
                                        builtin_tokens.offset += 1
                                    else:
                                        _rbnf_cur_token = None
                                except IndexError:
                                    _rbnf_cur_token = None
                                lcl_7 = _rbnf_cur_token
                                rbnf_tmp_3 = lcl_7
                                lcl_7 = (rbnf_tmp_3 is None)
                                if lcl_7:
                                    lcl_7 = None
                                else:
                                    try:
                                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                        if (_rbnf_cur_token.idint is 43):
                                            builtin_tokens.offset += 1
                                        else:
                                            _rbnf_cur_token = None
                                    except IndexError:
                                        _rbnf_cur_token = None
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_4 = lcl_8
                                    lcl_8 = (rbnf_tmp_4 is None)
                                    if lcl_8:
                                        lcl_8 = None
                                    else:
                                        lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                        lcl_9 = builtin_mk_ast('StructConst', lcl_9)
                                        rbnf_tmp_1_ = lcl_9
                                        lcl_8 = rbnf_tmp_1_
                                    lcl_7 = lcl_8
                                lcl_10 = lcl_7
                            lcl_5 = lcl_10
                        elif (lcl_6 == 24):
                            lcl_10 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                            rbnf_named__check_2 = lcl_10
                            lcl_10 = (rbnf_named__check_2 is None)
                            if lcl_10:
                                lcl_10 = None
                            else:
                                rbnf_tmp_2 = rbnf_named__check_2
                                try:
                                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                    if (_rbnf_cur_token.idint is 37):
                                        builtin_tokens.offset += 1
                                    else:
                                        _rbnf_cur_token = None
                                except IndexError:
                                    _rbnf_cur_token = None
                                lcl_7 = _rbnf_cur_token
                                rbnf_tmp_3 = lcl_7
                                lcl_7 = (rbnf_tmp_3 is None)
                                if lcl_7:
                                    lcl_7 = None
                                else:
                                    try:
                                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                        if (_rbnf_cur_token.idint is 43):
                                            builtin_tokens.offset += 1
                                        else:
                                            _rbnf_cur_token = None
                                    except IndexError:
                                        _rbnf_cur_token = None
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_4 = lcl_8
                                    lcl_8 = (rbnf_tmp_4 is None)
                                    if lcl_8:
                                        lcl_8 = None
                                    else:
                                        lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                        lcl_9 = builtin_mk_ast('StructConst', lcl_9)
                                        rbnf_tmp_1_ = lcl_9
                                        lcl_8 = rbnf_tmp_1_
                                    lcl_7 = lcl_8
                                lcl_10 = lcl_7
                            lcl_5 = lcl_10
                        elif (lcl_6 == 27):
                            lcl_10 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                            rbnf_named__check_2 = lcl_10
                            lcl_10 = (rbnf_named__check_2 is None)
                            if lcl_10:
                                lcl_10 = None
                            else:
                                rbnf_tmp_2 = rbnf_named__check_2
                                try:
                                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                    if (_rbnf_cur_token.idint is 37):
                                        builtin_tokens.offset += 1
                                    else:
                                        _rbnf_cur_token = None
                                except IndexError:
                                    _rbnf_cur_token = None
                                lcl_7 = _rbnf_cur_token
                                rbnf_tmp_3 = lcl_7
                                lcl_7 = (rbnf_tmp_3 is None)
                                if lcl_7:
                                    lcl_7 = None
                                else:
                                    try:
                                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                        if (_rbnf_cur_token.idint is 43):
                                            builtin_tokens.offset += 1
                                        else:
                                            _rbnf_cur_token = None
                                    except IndexError:
                                        _rbnf_cur_token = None
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_4 = lcl_8
                                    lcl_8 = (rbnf_tmp_4 is None)
                                    if lcl_8:
                                        lcl_8 = None
                                    else:
                                        lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                        lcl_9 = builtin_mk_ast('StructConst', lcl_9)
                                        rbnf_tmp_1_ = lcl_9
                                        lcl_8 = rbnf_tmp_1_
                                    lcl_7 = lcl_8
                                lcl_10 = lcl_7
                            lcl_5 = lcl_10
                        elif (lcl_6 == 26):
                            lcl_10 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                            rbnf_named__check_2 = lcl_10
                            lcl_10 = (rbnf_named__check_2 is None)
                            if lcl_10:
                                lcl_10 = None
                            else:
                                rbnf_tmp_2 = rbnf_named__check_2
                                try:
                                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                    if (_rbnf_cur_token.idint is 37):
                                        builtin_tokens.offset += 1
                                    else:
                                        _rbnf_cur_token = None
                                except IndexError:
                                    _rbnf_cur_token = None
                                lcl_7 = _rbnf_cur_token
                                rbnf_tmp_3 = lcl_7
                                lcl_7 = (rbnf_tmp_3 is None)
                                if lcl_7:
                                    lcl_7 = None
                                else:
                                    try:
                                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                        if (_rbnf_cur_token.idint is 43):
                                            builtin_tokens.offset += 1
                                        else:
                                            _rbnf_cur_token = None
                                    except IndexError:
                                        _rbnf_cur_token = None
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_4 = lcl_8
                                    lcl_8 = (rbnf_tmp_4 is None)
                                    if lcl_8:
                                        lcl_8 = None
                                    else:
                                        lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                        lcl_9 = builtin_mk_ast('StructConst', lcl_9)
                                        rbnf_tmp_1_ = lcl_9
                                        lcl_8 = rbnf_tmp_1_
                                    lcl_7 = lcl_8
                                lcl_10 = lcl_7
                            lcl_5 = lcl_10
                        elif (lcl_6 == 25):
                            lcl_10 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                            rbnf_named__check_2 = lcl_10
                            lcl_10 = (rbnf_named__check_2 is None)
                            if lcl_10:
                                lcl_10 = None
                            else:
                                rbnf_tmp_2 = rbnf_named__check_2
                                try:
                                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                    if (_rbnf_cur_token.idint is 37):
                                        builtin_tokens.offset += 1
                                    else:
                                        _rbnf_cur_token = None
                                except IndexError:
                                    _rbnf_cur_token = None
                                lcl_7 = _rbnf_cur_token
                                rbnf_tmp_3 = lcl_7
                                lcl_7 = (rbnf_tmp_3 is None)
                                if lcl_7:
                                    lcl_7 = None
                                else:
                                    try:
                                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                        if (_rbnf_cur_token.idint is 43):
                                            builtin_tokens.offset += 1
                                        else:
                                            _rbnf_cur_token = None
                                    except IndexError:
                                        _rbnf_cur_token = None
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_4 = lcl_8
                                    lcl_8 = (rbnf_tmp_4 is None)
                                    if lcl_8:
                                        lcl_8 = None
                                    else:
                                        lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                        lcl_9 = builtin_mk_ast('StructConst', lcl_9)
                                        rbnf_tmp_1_ = lcl_9
                                        lcl_8 = rbnf_tmp_1_
                                    lcl_7 = lcl_8
                                lcl_10 = lcl_7
                            lcl_5 = lcl_10
                        elif (lcl_6 == 23):
                            lcl_10 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                            rbnf_named__check_2 = lcl_10
                            lcl_10 = (rbnf_named__check_2 is None)
                            if lcl_10:
                                lcl_10 = None
                            else:
                                rbnf_tmp_2 = rbnf_named__check_2
                                try:
                                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                    if (_rbnf_cur_token.idint is 37):
                                        builtin_tokens.offset += 1
                                    else:
                                        _rbnf_cur_token = None
                                except IndexError:
                                    _rbnf_cur_token = None
                                lcl_7 = _rbnf_cur_token
                                rbnf_tmp_3 = lcl_7
                                lcl_7 = (rbnf_tmp_3 is None)
                                if lcl_7:
                                    lcl_7 = None
                                else:
                                    try:
                                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                        if (_rbnf_cur_token.idint is 43):
                                            builtin_tokens.offset += 1
                                        else:
                                            _rbnf_cur_token = None
                                    except IndexError:
                                        _rbnf_cur_token = None
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_4 = lcl_8
                                    lcl_8 = (rbnf_tmp_4 is None)
                                    if lcl_8:
                                        lcl_8 = None
                                    else:
                                        lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                        lcl_9 = builtin_mk_ast('StructConst', lcl_9)
                                        rbnf_tmp_1_ = lcl_9
                                        lcl_8 = rbnf_tmp_1_
                                    lcl_7 = lcl_8
                                lcl_10 = lcl_7
                            lcl_5 = lcl_10
                        elif (lcl_6 == 29):
                            lcl_10 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                            rbnf_named__check_2 = lcl_10
                            lcl_10 = (rbnf_named__check_2 is None)
                            if lcl_10:
                                lcl_10 = None
                            else:
                                rbnf_tmp_2 = rbnf_named__check_2
                                try:
                                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                    if (_rbnf_cur_token.idint is 37):
                                        builtin_tokens.offset += 1
                                    else:
                                        _rbnf_cur_token = None
                                except IndexError:
                                    _rbnf_cur_token = None
                                lcl_7 = _rbnf_cur_token
                                rbnf_tmp_3 = lcl_7
                                lcl_7 = (rbnf_tmp_3 is None)
                                if lcl_7:
                                    lcl_7 = None
                                else:
                                    try:
                                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                        if (_rbnf_cur_token.idint is 43):
                                            builtin_tokens.offset += 1
                                        else:
                                            _rbnf_cur_token = None
                                    except IndexError:
                                        _rbnf_cur_token = None
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_4 = lcl_8
                                    lcl_8 = (rbnf_tmp_4 is None)
                                    if lcl_8:
                                        lcl_8 = None
                                    else:
                                        lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                        lcl_9 = builtin_mk_ast('StructConst', lcl_9)
                                        rbnf_tmp_1_ = lcl_9
                                        lcl_8 = rbnf_tmp_1_
                                    lcl_7 = lcl_8
                                lcl_10 = lcl_7
                            lcl_5 = lcl_10
                        elif (lcl_6 == 28):
                            lcl_10 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                            rbnf_named__check_2 = lcl_10
                            lcl_10 = (rbnf_named__check_2 is None)
                            if lcl_10:
                                lcl_10 = None
                            else:
                                rbnf_tmp_2 = rbnf_named__check_2
                                try:
                                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                    if (_rbnf_cur_token.idint is 37):
                                        builtin_tokens.offset += 1
                                    else:
                                        _rbnf_cur_token = None
                                except IndexError:
                                    _rbnf_cur_token = None
                                lcl_7 = _rbnf_cur_token
                                rbnf_tmp_3 = lcl_7
                                lcl_7 = (rbnf_tmp_3 is None)
                                if lcl_7:
                                    lcl_7 = None
                                else:
                                    try:
                                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                        if (_rbnf_cur_token.idint is 43):
                                            builtin_tokens.offset += 1
                                        else:
                                            _rbnf_cur_token = None
                                    except IndexError:
                                        _rbnf_cur_token = None
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_4 = lcl_8
                                    lcl_8 = (rbnf_tmp_4 is None)
                                    if lcl_8:
                                        lcl_8 = None
                                    else:
                                        lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                        lcl_9 = builtin_mk_ast('StructConst', lcl_9)
                                        rbnf_tmp_1_ = lcl_9
                                        lcl_8 = rbnf_tmp_1_
                                    lcl_7 = lcl_8
                                lcl_10 = lcl_7
                            lcl_5 = lcl_10
                        elif (lcl_6 == 33):
                            lcl_10 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                            rbnf_named__check_2 = lcl_10
                            lcl_10 = (rbnf_named__check_2 is None)
                            if lcl_10:
                                lcl_10 = None
                            else:
                                rbnf_tmp_2 = rbnf_named__check_2
                                try:
                                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                    if (_rbnf_cur_token.idint is 37):
                                        builtin_tokens.offset += 1
                                    else:
                                        _rbnf_cur_token = None
                                except IndexError:
                                    _rbnf_cur_token = None
                                lcl_7 = _rbnf_cur_token
                                rbnf_tmp_3 = lcl_7
                                lcl_7 = (rbnf_tmp_3 is None)
                                if lcl_7:
                                    lcl_7 = None
                                else:
                                    try:
                                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                        if (_rbnf_cur_token.idint is 43):
                                            builtin_tokens.offset += 1
                                        else:
                                            _rbnf_cur_token = None
                                    except IndexError:
                                        _rbnf_cur_token = None
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_4 = lcl_8
                                    lcl_8 = (rbnf_tmp_4 is None)
                                    if lcl_8:
                                        lcl_8 = None
                                    else:
                                        lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                        lcl_9 = builtin_mk_ast('StructConst', lcl_9)
                                        rbnf_tmp_1_ = lcl_9
                                        lcl_8 = rbnf_tmp_1_
                                    lcl_7 = lcl_8
                                lcl_10 = lcl_7
                            lcl_5 = lcl_10
                        elif (lcl_6 == 4):
                            lcl_10 = rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens)
                            rbnf_named__check_2 = lcl_10
                            lcl_10 = (rbnf_named__check_2 is None)
                            if lcl_10:
                                lcl_10 = None
                            else:
                                rbnf_tmp_2 = rbnf_named__check_2
                                try:
                                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                    if (_rbnf_cur_token.idint is 37):
                                        builtin_tokens.offset += 1
                                    else:
                                        _rbnf_cur_token = None
                                except IndexError:
                                    _rbnf_cur_token = None
                                lcl_7 = _rbnf_cur_token
                                rbnf_tmp_3 = lcl_7
                                lcl_7 = (rbnf_tmp_3 is None)
                                if lcl_7:
                                    lcl_7 = None
                                else:
                                    try:
                                        _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                        if (_rbnf_cur_token.idint is 43):
                                            builtin_tokens.offset += 1
                                        else:
                                            _rbnf_cur_token = None
                                    except IndexError:
                                        _rbnf_cur_token = None
                                    lcl_8 = _rbnf_cur_token
                                    rbnf_tmp_4 = lcl_8
                                    lcl_8 = (rbnf_tmp_4 is None)
                                    if lcl_8:
                                        lcl_8 = None
                                    else:
                                        lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4)
                                        lcl_9 = builtin_mk_ast('StructConst', lcl_9)
                                        rbnf_tmp_1_ = lcl_9
                                        lcl_8 = rbnf_tmp_1_
                                    lcl_7 = lcl_8
                                lcl_10 = lcl_7
                            lcl_5 = lcl_10
                        else:
                            lcl_5 = None
                        lcl_4 = lcl_5
                    else:
                        lcl_4 = None
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_StructType(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 36):
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
            lcl_1 = builtin_tokens.offset
            rbnf_named__off_1 = lcl_1
            lcl_1 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
            if lcl_1:
                lcl_3 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                lcl_3 = lcl_3.idint
                if (lcl_3 == 37):
                    _rbnf_old_offset = builtin_tokens.offset
                    _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                    builtin_tokens.offset = (_rbnf_old_offset + 1)
                    lcl_4 = _rbnf_cur_token
                    rbnf_tmp_1 = lcl_4
                    lcl_4 = (rbnf_tmp_0, rbnf_tmp_1)
                    lcl_4 = builtin_mk_ast('StructType', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_2 = rbnf_tmp_1_
                elif (lcl_3 == 36):
                    lcl_4 = rbnf_named_parse_TypeList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('StructType', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 15):
                    lcl_4 = rbnf_named_parse_TypeList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('StructType', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 32):
                    lcl_4 = rbnf_named_parse_TypeList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('StructType', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 24):
                    lcl_4 = rbnf_named_parse_TypeList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('StructType', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 27):
                    lcl_4 = rbnf_named_parse_TypeList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('StructType', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 26):
                    lcl_4 = rbnf_named_parse_TypeList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('StructType', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 25):
                    lcl_4 = rbnf_named_parse_TypeList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('StructType', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 23):
                    lcl_4 = rbnf_named_parse_TypeList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('StructType', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 29):
                    lcl_4 = rbnf_named_parse_TypeList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('StructType', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 28):
                    lcl_4 = rbnf_named_parse_TypeList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('StructType', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 33):
                    lcl_4 = rbnf_named_parse_TypeList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('StructType', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 4):
                    lcl_4 = rbnf_named_parse_TypeList(builtin_state, builtin_tokens)
                    rbnf_named__check_1 = lcl_4
                    lcl_4 = (rbnf_named__check_1 is None)
                    if lcl_4:
                        lcl_4 = None
                    else:
                        rbnf_tmp_1 = rbnf_named__check_1
                        try:
                            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                            if (_rbnf_cur_token.idint is 37):
                                builtin_tokens.offset += 1
                            else:
                                _rbnf_cur_token = None
                        except IndexError:
                            _rbnf_cur_token = None
                        lcl_5 = _rbnf_cur_token
                        rbnf_tmp_2 = lcl_5
                        lcl_5 = (rbnf_tmp_2 is None)
                        if lcl_5:
                            lcl_5 = None
                        else:
                            lcl_6 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2)
                            lcl_6 = builtin_mk_ast('StructType', lcl_6)
                            rbnf_tmp_1_ = lcl_6
                            lcl_5 = rbnf_tmp_1_
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                else:
                    lcl_2 = None
                lcl_1 = lcl_2
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_SwitchTerm(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 81):
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
            lcl_1 = rbnf_named_parse_TypeValue(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 19):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_2 = _rbnf_cur_token
                rbnf_tmp_2 = lcl_2
                lcl_2 = (rbnf_tmp_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    lcl_3 = rbnf_named_parse_LabelType(builtin_state, builtin_tokens)
                    rbnf_named__check_3 = lcl_3
                    lcl_3 = (rbnf_named__check_3 is None)
                    if lcl_3:
                        lcl_3 = None
                    else:
                        rbnf_tmp_3 = rbnf_named__check_3
                        lcl_4 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                        rbnf_named__check_4 = lcl_4
                        lcl_4 = (rbnf_named__check_4 is None)
                        if lcl_4:
                            lcl_4 = None
                        else:
                            rbnf_tmp_4 = rbnf_named__check_4
                            try:
                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                if (_rbnf_cur_token.idint is 33):
                                    builtin_tokens.offset += 1
                                else:
                                    _rbnf_cur_token = None
                            except IndexError:
                                _rbnf_cur_token = None
                            lcl_5 = _rbnf_cur_token
                            rbnf_tmp_5 = lcl_5
                            lcl_5 = (rbnf_tmp_5 is None)
                            if lcl_5:
                                lcl_5 = None
                            else:
                                lcl_6 = builtin_tokens.offset
                                rbnf_named__off_3 = lcl_6
                                lcl_6 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                                if lcl_6:
                                    lcl_8 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                                    lcl_8 = lcl_8.idint
                                    if (lcl_8 == 24):
                                        lcl_9 = rbnf_named_parse_Cases(builtin_state, builtin_tokens)
                                        rbnf_named__check_6 = lcl_9
                                        lcl_9 = (rbnf_named__check_6 is None)
                                        if lcl_9:
                                            lcl_9 = None
                                        else:
                                            rbnf_tmp_6 = rbnf_named__check_6
                                            try:
                                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                                if (_rbnf_cur_token.idint is 35):
                                                    builtin_tokens.offset += 1
                                                else:
                                                    _rbnf_cur_token = None
                                            except IndexError:
                                                _rbnf_cur_token = None
                                            lcl_10 = _rbnf_cur_token
                                            rbnf_tmp_7 = lcl_10
                                            lcl_10 = (rbnf_tmp_7 is None)
                                            if lcl_10:
                                                lcl_10 = None
                                            else:
                                                lcl_11 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_6, rbnf_tmp_7)
                                                lcl_11 = builtin_mk_ast('SwitchTerm', lcl_11)
                                                rbnf_tmp_1_ = lcl_11
                                                lcl_10 = rbnf_tmp_1_
                                            lcl_9 = lcl_10
                                        lcl_7 = lcl_9
                                    elif (lcl_8 == 27):
                                        lcl_10 = rbnf_named_parse_Cases(builtin_state, builtin_tokens)
                                        rbnf_named__check_6 = lcl_10
                                        lcl_10 = (rbnf_named__check_6 is None)
                                        if lcl_10:
                                            lcl_10 = None
                                        else:
                                            rbnf_tmp_6 = rbnf_named__check_6
                                            try:
                                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                                if (_rbnf_cur_token.idint is 35):
                                                    builtin_tokens.offset += 1
                                                else:
                                                    _rbnf_cur_token = None
                                            except IndexError:
                                                _rbnf_cur_token = None
                                            lcl_11 = _rbnf_cur_token
                                            rbnf_tmp_7 = lcl_11
                                            lcl_11 = (rbnf_tmp_7 is None)
                                            if lcl_11:
                                                lcl_11 = None
                                            else:
                                                lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_6, rbnf_tmp_7)
                                                lcl_9 = builtin_mk_ast('SwitchTerm', lcl_9)
                                                rbnf_tmp_1_ = lcl_9
                                                lcl_11 = rbnf_tmp_1_
                                            lcl_10 = lcl_11
                                        lcl_7 = lcl_10
                                    elif (lcl_8 == 26):
                                        lcl_10 = rbnf_named_parse_Cases(builtin_state, builtin_tokens)
                                        rbnf_named__check_6 = lcl_10
                                        lcl_10 = (rbnf_named__check_6 is None)
                                        if lcl_10:
                                            lcl_10 = None
                                        else:
                                            rbnf_tmp_6 = rbnf_named__check_6
                                            try:
                                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                                if (_rbnf_cur_token.idint is 35):
                                                    builtin_tokens.offset += 1
                                                else:
                                                    _rbnf_cur_token = None
                                            except IndexError:
                                                _rbnf_cur_token = None
                                            lcl_11 = _rbnf_cur_token
                                            rbnf_tmp_7 = lcl_11
                                            lcl_11 = (rbnf_tmp_7 is None)
                                            if lcl_11:
                                                lcl_11 = None
                                            else:
                                                lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_6, rbnf_tmp_7)
                                                lcl_9 = builtin_mk_ast('SwitchTerm', lcl_9)
                                                rbnf_tmp_1_ = lcl_9
                                                lcl_11 = rbnf_tmp_1_
                                            lcl_10 = lcl_11
                                        lcl_7 = lcl_10
                                    elif (lcl_8 == 25):
                                        lcl_10 = rbnf_named_parse_Cases(builtin_state, builtin_tokens)
                                        rbnf_named__check_6 = lcl_10
                                        lcl_10 = (rbnf_named__check_6 is None)
                                        if lcl_10:
                                            lcl_10 = None
                                        else:
                                            rbnf_tmp_6 = rbnf_named__check_6
                                            try:
                                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                                if (_rbnf_cur_token.idint is 35):
                                                    builtin_tokens.offset += 1
                                                else:
                                                    _rbnf_cur_token = None
                                            except IndexError:
                                                _rbnf_cur_token = None
                                            lcl_11 = _rbnf_cur_token
                                            rbnf_tmp_7 = lcl_11
                                            lcl_11 = (rbnf_tmp_7 is None)
                                            if lcl_11:
                                                lcl_11 = None
                                            else:
                                                lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_6, rbnf_tmp_7)
                                                lcl_9 = builtin_mk_ast('SwitchTerm', lcl_9)
                                                rbnf_tmp_1_ = lcl_9
                                                lcl_11 = rbnf_tmp_1_
                                            lcl_10 = lcl_11
                                        lcl_7 = lcl_10
                                    elif (lcl_8 == 23):
                                        lcl_10 = rbnf_named_parse_Cases(builtin_state, builtin_tokens)
                                        rbnf_named__check_6 = lcl_10
                                        lcl_10 = (rbnf_named__check_6 is None)
                                        if lcl_10:
                                            lcl_10 = None
                                        else:
                                            rbnf_tmp_6 = rbnf_named__check_6
                                            try:
                                                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                                                if (_rbnf_cur_token.idint is 35):
                                                    builtin_tokens.offset += 1
                                                else:
                                                    _rbnf_cur_token = None
                                            except IndexError:
                                                _rbnf_cur_token = None
                                            lcl_11 = _rbnf_cur_token
                                            rbnf_tmp_7 = lcl_11
                                            lcl_11 = (rbnf_tmp_7 is None)
                                            if lcl_11:
                                                lcl_11 = None
                                            else:
                                                lcl_9 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_6, rbnf_tmp_7)
                                                lcl_9 = builtin_mk_ast('SwitchTerm', lcl_9)
                                                rbnf_tmp_1_ = lcl_9
                                                lcl_11 = rbnf_tmp_1_
                                            lcl_10 = lcl_11
                                        lcl_7 = lcl_10
                                    elif (lcl_8 == 35):
                                        lcl_10 = ()
                                        rbnf_tmp_1_ = lcl_10
                                        _rbnf_old_offset = builtin_tokens.offset
                                        _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                                        builtin_tokens.offset = (_rbnf_old_offset + 1)
                                        lcl_10 = _rbnf_cur_token
                                        rbnf_tmp_6 = lcl_10
                                        lcl_10 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3, rbnf_tmp_4, rbnf_tmp_5, rbnf_tmp_1_, rbnf_tmp_6)
                                        lcl_10 = builtin_mk_ast('SwitchTerm', lcl_10)
                                        rbnf_tmp_2_ = lcl_10
                                        lcl_7 = rbnf_tmp_2_
                                    else:
                                        lcl_7 = None
                                    lcl_6 = lcl_7
                                else:
                                    lcl_6 = None
                                lcl_5 = lcl_6
                            lcl_4 = lcl_5
                        lcl_3 = lcl_4
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_Terminator(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 81):
                lcl_3 = rbnf_named_parse_SwitchTerm(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Terminator', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 79):
                lcl_3 = rbnf_named_parse_RetTerm(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Terminator', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 80):
                lcl_3 = rbnf_named_parse_BrTerm(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Terminator', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_TopLevelEntity(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 11):
                lcl_3 = rbnf_named_parse_SourceFilename(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('TopLevelEntity', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 76):
                lcl_3 = rbnf_named_parse_FunctionDef(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('TopLevelEntity', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 75):
                lcl_3 = rbnf_named_parse_FunctionDecl(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('TopLevelEntity', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 4):
                lcl_3 = rbnf_named_parse_TypeDef(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('TopLevelEntity', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 5):
                lcl_3 = rbnf_named_parse_Global(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('TopLevelEntity', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_TopLevelEntityList(builtin_state, builtin_tokens):
        lcl_0 = ()
        rbnf_tmp_1_ = lcl_0
        lcl_0 = rbnf_named_parse_TopLevelEntity(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_1_, rbnf_tmp_0)
            lcl_1 = builtin_mk_ast('TopLevelEntityList', lcl_1)
            rbnf_tmp_2_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_TopLevelEntityList(rbnf_tmp_2_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_Type(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 36):
                lcl_3 = rbnf_named_parse_StructType(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('FirstClassType', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (rbnf_tmp_1_,)
                    lcl_4 = builtin_mk_ast('Type', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_4 = rbnf_named_lr_loop_Type(rbnf_tmp_2_, builtin_state, builtin_tokens)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 15):
                lcl_3 = rbnf_named_parse_VoidType(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Type', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = rbnf_named_lr_loop_Type(rbnf_tmp_1_, builtin_state, builtin_tokens)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 32):
                lcl_3 = rbnf_named_parse_LabelType(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('FirstClassType', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (rbnf_tmp_1_,)
                    lcl_4 = builtin_mk_ast('Type', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_4 = rbnf_named_lr_loop_Type(rbnf_tmp_2_, builtin_state, builtin_tokens)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 24):
                lcl_3 = rbnf_named_parse_IntType(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('FirstClassType', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (rbnf_tmp_1_,)
                    lcl_4 = builtin_mk_ast('Type', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_4 = rbnf_named_lr_loop_Type(rbnf_tmp_2_, builtin_state, builtin_tokens)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 27):
                lcl_3 = rbnf_named_parse_IntType(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('FirstClassType', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (rbnf_tmp_1_,)
                    lcl_4 = builtin_mk_ast('Type', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_4 = rbnf_named_lr_loop_Type(rbnf_tmp_2_, builtin_state, builtin_tokens)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 26):
                lcl_3 = rbnf_named_parse_IntType(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('FirstClassType', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (rbnf_tmp_1_,)
                    lcl_4 = builtin_mk_ast('Type', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_4 = rbnf_named_lr_loop_Type(rbnf_tmp_2_, builtin_state, builtin_tokens)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 25):
                lcl_3 = rbnf_named_parse_IntType(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('FirstClassType', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (rbnf_tmp_1_,)
                    lcl_4 = builtin_mk_ast('Type', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_4 = rbnf_named_lr_loop_Type(rbnf_tmp_2_, builtin_state, builtin_tokens)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 23):
                lcl_3 = rbnf_named_parse_IntType(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('FirstClassType', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (rbnf_tmp_1_,)
                    lcl_4 = builtin_mk_ast('Type', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_4 = rbnf_named_lr_loop_Type(rbnf_tmp_2_, builtin_state, builtin_tokens)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 29):
                lcl_3 = rbnf_named_parse_FloatType(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('FirstClassType', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (rbnf_tmp_1_,)
                    lcl_4 = builtin_mk_ast('Type', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_4 = rbnf_named_lr_loop_Type(rbnf_tmp_2_, builtin_state, builtin_tokens)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 28):
                lcl_3 = rbnf_named_parse_FloatType(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('FirstClassType', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (rbnf_tmp_1_,)
                    lcl_4 = builtin_mk_ast('Type', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_4 = rbnf_named_lr_loop_Type(rbnf_tmp_2_, builtin_state, builtin_tokens)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 33):
                lcl_3 = rbnf_named_parse_ArrayType(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('FirstClassType', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (rbnf_tmp_1_,)
                    lcl_4 = builtin_mk_ast('Type', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_4 = rbnf_named_lr_loop_Type(rbnf_tmp_2_, builtin_state, builtin_tokens)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 4):
                lcl_3 = rbnf_named_parse_NamedType(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('FirstClassType', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_4 = (rbnf_tmp_1_,)
                    lcl_4 = builtin_mk_ast('Type', lcl_4)
                    rbnf_tmp_2_ = lcl_4
                    lcl_4 = rbnf_named_lr_loop_Type(rbnf_tmp_2_, builtin_state, builtin_tokens)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_TypeConstList(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_TypeConstant(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('TypeConstList', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_TypeConstList(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_TypeConstant(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                lcl_2 = (rbnf_tmp_0, rbnf_tmp_1)
                lcl_2 = builtin_mk_ast('TypeConstant', lcl_2)
                rbnf_tmp_1_ = lcl_2
                lcl_1 = rbnf_tmp_1_
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_TypeDef(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            try:
                _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                if (_rbnf_cur_token.idint is 12):
                    builtin_tokens.offset += 1
                else:
                    _rbnf_cur_token = None
            except IndexError:
                _rbnf_cur_token = None
            lcl_1 = _rbnf_cur_token
            rbnf_tmp_1 = lcl_1
            lcl_1 = (rbnf_tmp_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                try:
                    _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
                    if (_rbnf_cur_token.idint is 14):
                        builtin_tokens.offset += 1
                    else:
                        _rbnf_cur_token = None
                except IndexError:
                    _rbnf_cur_token = None
                lcl_2 = _rbnf_cur_token
                rbnf_tmp_2 = lcl_2
                lcl_2 = (rbnf_tmp_2 is None)
                if lcl_2:
                    lcl_2 = None
                else:
                    lcl_3 = builtin_tokens.offset
                    rbnf_named__off_2 = lcl_3
                    lcl_3 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
                    if lcl_3:
                        lcl_5 = builtin_tokens.array[(builtin_tokens.offset + 0)]
                        lcl_5 = lcl_5.idint
                        if (lcl_5 == 36):
                            lcl_6 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_7 = builtin_mk_ast('TypeDef', lcl_7)
                                rbnf_tmp_1_ = lcl_7
                                lcl_6 = rbnf_tmp_1_
                            lcl_4 = lcl_6
                        elif (lcl_5 == 15):
                            lcl_6 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_7 = builtin_mk_ast('TypeDef', lcl_7)
                                rbnf_tmp_1_ = lcl_7
                                lcl_6 = rbnf_tmp_1_
                            lcl_4 = lcl_6
                        elif (lcl_5 == 13):
                            lcl_6 = rbnf_named_parse_OpaqueType(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_7 = builtin_mk_ast('TypeDef', lcl_7)
                                rbnf_tmp_1_ = lcl_7
                                lcl_6 = rbnf_tmp_1_
                            lcl_4 = lcl_6
                        elif (lcl_5 == 32):
                            lcl_6 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_7 = builtin_mk_ast('TypeDef', lcl_7)
                                rbnf_tmp_1_ = lcl_7
                                lcl_6 = rbnf_tmp_1_
                            lcl_4 = lcl_6
                        elif (lcl_5 == 24):
                            lcl_6 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_7 = builtin_mk_ast('TypeDef', lcl_7)
                                rbnf_tmp_1_ = lcl_7
                                lcl_6 = rbnf_tmp_1_
                            lcl_4 = lcl_6
                        elif (lcl_5 == 27):
                            lcl_6 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_7 = builtin_mk_ast('TypeDef', lcl_7)
                                rbnf_tmp_1_ = lcl_7
                                lcl_6 = rbnf_tmp_1_
                            lcl_4 = lcl_6
                        elif (lcl_5 == 26):
                            lcl_6 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_7 = builtin_mk_ast('TypeDef', lcl_7)
                                rbnf_tmp_1_ = lcl_7
                                lcl_6 = rbnf_tmp_1_
                            lcl_4 = lcl_6
                        elif (lcl_5 == 25):
                            lcl_6 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_7 = builtin_mk_ast('TypeDef', lcl_7)
                                rbnf_tmp_1_ = lcl_7
                                lcl_6 = rbnf_tmp_1_
                            lcl_4 = lcl_6
                        elif (lcl_5 == 23):
                            lcl_6 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_7 = builtin_mk_ast('TypeDef', lcl_7)
                                rbnf_tmp_1_ = lcl_7
                                lcl_6 = rbnf_tmp_1_
                            lcl_4 = lcl_6
                        elif (lcl_5 == 29):
                            lcl_6 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_7 = builtin_mk_ast('TypeDef', lcl_7)
                                rbnf_tmp_1_ = lcl_7
                                lcl_6 = rbnf_tmp_1_
                            lcl_4 = lcl_6
                        elif (lcl_5 == 28):
                            lcl_6 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_7 = builtin_mk_ast('TypeDef', lcl_7)
                                rbnf_tmp_1_ = lcl_7
                                lcl_6 = rbnf_tmp_1_
                            lcl_4 = lcl_6
                        elif (lcl_5 == 33):
                            lcl_6 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_7 = builtin_mk_ast('TypeDef', lcl_7)
                                rbnf_tmp_1_ = lcl_7
                                lcl_6 = rbnf_tmp_1_
                            lcl_4 = lcl_6
                        elif (lcl_5 == 4):
                            lcl_6 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
                            rbnf_named__check_3 = lcl_6
                            lcl_6 = (rbnf_named__check_3 is None)
                            if lcl_6:
                                lcl_6 = None
                            else:
                                rbnf_tmp_3 = rbnf_named__check_3
                                lcl_7 = (rbnf_tmp_0, rbnf_tmp_1, rbnf_tmp_2, rbnf_tmp_3)
                                lcl_7 = builtin_mk_ast('TypeDef', lcl_7)
                                rbnf_tmp_1_ = lcl_7
                                lcl_6 = rbnf_tmp_1_
                            lcl_4 = lcl_6
                        else:
                            lcl_4 = None
                        lcl_3 = lcl_4
                    else:
                        lcl_3 = None
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_TypeList(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('TypeList', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_1 = rbnf_named_lr_loop_TypeList(rbnf_tmp_1_, builtin_state, builtin_tokens)
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_TypeValue(builtin_state, builtin_tokens):
        lcl_0 = rbnf_named_parse_Type(builtin_state, builtin_tokens)
        rbnf_named__check_0 = lcl_0
        lcl_0 = (rbnf_named__check_0 is None)
        if lcl_0:
            lcl_0 = None
        else:
            rbnf_tmp_0 = rbnf_named__check_0
            lcl_1 = rbnf_named_parse_Value(builtin_state, builtin_tokens)
            rbnf_named__check_1 = lcl_1
            lcl_1 = (rbnf_named__check_1 is None)
            if lcl_1:
                lcl_1 = None
            else:
                rbnf_tmp_1 = rbnf_named__check_1
                lcl_2 = (rbnf_tmp_0, rbnf_tmp_1)
                lcl_2 = builtin_mk_ast('TypeValue', lcl_2)
                rbnf_tmp_1_ = lcl_2
                lcl_1 = rbnf_tmp_1_
            lcl_0 = lcl_1
        return lcl_0

    def rbnf_named_parse_UndefConst(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 46):
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
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('UndefConst', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_UnnamedAddr(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 129):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('UnnamedAddr', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            elif (lcl_2 == 128):
                _rbnf_old_offset = builtin_tokens.offset
                _rbnf_cur_token = builtin_tokens.array[_rbnf_old_offset]
                builtin_tokens.offset = (_rbnf_old_offset + 1)
                lcl_3 = _rbnf_cur_token
                rbnf_tmp_0 = lcl_3
                lcl_3 = (rbnf_tmp_0,)
                lcl_3 = builtin_mk_ast('UnnamedAddr', lcl_3)
                rbnf_tmp_1_ = lcl_3
                lcl_1 = rbnf_tmp_1_
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_Value(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 36):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 45):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 67):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 59):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 46):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 57):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 38):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 53):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 60):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 62):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 58):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 66):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 40):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 41):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 55):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 63):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 69):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 70):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 54):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 61):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 56):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 39):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 52):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 68):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 44):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 47):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 73):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 64):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 65):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 51):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 33):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 42):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 4):
                lcl_3 = rbnf_named_parse_LocalName(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 2):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 5):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 10):
                lcl_3 = rbnf_named_parse_Constant(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('Value', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_ValueInstruction(builtin_state, builtin_tokens):
        lcl_0 = builtin_tokens.offset
        rbnf_named__off_0 = lcl_0
        lcl_0 = (len(builtin_tokens.array) > (builtin_tokens.offset + 0))
        if lcl_0:
            lcl_2 = builtin_tokens.array[(builtin_tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 67):
                lcl_3 = rbnf_named_parse_BinInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 59):
                lcl_3 = rbnf_named_parse_BinInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 57):
                lcl_3 = rbnf_named_parse_BinInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 53):
                lcl_3 = rbnf_named_parse_BinInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 60):
                lcl_3 = rbnf_named_parse_BinInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 62):
                lcl_3 = rbnf_named_parse_BinInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 87):
                lcl_3 = rbnf_named_parse_SelectInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 58):
                lcl_3 = rbnf_named_parse_BinInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 86):
                lcl_3 = rbnf_named_parse_PhiInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 66):
                lcl_3 = rbnf_named_parse_BinInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 55):
                lcl_3 = rbnf_named_parse_BinInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 63):
                lcl_3 = rbnf_named_parse_BinInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 83):
                lcl_3 = rbnf_named_parse_LoadInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 69):
                lcl_3 = rbnf_named_parse_InsValInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 84):
                lcl_3 = rbnf_named_parse_CmpInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 70):
                lcl_3 = rbnf_named_parse_GEPInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 54):
                lcl_3 = rbnf_named_parse_BinInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 61):
                lcl_3 = rbnf_named_parse_BinInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 56):
                lcl_3 = rbnf_named_parse_BinInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 85):
                lcl_3 = rbnf_named_parse_CmpInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 52):
                lcl_3 = rbnf_named_parse_BinInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 68):
                lcl_3 = rbnf_named_parse_ExtValInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 73):
                lcl_3 = rbnf_named_parse_BitcastInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 64):
                lcl_3 = rbnf_named_parse_BinInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 65):
                lcl_3 = rbnf_named_parse_BinInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 82):
                lcl_3 = rbnf_named_parse_AllocaInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            elif (lcl_2 == 51):
                lcl_3 = rbnf_named_parse_BinInst(builtin_state, builtin_tokens)
                rbnf_named__check_0 = lcl_3
                lcl_3 = (rbnf_named__check_0 is None)
                if lcl_3:
                    lcl_3 = None
                else:
                    rbnf_tmp_0 = rbnf_named__check_0
                    lcl_4 = (rbnf_tmp_0,)
                    lcl_4 = builtin_mk_ast('ValueInstruction', lcl_4)
                    rbnf_tmp_1_ = lcl_4
                    lcl_3 = rbnf_tmp_1_
                lcl_1 = lcl_3
            else:
                lcl_1 = None
            lcl_0 = lcl_1
        else:
            lcl_0 = None
        return lcl_0

    def rbnf_named_parse_VoidType(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 15):
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
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('VoidType', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_ZeroInitializerConst(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 45):
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
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('ZeroInitializerConst', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0

    def rbnf_named_parse_name(builtin_state, builtin_tokens):
        try:
            _rbnf_cur_token = builtin_tokens.array[builtin_tokens.offset]
            if (_rbnf_cur_token.idint is 3):
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
            lcl_1 = (rbnf_tmp_0,)
            lcl_1 = builtin_mk_ast('name', lcl_1)
            rbnf_tmp_1_ = lcl_1
            lcl_0 = rbnf_tmp_1_
        return lcl_0
    return rbnf_named_parse_START