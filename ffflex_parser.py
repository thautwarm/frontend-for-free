# this file is auto-generated by RBNF.hs and the Python package rbnf-rts

from rbnf_rts.rbnf_linker import link
from rbnf_rts.utils import ImmutableMap
from rbnf_rts.lexical import *
__all__ = ['lexicals', 'run_lexer', 'mk_parser']
(lexicals, run_lexer) = lexer(r(WS='\\s+'), r(QuotedStr='"([^\\\\"]+|\\\\.)*?"|\'([^\\\\\']+|\\\\.)*?\''), r(Ident='[a-zA-Z_\\u4e00-\\u9fa5][a-zA-Z0-9_\\u4e00-\\u9fa5]*'), l[','], l['%reserve'], l['%include'], l['%ignore'], ignores=['WS'], reserved_map=ImmutableMap.from_dict({',': 'quote ,', '%ignore': 'quote %ignore', '%reserve': 'quote %reserve', '%include': 'quote %include'}), numbering={'BOF': 0, 'EOF': 1, 'quote ,': 2, 'quote %ignore': 3, 'quote %reserve': 4, 'quote %include': 5, 'WS': 6, 'QuotedStr': 7, 'Ident': 8})



def mk_parser(literal, unesc):
    from rbnf_rts.rts import AST as prim__mk__ast, Cons as prim__cons, _nil as prim__nil

    def lr_step_rbnf__sep__list__0(_slot_0, prim__state, prim__tokens):
        lcl_0 = 2
        try:
            _py_local_tk = prim__tokens.array[prim__tokens.offset]
            if (_py_local_tk.idint is lcl_0):
                prim__tokens.offset += 1
            else:
                _py_local_tk = None
        except IndexError:
            _py_local_tk = None
        lcl_0 = _py_local_tk
        _slot_1 = lcl_0
        lcl_0 = (_slot_1 is None)
        if lcl_0:
            lcl_1 = prim__tokens.offset
            lcl_1 = (lcl_1, 'quote , not match')
            lcl_1 = prim__cons(lcl_1, prim__nil)
            lcl_1 = lcl_1
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = parse_ignorable(prim__state, prim__tokens)
            _slot_2_check = lcl_1
            lcl_1 = _slot_2_check[0]
            lcl_1 = (lcl_1 is False)
            if lcl_1:
                lcl_1 = _slot_2_check
            else:
                lcl_2 = _slot_2_check[1]
                lcl_2 = lcl_2
                _slot_2 = lcl_2
                lcl_2 = _slot_0
                lcl_3 = _slot_2
                _py_local_t = lcl_2
                _py_local_t.append(lcl_3)
                lcl_2 = _py_local_t
                _slot_local__1 = lcl_2
                lcl_2 = (True, _slot_local__1)
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def lr_loop_rbnf__sep__list__0(_slot_0, prim__state, prim__tokens):
        lr_rbnf__sep__list__0_reduce = _slot_0
        lcl_0 = prim__tokens.offset
        _off_0 = lcl_0
        lcl_0 = lr_step_rbnf__sep__list__0(lr_rbnf__sep__list__0_reduce, prim__state, prim__tokens)
        lr_rbnf__sep__list__0_try = lcl_0
        lcl_0 = lr_rbnf__sep__list__0_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = prim__tokens.offset
            _off_0 = lcl_1
            lcl_1 = lr_rbnf__sep__list__0_try[1]
            lcl_1 = lcl_1
            lr_rbnf__sep__list__0_reduce = lcl_1
            lcl_1 = lr_step_rbnf__sep__list__0(lr_rbnf__sep__list__0_reduce, prim__state, prim__tokens)
            lr_rbnf__sep__list__0_try = lcl_1
            lcl_1 = lr_rbnf__sep__list__0_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = prim__tokens.offset
        lcl_0 = (lcl_0 is _off_0)
        if lcl_0:
            lcl_1 = (True, lr_rbnf__sep__list__0_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = lr_rbnf__sep__list__0_try
        return lcl_0

    def lr_step_rbnf__sep__list__1(_slot_0, prim__state, prim__tokens):
        lcl_0 = 2
        try:
            _py_local_tk = prim__tokens.array[prim__tokens.offset]
            if (_py_local_tk.idint is lcl_0):
                prim__tokens.offset += 1
            else:
                _py_local_tk = None
        except IndexError:
            _py_local_tk = None
        lcl_0 = _py_local_tk
        _slot_1 = lcl_0
        lcl_0 = (_slot_1 is None)
        if lcl_0:
            lcl_1 = prim__tokens.offset
            lcl_1 = (lcl_1, 'quote , not match')
            lcl_1 = prim__cons(lcl_1, prim__nil)
            lcl_1 = lcl_1
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = parse_filename(prim__state, prim__tokens)
            _slot_2_check = lcl_1
            lcl_1 = _slot_2_check[0]
            lcl_1 = (lcl_1 is False)
            if lcl_1:
                lcl_1 = _slot_2_check
            else:
                lcl_2 = _slot_2_check[1]
                lcl_2 = lcl_2
                _slot_2 = lcl_2
                lcl_2 = _slot_0
                lcl_3 = _slot_2
                _py_local_t = lcl_2
                _py_local_t.append(lcl_3)
                lcl_2 = _py_local_t
                _slot_local__1 = lcl_2
                lcl_2 = (True, _slot_local__1)
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def lr_loop_rbnf__sep__list__1(_slot_0, prim__state, prim__tokens):
        lr_rbnf__sep__list__1_reduce = _slot_0
        lcl_0 = prim__tokens.offset
        _off_0 = lcl_0
        lcl_0 = lr_step_rbnf__sep__list__1(lr_rbnf__sep__list__1_reduce, prim__state, prim__tokens)
        lr_rbnf__sep__list__1_try = lcl_0
        lcl_0 = lr_rbnf__sep__list__1_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = prim__tokens.offset
            _off_0 = lcl_1
            lcl_1 = lr_rbnf__sep__list__1_try[1]
            lcl_1 = lcl_1
            lr_rbnf__sep__list__1_reduce = lcl_1
            lcl_1 = lr_step_rbnf__sep__list__1(lr_rbnf__sep__list__1_reduce, prim__state, prim__tokens)
            lr_rbnf__sep__list__1_try = lcl_1
            lcl_1 = lr_rbnf__sep__list__1_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = prim__tokens.offset
        lcl_0 = (lcl_0 is _off_0)
        if lcl_0:
            lcl_1 = (True, lr_rbnf__sep__list__1_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = lr_rbnf__sep__list__1_try
        return lcl_0

    def parse_START(prim__state, prim__tokens):
        lcl_0 = 0
        try:
            _py_local_tk = prim__tokens.array[prim__tokens.offset]
            if (_py_local_tk.idint is lcl_0):
                prim__tokens.offset += 1
            else:
                _py_local_tk = None
        except IndexError:
            _py_local_tk = None
        lcl_0 = _py_local_tk
        _slot_0 = lcl_0
        lcl_0 = (_slot_0 is None)
        if lcl_0:
            lcl_1 = prim__tokens.offset
            lcl_1 = (lcl_1, 'BOF not match')
            lcl_1 = prim__cons(lcl_1, prim__nil)
            lcl_1 = lcl_1
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = parse_stmt(prim__state, prim__tokens)
            _slot_1_check = lcl_1
            lcl_1 = _slot_1_check[0]
            lcl_1 = (lcl_1 is False)
            if lcl_1:
                lcl_1 = _slot_1_check
            else:
                lcl_2 = _slot_1_check[1]
                lcl_2 = lcl_2
                _slot_1 = lcl_2
                lcl_2 = 1
                try:
                    _py_local_tk = prim__tokens.array[prim__tokens.offset]
                    if (_py_local_tk.idint is lcl_2):
                        prim__tokens.offset += 1
                    else:
                        _py_local_tk = None
                except IndexError:
                    _py_local_tk = None
                lcl_2 = _py_local_tk
                _slot_2 = lcl_2
                lcl_2 = (_slot_2 is None)
                if lcl_2:
                    lcl_3 = prim__tokens.offset
                    lcl_3 = (lcl_3, 'EOF not match')
                    lcl_3 = prim__cons(lcl_3, prim__nil)
                    lcl_3 = lcl_3
                    lcl_3 = (False, lcl_3)
                    lcl_2 = lcl_3
                else:
                    lcl_3 = _slot_1
                    _slot_local__1 = lcl_3
                    lcl_3 = (True, _slot_local__1)
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def parse_filename(prim__state, prim__tokens):
        lcl_0 = 7
        try:
            _py_local_tk = prim__tokens.array[prim__tokens.offset]
            if (_py_local_tk.idint is lcl_0):
                prim__tokens.offset += 1
            else:
                _py_local_tk = None
        except IndexError:
            _py_local_tk = None
        lcl_0 = _py_local_tk
        _slot_0 = lcl_0
        lcl_0 = (_slot_0 is None)
        if lcl_0:
            lcl_1 = prim__tokens.offset
            lcl_1 = (lcl_1, 'QuotedStr not match')
            lcl_1 = prim__cons(lcl_1, prim__nil)
            lcl_1 = lcl_1
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = _slot_0
            lcl_1 = lcl_1.value
            lcl_1 = unesc(lcl_1)
            _slot_local__1 = lcl_1
            lcl_1 = (True, _slot_local__1)
            lcl_0 = lcl_1
        return lcl_0

    def parse_ignorable(prim__state, prim__tokens):
        lcl_0 = prim__tokens.offset
        _off_0 = lcl_0
        lcl_0 = (len(prim__tokens.array) > (prim__tokens.offset + 0))
        if lcl_0:
            lcl_2 = prim__tokens.array[(prim__tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 7):
                _py_local_i = prim__tokens.offset
                _py_local_t = prim__tokens.array[_py_local_i]
                prim__tokens.offset = (_py_local_i + 1)
                lcl_3 = _py_local_t
                _slot_0 = lcl_3
                lcl_3 = _slot_0
                lcl_3 = lcl_3.value
                lcl_3 = literal(lcl_3)
                _slot_local__1 = lcl_3
                lcl_3 = (True, _slot_local__1)
                lcl_1 = lcl_3
            elif (lcl_2 == 8):
                _py_local_i = prim__tokens.offset
                _py_local_t = prim__tokens.array[_py_local_i]
                prim__tokens.offset = (_py_local_i + 1)
                lcl_3 = _py_local_t
                _slot_0 = lcl_3
                lcl_3 = _slot_0
                lcl_3 = lcl_3.value
                _slot_local__1 = lcl_3
                lcl_3 = (True, _slot_local__1)
                lcl_1 = lcl_3
            else:
                lcl_3 = (_off_0, 'ignorable lookahead failed')
                lcl_3 = prim__cons(lcl_3, prim__nil)
                lcl_3 = lcl_3
                lcl_3 = (False, lcl_3)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        else:
            lcl_1 = (_off_0, 'ignorable got EOF')
            lcl_1 = prim__cons(lcl_1, prim__nil)
            lcl_1 = lcl_1
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0

    def parse_ignore(prim__state, prim__tokens):
        lcl_0 = 3
        try:
            _py_local_tk = prim__tokens.array[prim__tokens.offset]
            if (_py_local_tk.idint is lcl_0):
                prim__tokens.offset += 1
            else:
                _py_local_tk = None
        except IndexError:
            _py_local_tk = None
        lcl_0 = _py_local_tk
        _slot_0 = lcl_0
        lcl_0 = (_slot_0 is None)
        if lcl_0:
            lcl_1 = prim__tokens.offset
            lcl_1 = (lcl_1, 'quote %ignore not match')
            lcl_1 = prim__cons(lcl_1, prim__nil)
            lcl_1 = lcl_1
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = parse_rbnf__sep__list__0(prim__state, prim__tokens)
            _slot_1_check = lcl_1
            lcl_1 = _slot_1_check[0]
            lcl_1 = (lcl_1 is False)
            if lcl_1:
                lcl_1 = _slot_1_check
            else:
                lcl_2 = _slot_1_check[1]
                lcl_2 = lcl_2
                _slot_1 = lcl_2
                lcl_2 = _slot_0
                lcl_2 = lcl_2.value
                lcl_3 = _slot_1
                lcl_2 = (lcl_2, lcl_3)
                _slot_local__1 = lcl_2
                lcl_2 = (True, _slot_local__1)
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def parse_include(prim__state, prim__tokens):
        lcl_0 = 5
        try:
            _py_local_tk = prim__tokens.array[prim__tokens.offset]
            if (_py_local_tk.idint is lcl_0):
                prim__tokens.offset += 1
            else:
                _py_local_tk = None
        except IndexError:
            _py_local_tk = None
        lcl_0 = _py_local_tk
        _slot_0 = lcl_0
        lcl_0 = (_slot_0 is None)
        if lcl_0:
            lcl_1 = prim__tokens.offset
            lcl_1 = (lcl_1, 'quote %include not match')
            lcl_1 = prim__cons(lcl_1, prim__nil)
            lcl_1 = lcl_1
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = prim__tokens.offset
            _off_1 = lcl_1
            lcl_1 = (len(prim__tokens.array) > (prim__tokens.offset + 0))
            if lcl_1:
                lcl_3 = prim__tokens.array[(prim__tokens.offset + 0)]
                lcl_3 = lcl_3.idint
                if (lcl_3 == 7):
                    lcl_4 = parse_rbnf__sep__list__1(prim__state, prim__tokens)
                    _slot_1_check = lcl_4
                    lcl_4 = _slot_1_check[0]
                    lcl_4 = (lcl_4 is False)
                    if lcl_4:
                        lcl_4 = _slot_1_check
                    else:
                        lcl_5 = _slot_1_check[1]
                        lcl_5 = lcl_5
                        _slot_1 = lcl_5
                        lcl_5 = _slot_0
                        lcl_5 = lcl_5.value
                        lcl_6 = _slot_1
                        lcl_6 = (None, lcl_6)
                        lcl_5 = (lcl_5, lcl_6)
                        _slot_local__1 = lcl_5
                        lcl_5 = (True, _slot_local__1)
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                elif (lcl_3 == 8):
                    _py_local_i = prim__tokens.offset
                    _py_local_t = prim__tokens.array[_py_local_i]
                    prim__tokens.offset = (_py_local_i + 1)
                    lcl_4 = _py_local_t
                    _slot_1 = lcl_4
                    lcl_4 = parse_rbnf__sep__list__1(prim__state, prim__tokens)
                    _slot_2_check = lcl_4
                    lcl_4 = _slot_2_check[0]
                    lcl_4 = (lcl_4 is False)
                    if lcl_4:
                        lcl_4 = _slot_2_check
                    else:
                        lcl_5 = _slot_2_check[1]
                        lcl_5 = lcl_5
                        _slot_2 = lcl_5
                        lcl_5 = _slot_0
                        lcl_5 = lcl_5.value
                        lcl_6 = _slot_1
                        lcl_7 = _slot_2
                        lcl_6 = (lcl_6, lcl_7)
                        lcl_5 = (lcl_5, lcl_6)
                        _slot_local__1 = lcl_5
                        lcl_5 = (True, _slot_local__1)
                        lcl_4 = lcl_5
                    lcl_2 = lcl_4
                else:
                    lcl_4 = (_off_1, 'include lookahead failed')
                    lcl_4 = prim__cons(lcl_4, prim__nil)
                    lcl_4 = lcl_4
                    lcl_4 = (False, lcl_4)
                    lcl_2 = lcl_4
                lcl_1 = lcl_2
            else:
                lcl_2 = (_off_1, 'include got EOF')
                lcl_2 = prim__cons(lcl_2, prim__nil)
                lcl_2 = lcl_2
                lcl_2 = (False, lcl_2)
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def parse_rbnf__sep__list__0(prim__state, prim__tokens):
        lcl_0 = parse_ignorable(prim__state, prim__tokens)
        _slot_0_check = lcl_0
        lcl_0 = _slot_0_check[0]
        lcl_0 = (lcl_0 is False)
        if lcl_0:
            lcl_0 = _slot_0_check
        else:
            lcl_1 = _slot_0_check[1]
            lcl_1 = lcl_1
            _slot_0 = lcl_1
            lcl_1 = _slot_0
            lcl_1 = [lcl_1]
            _slot_local__1 = lcl_1
            lcl_1 = lr_loop_rbnf__sep__list__0(_slot_local__1, prim__state, prim__tokens)
            lcl_0 = lcl_1
        return lcl_0

    def parse_rbnf__sep__list__1(prim__state, prim__tokens):
        lcl_0 = parse_filename(prim__state, prim__tokens)
        _slot_0_check = lcl_0
        lcl_0 = _slot_0_check[0]
        lcl_0 = (lcl_0 is False)
        if lcl_0:
            lcl_0 = _slot_0_check
        else:
            lcl_1 = _slot_0_check[1]
            lcl_1 = lcl_1
            _slot_0 = lcl_1
            lcl_1 = _slot_0
            lcl_1 = [lcl_1]
            _slot_local__1 = lcl_1
            lcl_1 = lr_loop_rbnf__sep__list__1(_slot_local__1, prim__state, prim__tokens)
            lcl_0 = lcl_1
        return lcl_0

    def parse_reserve(prim__state, prim__tokens):
        lcl_0 = 4
        try:
            _py_local_tk = prim__tokens.array[prim__tokens.offset]
            if (_py_local_tk.idint is lcl_0):
                prim__tokens.offset += 1
            else:
                _py_local_tk = None
        except IndexError:
            _py_local_tk = None
        lcl_0 = _py_local_tk
        _slot_0 = lcl_0
        lcl_0 = (_slot_0 is None)
        if lcl_0:
            lcl_1 = prim__tokens.offset
            lcl_1 = (lcl_1, 'quote %reserve not match')
            lcl_1 = prim__cons(lcl_1, prim__nil)
            lcl_1 = lcl_1
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        else:
            lcl_1 = 8
            try:
                _py_local_tk = prim__tokens.array[prim__tokens.offset]
                if (_py_local_tk.idint is lcl_1):
                    prim__tokens.offset += 1
                else:
                    _py_local_tk = None
            except IndexError:
                _py_local_tk = None
            lcl_1 = _py_local_tk
            _slot_1 = lcl_1
            lcl_1 = (_slot_1 is None)
            if lcl_1:
                lcl_2 = prim__tokens.offset
                lcl_2 = (lcl_2, 'Ident not match')
                lcl_2 = prim__cons(lcl_2, prim__nil)
                lcl_2 = lcl_2
                lcl_2 = (False, lcl_2)
                lcl_1 = lcl_2
            else:
                lcl_2 = 8
                try:
                    _py_local_tk = prim__tokens.array[prim__tokens.offset]
                    if (_py_local_tk.idint is lcl_2):
                        prim__tokens.offset += 1
                    else:
                        _py_local_tk = None
                except IndexError:
                    _py_local_tk = None
                lcl_2 = _py_local_tk
                _slot_2 = lcl_2
                lcl_2 = (_slot_2 is None)
                if lcl_2:
                    lcl_3 = prim__tokens.offset
                    lcl_3 = (lcl_3, 'Ident not match')
                    lcl_3 = prim__cons(lcl_3, prim__nil)
                    lcl_3 = lcl_3
                    lcl_3 = (False, lcl_3)
                    lcl_2 = lcl_3
                else:
                    lcl_3 = _slot_0
                    lcl_3 = lcl_3.value
                    lcl_4 = _slot_1
                    lcl_4 = lcl_4.value
                    lcl_5 = _slot_2
                    lcl_5 = lcl_5.value
                    lcl_4 = (lcl_4, lcl_5)
                    lcl_3 = (lcl_3, lcl_4)
                    _slot_local__1 = lcl_3
                    lcl_3 = (True, _slot_local__1)
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0

    def parse_stmt(prim__state, prim__tokens):
        lcl_0 = prim__tokens.offset
        _off_0 = lcl_0
        lcl_0 = (len(prim__tokens.array) > (prim__tokens.offset + 0))
        if lcl_0:
            lcl_2 = prim__tokens.array[(prim__tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 4):
                lcl_3 = parse_reserve(prim__state, prim__tokens)
                _slot_0_check = lcl_3
                lcl_3 = _slot_0_check[0]
                lcl_3 = (lcl_3 is False)
                if lcl_3:
                    lcl_3 = _slot_0_check
                else:
                    lcl_4 = _slot_0_check[1]
                    lcl_4 = lcl_4
                    _slot_0 = lcl_4
                    lcl_4 = _slot_0
                    _slot_local__1 = lcl_4
                    lcl_4 = (True, _slot_local__1)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 5):
                lcl_3 = parse_include(prim__state, prim__tokens)
                _slot_0_check = lcl_3
                lcl_3 = _slot_0_check[0]
                lcl_3 = (lcl_3 is False)
                if lcl_3:
                    lcl_3 = _slot_0_check
                else:
                    lcl_4 = _slot_0_check[1]
                    lcl_4 = lcl_4
                    _slot_0 = lcl_4
                    lcl_4 = _slot_0
                    _slot_local__1 = lcl_4
                    lcl_4 = (True, _slot_local__1)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 3):
                lcl_3 = parse_ignore(prim__state, prim__tokens)
                _slot_0_check = lcl_3
                lcl_3 = _slot_0_check[0]
                lcl_3 = (lcl_3 is False)
                if lcl_3:
                    lcl_3 = _slot_0_check
                else:
                    lcl_4 = _slot_0_check[1]
                    lcl_4 = lcl_4
                    _slot_0 = lcl_4
                    lcl_4 = _slot_0
                    _slot_local__1 = lcl_4
                    lcl_4 = (True, _slot_local__1)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_3 = (_off_0, 'stmt lookahead failed')
                lcl_3 = prim__cons(lcl_3, prim__nil)
                lcl_3 = lcl_3
                lcl_3 = (False, lcl_3)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        else:
            lcl_1 = (_off_0, 'stmt got EOF')
            lcl_1 = prim__cons(lcl_1, prim__nil)
            lcl_1 = lcl_1
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0
    return parse_START