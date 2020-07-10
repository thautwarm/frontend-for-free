# this file is auto-generated by RBNF.hs and the Python package rbnf-rts

from rbnf_rts.rbnf_linker import link
from rbnf_rts.utils import ImmutableMap
from rbnf_rts.lexical import *
__all__ = ['lexicals', 'run_lexer', 'mk_parser']
(lexicals, run_lexer) = lexer(r(number='[+-]?\\d+'), r(space='\\s+'), l['/'], l['-'], l['+'], l['*'], l[')'], l['('], ignores=['space'], reserved_map=ImmutableMap.from_dict({'*': 'quote *', '/': 'quote /', '+': 'quote +', '-': 'quote -', '(': 'quote (', ')': 'quote )'}), numbering={'BOF': 0, 'EOF': 1, 'quote *': 2, 'quote /': 3, 'quote +': 4, 'quote -': 5, 'quote (': 6, 'quote )': 7, 'number': 8, 'space': 9})



def mk_parser(arith, unwrap):
    from rbnf_rts.rts import AST as prim__mk__ast, Cons as prim__cons, _nil as prim__nil

    def lr_step_Add(_slot_0, prim__state, prim__tokens):
        Add_lhs_0 = _slot_0
        lcl_0 = prim__tokens.offset
        _off_0 = lcl_0
        lcl_0 = (len(prim__tokens.array) > (prim__tokens.offset + 0))
        if lcl_0:
            lcl_2 = prim__tokens.array[(prim__tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 5):
                _py_local_i = prim__tokens.offset
                _py_local_t = prim__tokens.array[_py_local_i]
                prim__tokens.offset = (_py_local_i + 1)
                lcl_3 = _py_local_t
                _slot_1 = lcl_3
                Add_op_1 = _slot_1
                lcl_3 = parse_Mul(prim__state, prim__tokens)
                _slot_2_check = lcl_3
                lcl_3 = _slot_2_check[0]
                lcl_3 = (lcl_3 is False)
                if lcl_3:
                    lcl_3 = _slot_2_check
                else:
                    lcl_4 = _slot_2_check[1]
                    lcl_4 = lcl_4
                    _slot_2 = lcl_4
                    Add_rhs_1 = _slot_2
                    lcl_4 = arith(Add_op_1, Add_lhs_0, Add_rhs_1)
                    _slot_local__1 = lcl_4
                    lcl_4 = (True, _slot_local__1)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 4):
                _py_local_i = prim__tokens.offset
                _py_local_t = prim__tokens.array[_py_local_i]
                prim__tokens.offset = (_py_local_i + 1)
                lcl_3 = _py_local_t
                _slot_1 = lcl_3
                Add_op_1 = _slot_1
                lcl_3 = parse_Mul(prim__state, prim__tokens)
                _slot_2_check = lcl_3
                lcl_3 = _slot_2_check[0]
                lcl_3 = (lcl_3 is False)
                if lcl_3:
                    lcl_3 = _slot_2_check
                else:
                    lcl_4 = _slot_2_check[1]
                    lcl_4 = lcl_4
                    _slot_2 = lcl_4
                    Add_rhs_1 = _slot_2
                    lcl_4 = arith(Add_op_1, Add_lhs_0, Add_rhs_1)
                    _slot_local__1 = lcl_4
                    lcl_4 = (True, _slot_local__1)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_3 = (_off_0, 'Add lookahead failed')
                lcl_3 = prim__cons(lcl_3, prim__nil)
                lcl_3 = lcl_3
                lcl_3 = (False, lcl_3)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        else:
            lcl_1 = (_off_0, 'Add got EOF')
            lcl_1 = prim__cons(lcl_1, prim__nil)
            lcl_1 = lcl_1
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0

    def lr_loop_Add(_slot_0, prim__state, prim__tokens):
        lr_Add_reduce = _slot_0
        lcl_0 = prim__tokens.offset
        _off_0 = lcl_0
        lcl_0 = lr_step_Add(lr_Add_reduce, prim__state, prim__tokens)
        lr_Add_try = lcl_0
        lcl_0 = lr_Add_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = prim__tokens.offset
            _off_0 = lcl_1
            lcl_1 = lr_Add_try[1]
            lcl_1 = lcl_1
            lr_Add_reduce = lcl_1
            lcl_1 = lr_step_Add(lr_Add_reduce, prim__state, prim__tokens)
            lr_Add_try = lcl_1
            lcl_1 = lr_Add_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = prim__tokens.offset
        lcl_0 = (lcl_0 is _off_0)
        if lcl_0:
            lcl_1 = (True, lr_Add_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = lr_Add_try
        return lcl_0

    def lr_step_Mul(_slot_0, prim__state, prim__tokens):
        Mul_lhs_0 = _slot_0
        lcl_0 = prim__tokens.offset
        _off_0 = lcl_0
        lcl_0 = (len(prim__tokens.array) > (prim__tokens.offset + 0))
        if lcl_0:
            lcl_2 = prim__tokens.array[(prim__tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 3):
                _py_local_i = prim__tokens.offset
                _py_local_t = prim__tokens.array[_py_local_i]
                prim__tokens.offset = (_py_local_i + 1)
                lcl_3 = _py_local_t
                _slot_1 = lcl_3
                Mul_op_1 = _slot_1
                lcl_3 = parse_Atom(prim__state, prim__tokens)
                _slot_2_check = lcl_3
                lcl_3 = _slot_2_check[0]
                lcl_3 = (lcl_3 is False)
                if lcl_3:
                    lcl_3 = _slot_2_check
                else:
                    lcl_4 = _slot_2_check[1]
                    lcl_4 = lcl_4
                    _slot_2 = lcl_4
                    Mul_rhs_1 = _slot_2
                    lcl_4 = arith(Mul_op_1, Mul_lhs_0, Mul_rhs_1)
                    _slot_local__1 = lcl_4
                    lcl_4 = (True, _slot_local__1)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 2):
                _py_local_i = prim__tokens.offset
                _py_local_t = prim__tokens.array[_py_local_i]
                prim__tokens.offset = (_py_local_i + 1)
                lcl_3 = _py_local_t
                _slot_1 = lcl_3
                Mul_op_1 = _slot_1
                lcl_3 = parse_Atom(prim__state, prim__tokens)
                _slot_2_check = lcl_3
                lcl_3 = _slot_2_check[0]
                lcl_3 = (lcl_3 is False)
                if lcl_3:
                    lcl_3 = _slot_2_check
                else:
                    lcl_4 = _slot_2_check[1]
                    lcl_4 = lcl_4
                    _slot_2 = lcl_4
                    Mul_rhs_1 = _slot_2
                    lcl_4 = arith(Mul_op_1, Mul_lhs_0, Mul_rhs_1)
                    _slot_local__1 = lcl_4
                    lcl_4 = (True, _slot_local__1)
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            else:
                lcl_3 = (_off_0, 'Mul lookahead failed')
                lcl_3 = prim__cons(lcl_3, prim__nil)
                lcl_3 = lcl_3
                lcl_3 = (False, lcl_3)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        else:
            lcl_1 = (_off_0, 'Mul got EOF')
            lcl_1 = prim__cons(lcl_1, prim__nil)
            lcl_1 = lcl_1
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0

    def lr_loop_Mul(_slot_0, prim__state, prim__tokens):
        lr_Mul_reduce = _slot_0
        lcl_0 = prim__tokens.offset
        _off_0 = lcl_0
        lcl_0 = lr_step_Mul(lr_Mul_reduce, prim__state, prim__tokens)
        lr_Mul_try = lcl_0
        lcl_0 = lr_Mul_try[0]
        lcl_0 = (lcl_0 is not False)
        while lcl_0:
            lcl_1 = prim__tokens.offset
            _off_0 = lcl_1
            lcl_1 = lr_Mul_try[1]
            lcl_1 = lcl_1
            lr_Mul_reduce = lcl_1
            lcl_1 = lr_step_Mul(lr_Mul_reduce, prim__state, prim__tokens)
            lr_Mul_try = lcl_1
            lcl_1 = lr_Mul_try[0]
            lcl_1 = (lcl_1 is not False)
            lcl_0 = lcl_1
        lcl_0 = prim__tokens.offset
        lcl_0 = (lcl_0 is _off_0)
        if lcl_0:
            lcl_1 = (True, lr_Mul_reduce)
            lcl_0 = lcl_1
        else:
            lcl_0 = lr_Mul_try
        return lcl_0

    def parse_Add(prim__state, prim__tokens):
        lcl_0 = parse_Mul(prim__state, prim__tokens)
        _slot_0_check = lcl_0
        lcl_0 = _slot_0_check[0]
        lcl_0 = (lcl_0 is False)
        if lcl_0:
            lcl_0 = _slot_0_check
        else:
            lcl_1 = _slot_0_check[1]
            lcl_1 = lcl_1
            _slot_0 = lcl_1
            Add_a_0 = _slot_0
            _slot_local__1 = Add_a_0
            lcl_1 = lr_loop_Add(_slot_local__1, prim__state, prim__tokens)
            lcl_0 = lcl_1
        return lcl_0

    def parse_Atom(prim__state, prim__tokens):
        lcl_0 = prim__tokens.offset
        _off_0 = lcl_0
        lcl_0 = (len(prim__tokens.array) > (prim__tokens.offset + 0))
        if lcl_0:
            lcl_2 = prim__tokens.array[(prim__tokens.offset + 0)]
            lcl_2 = lcl_2.idint
            if (lcl_2 == 6):
                _py_local_i = prim__tokens.offset
                _py_local_t = prim__tokens.array[_py_local_i]
                prim__tokens.offset = (_py_local_i + 1)
                lcl_3 = _py_local_t
                _slot_0 = lcl_3
                lcl_3 = parse_Add(prim__state, prim__tokens)
                _slot_1_check = lcl_3
                lcl_3 = _slot_1_check[0]
                lcl_3 = (lcl_3 is False)
                if lcl_3:
                    lcl_3 = _slot_1_check
                else:
                    lcl_4 = _slot_1_check[1]
                    lcl_4 = lcl_4
                    _slot_1 = lcl_4
                    Atom_a_1 = _slot_1
                    lcl_4 = 7
                    try:
                        _py_local_tk = prim__tokens.array[prim__tokens.offset]
                        if (_py_local_tk.idint is lcl_4):
                            prim__tokens.offset += 1
                        else:
                            _py_local_tk = None
                    except IndexError:
                        _py_local_tk = None
                    lcl_4 = _py_local_tk
                    _slot_2 = lcl_4
                    lcl_4 = (_slot_2 is None)
                    if lcl_4:
                        lcl_5 = prim__tokens.offset
                        lcl_5 = (lcl_5, 'quote ) not match')
                        lcl_5 = prim__cons(lcl_5, prim__nil)
                        lcl_5 = lcl_5
                        lcl_5 = (False, lcl_5)
                        lcl_4 = lcl_5
                    else:
                        _slot_local__1 = Atom_a_1
                        lcl_5 = (True, _slot_local__1)
                        lcl_4 = lcl_5
                    lcl_3 = lcl_4
                lcl_1 = lcl_3
            elif (lcl_2 == 8):
                _py_local_i = prim__tokens.offset
                _py_local_t = prim__tokens.array[_py_local_i]
                prim__tokens.offset = (_py_local_i + 1)
                lcl_3 = _py_local_t
                _slot_0 = lcl_3
                Atom_a_1 = _slot_0
                lcl_3 = unwrap(Atom_a_1)
                _slot_local__1 = lcl_3
                lcl_3 = (True, _slot_local__1)
                lcl_1 = lcl_3
            else:
                lcl_3 = (_off_0, 'Atom lookahead failed')
                lcl_3 = prim__cons(lcl_3, prim__nil)
                lcl_3 = lcl_3
                lcl_3 = (False, lcl_3)
                lcl_1 = lcl_3
            lcl_0 = lcl_1
        else:
            lcl_1 = (_off_0, 'Atom got EOF')
            lcl_1 = prim__cons(lcl_1, prim__nil)
            lcl_1 = lcl_1
            lcl_1 = (False, lcl_1)
            lcl_0 = lcl_1
        return lcl_0

    def parse_Mul(prim__state, prim__tokens):
        lcl_0 = parse_Atom(prim__state, prim__tokens)
        _slot_0_check = lcl_0
        lcl_0 = _slot_0_check[0]
        lcl_0 = (lcl_0 is False)
        if lcl_0:
            lcl_0 = _slot_0_check
        else:
            lcl_1 = _slot_0_check[1]
            lcl_1 = lcl_1
            _slot_0 = lcl_1
            Mul_a_0 = _slot_0
            _slot_local__1 = Mul_a_0
            lcl_1 = lr_loop_Mul(_slot_local__1, prim__state, prim__tokens)
            lcl_0 = lcl_1
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
            lcl_1 = parse_Add(prim__state, prim__tokens)
            _slot_1_check = lcl_1
            lcl_1 = _slot_1_check[0]
            lcl_1 = (lcl_1 is False)
            if lcl_1:
                lcl_1 = _slot_1_check
            else:
                lcl_2 = _slot_1_check[1]
                lcl_2 = lcl_2
                _slot_1 = lcl_2
                START_a_1 = _slot_1
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
                    _slot_local__1 = START_a_1
                    lcl_3 = (True, _slot_local__1)
                    lcl_2 = lcl_3
                lcl_1 = lcl_2
            lcl_0 = lcl_1
        return lcl_0
    return parse_START