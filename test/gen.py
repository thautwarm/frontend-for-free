# This file is automatically generated by RBNF.hs
#token=. {idint:int, colomn:int, lineno:int, val:str, name:str}
#ast=. {}
#linkedlist=a. {}
#tokens=. {offset:int}
#prim__eq::forall a. (a * a) -> bool
#prim__not__eq::forall a. (a * a) -> bool
#prim__null::forall a. a
#prim__peekable::(tokens * int) -> bool
#prim__peek::(tokens * int) -> token
#prim__match__tk::(tokens * int) -> ast
#prim__tk__id::str -> int
#prim__reset::(tokens * int) -> ()
#prim__cons::forall a. (a * linkedlist a) -> linkedlist a
#prim__nil::forall a. linkedlist a
#prim__to__errs::any -> linkedlist (int * str)
#prim__to__result::any -> ast
#prim__to__any::forall a. a -> any
#prim__mk__ast::forall a. (str * a) -> ast
#prim__is__null::forall a. a -> bool
#always__true::State -> bool
def parse_B(prim__state, prim__tokens):
    _tmp_0_flag = False
    lcl_0 = prim__tokens.offset
    _off_0 = lcl_0
    lcl_0 = prim__peekable(prim__tokens, 0)
    if lcl_0:
        lcl_2 = prim__peek(prim__tokens, 0)
        lcl_2 = lcl_2.idint
        if lcl_2 == prim__tk__id(b):
            lcl_2 = prim__tokens.offset
            _off_1 = lcl_2
            lcl_2 = prim__tk__id("b")
            lcl_2 = prim__match__tk(prim__tokens, lcl_2)
            _slot_0 = lcl_2
            lcl_2 = prim__is__null(_slot_0)
            if lcl_2:
                lcl_2 = prim__null
            else:
                lcl_3 = prim__tokens.offset
                _off_2 = lcl_3
                lcl_3 = prim__tk__id("A")
                lcl_3 = prim__match__tk(prim__tokens, lcl_3)
                _slot_1 = lcl_3
                lcl_3 = prim__is__null(_slot_1)
                if lcl_3:
                    lcl_3 = prim__null
                else:
                    lcl_4 = (_slot_0, _slot_1)
                    _slot_local__1 = lcl_4
                    lcl_4 = (_slot_local__1,)
                    lcl_4 = prim__mk__ast("B", lcl_4)
                    _slot_local__2 = lcl_4
                    lcl_3 = _slot_local__2
                lcl_2 = lcl_3
            lcl_1 = lcl_2
        elif lcl_2 == prim__tk__id(A):
            lcl_2 = prim__tokens.offset
            _off_1 = lcl_2
            lcl_3 = prim__tk__id("A")
            lcl_4 = prim__match__tk(prim__tokens, lcl_3)
            _slot_0 = lcl_4
            lcl_2 = prim__is__null(_slot_0)
            if lcl_2:
                lcl_2 = prim__null
            else:
                lcl_2 = (_slot_0,)
                lcl_2 = prim__mk__ast("B", lcl_2)
                _slot_local__1 = lcl_2
                lcl_2 = _slot_local__1
            lcl_1 = lcl_2
        else:
            _tmp_0_flag = True
            lcl_1 = prim__null
        lcl_0 = lcl_1
    else:
        lcl_0 = prim__null
    _tmp_0_result = lcl_0
    lcl_0 = prim__is__null(_tmp_0_result)
    lcl_1 = lcl_0 or _tmp_0_flag
    if lcl_1:
        lcl_0 = _tmp_0_result
    else:
        lcl_1 = prim__reset(prim__tokens, _off_0)
        lcl_0 = prim__null
    return lcl_0
