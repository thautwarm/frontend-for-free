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
#prim__is__not__null::forall a. a -> bool
#always__true::State -> bool
def lr_step_B(_slot_0, prim__state, prim__tokens):
    lcl_0 = prim__tokens.offset
    _off_0 = lcl_0
    lcl_0 = prim__tk__id("b")
    lcl_0 = prim__match__tk(prim__tokens, lcl_0)
    _slot_1 = lcl_0
    lcl_0 = prim__is__null(_slot_1)
    if lcl_0:
        lcl_0 = prim__null
    else:
        lcl_1 = (_slot_0, _slot_1)
        _slot_local__1 = lcl_1
        lcl_1 = (_slot_local__1,)
        lcl_1 = prim__mk__ast("B", lcl_1)
        _slot_local__2 = lcl_1
        lcl_0 = _slot_local__2
    return lcl_0
def lr_loop_B(_slot_0, prim__state, prim__tokens):
    lr_B_reduce = _slot_0
    lcl_0 = prim__tokens.offset
    _off_0 = lcl_0
    lcl_0 = lr_step_B(lr_B_reduce, prim__state, prim__tokens)
    lr_B_try = lcl_0
    lcl_0 = prim__is__not__null(lr_B_try)
    while lcl_0:
        lcl_0 = prim__tokens.offset
        _off_0 = lcl_0
        lr_B_reduce = lr_B_try
        lcl_0 = lr_step_B(lr_B_reduce, prim__state, prim__tokens)
        lr_B_try = lcl_0
    lcl_0 = prim__reset(prim__tokens, _off_0)
    return lr_B_reduce
def parse_B(prim__state, prim__tokens):
    lcl_0 = prim__tokens.offset
    _off_0 = lcl_0
    lcl_0 = prim__tk__id("b")
    lcl_0 = prim__match__tk(prim__tokens, lcl_0)
    _slot_0 = lcl_0
    lcl_0 = prim__is__null(_slot_0)
    if lcl_0:
        lcl_0 = prim__null
    else:
        lcl_1 = (_slot_0,)
        lcl_1 = prim__mk__ast("B", lcl_1)
        _slot_local__1 = lcl_1
        lcl_1 = lr_loop_B(_slot_local__1, prim__state, prim__tokens)
        lcl_0 = lcl_1
    return lcl_0