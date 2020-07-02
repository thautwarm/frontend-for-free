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