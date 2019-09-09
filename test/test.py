import operator
from dataclasses import dataclass
from typing import Generic, TypeVar
T = TypeVar('T')

nil = None


class Nil:
    def __init__(self):
        global nil
        if nil is None:
            nil = self
            return
        raise ValueError("Nil cannot get instantiated twice.")

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
        return "[]"


Nil()


class Cons:
    def __init__(self, _head, _tail):
        self.head = _head
        self.tail = _tail

    def __len__(self):
        _nil = nil
        l = 0
        while self is not _nil:
            l += 1
            self = self.tail
        return l

    def __iter__(self):
        _nil = nil
        while self is not _nil:
            yield self.head
            self = self.tail

    def __getitem__(self, n):
        while n != 0:
            self = self.tail
        return self.head

    def __repr__(self):
        return repr(list(self))


# ast=. {}
# linkedlist=a. {}
# tokens=. {offset:int}
# prim__eq::forall a. (a * a) -> bool
# prim__not__eq::forall a. (a * a) -> bool
# prim__null::forall a. a
# prim__peekable::(tokens * int) -> bool
# prim__peek::(tokens * int) -> token
# prim__match__tk::(tokens * int) -> ast
# prim__tk__id::str -> int
# prim__reset::(tokens * int) -> ()
# prim__cons::forall a. (a * linkedlist a) -> linkedlist a
# prim__nil::forall a. linkedlist a
# prim__to__errs::any -> linkedlist (int * str)
# prim__to__result::any -> ast
# prim__to__any::forall a. a -> any
# prim__mk__ast::forall a. (str * a) -> ast
# prim__is__null::forall a. a -> bool
# always__true::State -> bool

@dataclass
class Token:
    idint: int


prim__eq = operator.eq
prim__not__eq = operator.ne
prim__null = None


def prim__peekable(tokens, i):
    return len(tokens.array) > tokens.offset + i


def prim__peek(tokens, i):
    return tokens.array[tokens.offset + i]


def prim__match__tk(tokens, idint):
    # print(tokens.offset)
    try:
        tk = tokens.array[tokens.offset]
    except IndexError:
        return None
    if tk.idint is idint:
        tokens.offset += 1
    return tk


class Count(dict):
    def __missing__(self, k):
        v = self[k] = len(self)
        return v
token_cnt = Count()

def prim__tk__id(s):
    return token_cnt[s]


def prim__reset(tokens, i):
    tokens.offset = i


prim__cons = Cons
prim__nil = nil


def prim__to__result(x): return x


def prim__to__any(x): return x


@dataclass
class AST(Generic[T]):
    tag: str
    content: T


def prim__mk__ast(s, x): return AST(s, x)


def prim__is__null(x): return x is None


def prim__is__not__null(x): return x is not None


def always__true(s): return True


class Tokens:
    def __init__(self, array):
        self.array = array
        self.offset = 0

with open("./gen.py") as f:
    exec(f.read(), globals())


bT = Token(token_cnt["b"])
tokens = Tokens([bT] * 4)

assert parse_B(None, tokens) == AST(
    tag='B',
    content=(
        (AST(
            tag='B',
            content=(
                (AST(tag='B',
                     content=((
                         AST(tag='B',
                             content=(Token(idint=0),)),
                         Token(idint=0)),)),
                 Token(idint=0)),)),
         Token(idint=0)),))


with open("./gen2.py") as f:
    exec(f.read(), globals())

number = Token(token_cnt["number"])
lp = Token(token_cnt["quote ("])
rp = Token(token_cnt["quote )"])

mult = Token(token_cnt["quote *"])
div = Token(token_cnt["quote /"])

tokens = Tokens([
    number
    # lp, number, rp, mult,
    # number, mult,
    # lp,
    #     number, div,
    #     lp,
    #         number, mult, number, mult, number,
    #     rp,
    # rp
])

print(parse_Atom(None, tokens))