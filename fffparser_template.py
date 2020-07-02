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
    __slots__ = ['tag', "contents"]

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
            # noinspection PyMethodFirstArgAssignment
            self = self.tail
        return l

    def __iter__(self):
        nil = _nil
        while self is not nil:
            yield self.head
            # noinspection PyMethodFirstArgAssignment
            self = self.tail

    def __getitem__(self, n):
        while n != 0:
            # noinspection PyMethodFirstArgAssignment
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
            return pretty_call(
                ctx, Tokens, offset=value.offset, array=value.array)


        @register_pretty(AST)
        def pretty_ast(value, ctx):
            return pretty_call(ctx, AST, tag=value.tag, contents=value.contents)
    mk_pretty()
    del mk_pretty
except ImportError:
    pass

del T, Generic, TypeVar
