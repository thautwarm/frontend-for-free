from typing import TypeVar, Dict, Optional, Iterable
import ast
import copy

T = TypeVar('T')


def mangle(x: str):
    return x.replace('_', '__')


class Substitute(ast.NodeTransformer):

    def __init__(self, m: dict):
        self.m = m

    def visit_Name(self, n: ast.Name):
        v = self.m.get(n.id, None)
        if v is None:
            return n
        return v


def subst(**kwargs):
    stor = Substitute(kwargs)
    deepcopy = copy.deepcopy

    def bind_node(node: ast.AST):
        node = deepcopy(node)
        node = stor.visit(node)
        return node

    return bind_node


class Macro:

    def __init__(self, name, f, is_stmt: bool):
        self.name = name
        self.is_stmt = is_stmt
        self.f = f


def macro_exp(template: str):
    template = ast.parse(template)
    exp: ast.Return = template.body[0]
    node = exp.value

    def stor(f):

        def perform_subst(*args):
            return f(*args)(node)

        return Macro(f.__name__, perform_subst, False)

    return stor


def macro_stmt(template: str, ret: Optional[str]):
    assert isinstance(template, str)
    assert isinstance(ret, (str, None.__class__))
    template = ast.parse(template)
    if ret:
        ret = ast.Name(id=ret, ctx=ast.Load())
    stmt = template

    def stor(f):

        def perform_subst(lhs, *args):
            mod: ast.Module = f(*args)(stmt)
            body = mod.body
            if ret and lhs:
                body.append(ast.Assign(targets=lhs, value=ret))
            return body

        return Macro(f.__name__, perform_subst, True)

    return stor


def link(lexicals: Dict[str, int],
         genast: ast.Module,
         requires: Optional[Iterable[str]] = None):
    requires = requires or ()
    if max(lexicals.values()) > 254:
        raise ValueError("Too many lexical names!(exceed 254)")
    opt = Optimizer()

    @opt.register
    @macro_exp("[]")
    def builtin_empty_list():
        return subst()


    @opt.register
    @macro_stmt("""
_rbnf_immediate_lst = a
_rbnf_immediate_lst.append(b)
        """,
                ret="_rbnf_immediate_lst")
    def builtin_push_list(a, b):
        return subst(a=a, b=b)

    @opt.register
    @macro_exp("a.append(b) or a")
    def builtin_push_list(a, b):
        assert isinstance(n, ast.Name)
        assert n.id.startswith("RBNFint")
        n.id = "_slot_{}".format(int(n.id[7:]))
        return subst(a=n)

    @opt.register
    @macro_exp("lcl")
    def RBNFelt(n):
        assert isinstance(n, ast.Name)
        assert n.id.startswith("RBNFint")
        n.id = "_slot_{}".format(int(n.id[7:]))
        return subst(lcl=n)

    @opt.register
    @macro_stmt("""
_py_local_t = a
_py_local_t.append(b)
        """,
                ret="_py_local_t")
    def RBNFappend(a, b):
        return subst(a=a, b=b)

    @opt.register
    @macro_exp("n")
    def RBNFtuple(*args):
        return subst(n=ast.Tuple(elts=list(args), ctx=ast.Load()))

    @opt.register
    @macro_exp("n")
    def RBNFlist(*args):
        return subst(n=ast.List(elts=list(args), ctx=ast.Load()))

    @opt.register
    @macro_exp("a is b")
    def prim__eq(a, b):
        return subst(a=a, b=b)

    @opt.register
    @macro_exp("a is not b")
    def prim__not__eq(a, b):
        return subst(a=a, b=b)

    @opt.register
    @macro_exp("len(tokens.array) > tokens.offset + i")
    def prim__peekable(tokens, i):
        return subst(tokens=tokens, i=i)

    @opt.register
    @macro_stmt("""
_py_local_i = tokens.offset
_py_local_t = tokens.array[_py_local_i]
tokens.offset = _py_local_i + 1
    """,
                ret="_py_local_t")
    def prim__mv__forward(tokens):
        return subst(tokens=tokens)

    @opt.register
    @macro_exp("tokens.array[tokens.offset + i]")
    def prim__peek(tokens, i):
        return subst(tokens=tokens, i=i)

    @opt.register
    @macro_stmt("""
try:
    _py_local_tk = tokens.array[tokens.offset]
    if _py_local_tk.idint is idint:
        tokens.offset += 1
    else:
        _py_local_tk  = None
except IndexError:
    _py_local_tk = None
""", "_py_local_tk")
    def prim__match__tk(tokens, idint):
        # print(tokens.offset)
        return subst(tokens=tokens, idint=idint)

    # should specialize
    @opt.register
    @macro_exp("idint")
    def prim__tk__id(s: ast.Str):
        assert isinstance(s, ast.Str)
        return subst(idint=ast.Constant(lexicals[s.s]))

    @opt.register
    @macro_stmt("tokens.offset = i", None)
    def prim__reset(tokens, i):
        return subst(tokens=tokens, i=i)

    @opt.register
    @macro_exp("x")
    def prim__to__result(x):
        return subst(x=x)

    @opt.register
    @macro_exp("x")
    def prim__to__any(x):
        return subst(x=x)

    @opt.register
    @macro_exp("x is None")
    def prim__is__null(x):
        return subst(x=x)

    @opt.register
    @macro_exp("x is not None")
    def prim__is__not__null(x):
        return subst(x=x)

    genast: ast.Module = opt.visit(genast)
    mangling = '\n    '.join('{} = {}'.format(mangle(req), req) for req in requires if mangle(req) != req)
    imp: ast.Module = ast.parse(f"""
def mk_parser({', '.join(requires)}):
    from rbnf_rts.rts import AST as prim__mk__ast, Cons as prim__cons, _nil as prim__nil
    {mangling}
""")
    fn: ast.FunctionDef = imp.body[0]
    fn.body.extend(genast.body)
    fn.body.append(ast.Return(ast.Name("parse_START", ast.Load())))
    ast.fix_missing_locations(fn)
    return imp


class Optimizer(ast.NodeTransformer):

    def __init__(self):
        self.exprs = {}
        self.stmts = {}

    def register(self, macro: Macro):
        if macro.is_stmt:
            self.stmts[macro.name] = macro
        else:
            self.exprs[macro.name] = macro

    def visit_Assign(self, node: ast.Assign):
        lhs = node.targets
        rhs = node.value
        if isinstance(rhs, ast.Call):
            if isinstance(rhs.func,
                          ast.Name) and rhs.func.id in self.stmts:
                macro = self.stmts[rhs.func.id]
                return macro.f(lhs, *rhs.args)
        return self.generic_visit(node)

    def visit_Expr(self, node: ast.Expr):
        if isinstance(node.value, ast.Call):
            call = node.value
            if isinstance(call.func,
                          ast.Name) and call.func.id in self.stmts:
                macro = self.stmts[call.func.id]
                return macro.f(None, *call.args)
        return self.generic_visit(node)

    def visit_Call(self, node: ast.Call):
        if isinstance(node.func, ast.Name):
            func = node.func
            if func.id in self.exprs:
                macro = self.exprs[func.id]
                return macro.f(*node.args)
        return self.generic_visit(node)

    def visit_Name(self, node: ast.Name):
        if node.id == "prim__null":
            return ast.Constant(None)
        return node