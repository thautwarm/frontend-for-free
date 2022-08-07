from typing import TypeVar, Dict, Optional, Iterable
import pathlib
import copy
import types
ast = types.ModuleType("ast")
ast.__dict__.update({**__import__("ast").__dict__, **__import__("ast_compat").__dict__})

LICENSE = '''
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
'''

T = TypeVar('T')

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


def link(genast: ast.Module, params: str):
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
    @macro_exp("a is b")
    def builtin_eq(a, b):
        return subst(a=a, b=b)

    @opt.register
    @macro_exp("a is not b")
    def builtin_not_eq(a, b):
        return subst(a=a, b=b)
    
    @opt.register
    @macro_stmt("""
try:
    _rbnf_cur_token = tokens.array[tokens.offset]
    if _rbnf_cur_token.idint is idint:
        tokens.offset += 1
    else:
        _rbnf_cur_token  = None
except IndexError:
    _rbnf_cur_token = None
""", "_rbnf_cur_token")
    def builtin_match_tk(tokens, idint):
        return subst(tokens=tokens, idint=idint)

    @opt.register
    @macro_stmt("""
try:
    tokens.array[tokens.offset + i]
    _rbnf_peek_tmp = True
except IndexError:
    _rbnf_peek_tmp = False
    """,
                ret="_rbnf_peek_tmp")
    def builtin_peekable(tokens, i):
        return subst(tokens=tokens, i=i)

    @opt.register
    @macro_stmt("""
_rbnf_old_offset = tokens.offset
_rbnf_cur_token = tokens.array[_rbnf_old_offset]
tokens.offset = _rbnf_old_offset + 1
    """,
                ret="_rbnf_cur_token")
    def builtin_mv_forward(tokens):
        return subst(tokens=tokens)

    @opt.register
    @macro_exp("tokens.array[tokens.offset + i]")
    def builtin_peek(tokens, i):
        return subst(tokens=tokens, i=i)

    @opt.register
    @macro_exp("x[1]")
    def builtin_to_result(x):
        return subst(x=x)

    @opt.register
    @macro_exp("x")
    def builtin_to_any(x):
        return subst(x=x)
    
    @opt.register
    @macro_exp("x is None")
    def builtin_is_null(x):
        return subst(x=x)

    @opt.register
    @macro_exp("x is not None")
    def builtin_is_not_null(x):
        return subst(x=x)

    genast: ast.Module = opt.visit(genast)
    parser_template = (pathlib.Path(__file__).parent / "fffparser_template.py").open().read()
    imp: ast.Module = ast.parse(f"""
{parser_template}
builtin_cons = Cons
builtin_nil = _nil
builtin_mk_ast = AST
def mk_parser({params}):
    pass
""")
    fn: ast.FunctionDef = imp.body[-1]
    fn.body.extend(genast.body)
    fn.body.append(ast.Return(ast.Name("rbnf_named_parse_START", ast.Load())))
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

    def visit_FunctionDef(self, node: ast.FunctionDef):
        if node.name.startswith("rbnf_named"):
            # it's a function generated by rbnf
            return self.generic_visit(node)
        
        return node

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
        if node.id == "builtin_null":
            return ast.Constant(None)
        return node

def main(require_file, filename, out, *, params:str=''):
    with open(filename) as f:
        genast = ast.parse(f.read())
    
    with open(out, 'w') as f, open(require_file) as r:
        f.write(LICENSE)
        f.write(r.read())
        f.write(ast.unparse(link(genast, params)))

def entry():
    from wisepy2 import wise
    wise(main)()