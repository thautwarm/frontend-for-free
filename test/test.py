from arith_lex import lexer
from arith_parser import *
from prettyprinter import pprint
tokens = lexer("a.txt", "1 + ")
tokens = Tokens(list(tokens))

tokens = list(lexer("<current file>", "1 * -2 + 3 * 4"))
parse = mk_parser()
got = parse(None, Tokens(tokens))
print(got)
