from list_lex import lexer
from list_opt import *

tokens = lexer("a.txt", "K")
tokens = Tokens(list(tokens))

p = mk_parser()
print(p(None, tokens))
