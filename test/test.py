from list_lex import lexer
from list_opt import *
from prettyprinter import pprint
tokens = lexer("a.txt", "aaaaa")
tokens = Tokens(list(tokens))

p = mk_parser()
pprint(p(None, tokens))
