import warnings
warnings.filterwarnings('ignore', category=SyntaxWarning, message='"is" with a literal')

from arith_lex import lexer
from arith_parser import mk_parser, Tokens


tokens = list(lexer("<current file>", "1 * -2 + 3 * 4"))
parse = mk_parser()
got = parse(None, Tokens(tokens))
print(got)
