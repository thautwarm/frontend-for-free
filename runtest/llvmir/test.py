import warnings
warnings.filterwarnings('ignore', category=SyntaxWarning, message='"is" with a literal')

from llvmir_lex import lexer, numbering
from llvmir_parser import mk_parser, Tokens


parse = mk_parser()

sources = [
    "@gg = constant void bitcast (i8* 3 to i16*)",
    "%a = type {i8*, i1}", """
define i8 @f (void){
    %i = getelementptr i32*, i32* %j, i32 1
    ret void
}

define i32* @f(void (void*)* %g){
    %a = alloca i8, align 4
    %b = bitcast i8* %a to void*
    ret i32* %add

}
"""
]
print(numbering["quote constant"])
from prettyprinter import pprint
for src in sources:

    tokens = list(lexer("<unknown filename>", src))
    pprint(parse(None, Tokens(tokens)))
