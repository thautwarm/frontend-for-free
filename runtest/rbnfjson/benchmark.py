from lark import Lark, Transformer, v_args
from timeit import timeit

## Lark
json_grammar = r"""
    ?start: value
    ?value: object
          | array
          | string
          | SIGNED_NUMBER      -> number
          | "true"             -> true
          | "false"            -> false
          | "null"             -> null
    array  : "[" [value ("," value)*] "]"
    object : "{" [pair ("," pair)*] "}"
    pair   : string ":" value
    string : ESCAPED_STRING
    %import common.ESCAPED_STRING
    %import common.SIGNED_NUMBER
    %import common.WS
    %ignore WS
"""


class TreeToJson(Transformer):
    @v_args(inline=True)
    def string(self, s):
        return s[1:-1].replace('\\"', '"')

    array = list
    pair = tuple
    object = dict
    number = v_args(inline=True)(float)

    null = lambda self, _: None
    true = lambda self, _: True
    false = lambda self, _: False


### Create the JSON parser with Lark, using the Earley algorithm
# json_parser = Lark(json_grammar, parser='earley', lexer='standard')
# def parse(x):
#     return TreeToJson().transform(json_parser.parse(x))

### Create the JSON parser with Lark, using the LALR algorithm
json_parser = Lark(
    json_grammar,
    parser="lalr",
    # Using the standard lexer isn't required, and isn't usually recommended.
    # But, it's good enough for JSON, and it's slightly faster.
    lexer="standard",  # Disabling propagate_positions and placeholders slightly improves speed
    propagate_positions=False,
    maybe_placeholders=False,
    # Using an internal transformer is faster and more memory efficient
    transformer=TreeToJson(),
)
lark_parse = json_parser.parse

## RBNF.hs
from runtest.rbnfjson.parser_wrap import parse

## Python JSON
from json import loads

text = """
{
            "empty_object" : {},
            "empty_array"  : [],
            "booleans"     : { "YES" : true, "NO" : false },
            "numbers"      : [ 0, 1, -2, 3.3, 4.4e5, 6.6e-7 ],
            "strings"      : [ "This", [ "And" , "That", "And a \\"b" ] ],
            "nothing"      : null
}
"""


assert loads(text) == parse(text) == lark_parse(text)
print(timeit("parse(text)", globals=dict(text=text, parse=loads), number=10000))
print(timeit("parse(text)", globals=dict(text=text, parse=parse), number=10000))
print(timeit("parse(text)", globals=dict(text=text, parse=lark_parse), number=10000))
