import warnings
warnings.filterwarnings('ignore', category=SyntaxWarning, message='"is" with a literal')
from parser_wrap import parse

data = parse('''
        {
            "empty_object" : {},
            "empty_array"  : [],
            "booleans"     : { "YES" : true, "NO" : false },
            "numbers"      : [ 0, 1, -2, 3.3, 4.4e5, 6.6e-7 ],
            "strings"      : [ "This", [ "And" , "That", "And a \\"b" ] ],
            "nothing"      : null
        }
    ''')

assert isinstance(data, dict)


expected = {
    'empty_object': {}, 'empty_array': [], 'booleans': {'YES': True, 'NO': False},
    'numbers': [0, 1, -2, 3.3, 440000.0, 6.6e-07], 'strings': ['This', ['And', 'That', 'And a "b']], 'nothing': None
}
assert str(data) == str(expected)