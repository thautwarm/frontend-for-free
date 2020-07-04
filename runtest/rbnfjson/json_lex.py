from warnings import warn

class Token:
    offset: int
    lineno: int
    colno: int
    filename: str
    idint: int
    value: str
    __slots__ = ("offset", "lineno", "colno", "filename", "idint", "value")

    def __init__(self, offset, lineno, colno, filename, type, value):
        self.offset = offset
        self.lineno = lineno
        self.colno = colno
        self.filename = filename
        self.idint = type
        self.value = value

    def __eq__(self, other: "Token"):
        if not isinstance(other, Token):
            return False

        return (
            self.offset == other.offset
            and self.filename == other.filename
            and self.idint == other.idint
            and self.value == other.value
            and self.colno == other.colno
            and self.lineno == other.lineno
        )

    def __hash__(self):
        return (
            (self.offset ^ self.lineno ^ self.colno + 2333 + self.idint)
            ^ hash(self.filename)
            ^ hash(self.value)
        )

    def __repr__(self):
        return (
            "Token(offset=%d, lineno=%d, colno=%d, filename=%s, type=%d, value=%s)"
            % (
                self.offset,
                self.lineno,
                self.colno,
                self.filename,
                self.idint,
                repr(self.value),
            )
        )

def lexer(filename, text):
    text_length = len(text)
    colno = 0
    lineno = 0
    pos = 0
    newline = "\n"
    match = REGEX.match
    ignores = IGNORES
    unionall_info = UNIONALL_INFO
    _Token = Token
    tokens = [_Token(0, 0, 0, filename, BOF, "")]
    append = tokens.append
    while True:
        if text_length <= pos:
            break
        
        res = match(text, pos)
        if not res:
            warn(f"No handler for character `{text[pos].__repr__()}`.")
            ch = text[pos]
            append(Token(pos, lineno, colno, filename, -1, ch))
            if ch == "\n":
                lineno += 1
                colno = 0
            pos += 1
            continue
        pat = res.group()
        typeid, cast_map = unionall_info[res.lastindex]
        if typeid in ignores:
            n = len(pat)
            line_inc = pat.count(newline)
            if line_inc:
                latest_newline_idx = pat.rindex(newline)
                colno = n - latest_newline_idx
                lineno += line_inc
            else:
                colno += n
            pos += n
            continue

        if cast_map:
            typeid = cast_map.get(pat, typeid)
        
        append(_Token(pos, lineno, colno, filename, typeid, pat))
        n = len(pat)
        line_inc = pat.count(newline)
        if line_inc:
            latest_newline_idx = pat.rindex(newline)
            colno = n - latest_newline_idx
            lineno += line_inc
        else:
            colno += n
        pos += n

    append(Token(0, 0, 0, filename, EOF, ""))
    return tokens
EOF = 1
BOF = 0
REGEX = __import__('re').compile('(\\s+)|("([^\\\\"]+|\\\\.)*?")|([-+]?[0-9]+\\.\\d+([eE][-+]?\\d+)?|[-+]?[0-9]+[eE][-+]?\\d+)|([-+]?[0-9]+)|(\\})|(\\{)|(true)|(null)|(false)|(\\])|(\\[)|(:)|(,)')
IGNORES = (14,)
UNIONALL_INFO = ((None, None), (14, None), (2, None), (None, None), (4, None), (None, None), (3, None), (12, None), (11, None), (5, None), (6, None), (7, None), (9, None), (8, None), (13, None), (10, None))
numbering = {'BOF': 0, 'EOF': 1, 'ESCAPED_STRING': 2, 'SIGNED_INT': 3, 'SIGNED_FLOAT': 4, 'quote true': 5, 'quote null': 6, 'quote false': 7, 'quote [': 8, 'quote ]': 9, 'quote ,': 10, 'quote {': 11, 'quote }': 12, 'quote :': 13, 'WS': 14}
