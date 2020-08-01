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

def lexer(filename, text: str, pos=0):
    text_length = len(text)
    colno = 0
    lineno = 0
    newline = "\n"
    match = REGEX_STR.match
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

    append(Token(pos, lineno, colno, filename, EOF, ""))
    return tokens

def lexer_lazy_bytes(filename, text: bytes, pos=0):
    text_length = len(text)
    colno = 0
    lineno = 0
    match = REGEX_BYTES.match
    ignores = IGNORES
    unionall_info = UNIONALL_INFO_BYTES
    _Token = Token
    yield _Token(0, 0, 0, filename, BOF, b"")
    
    while True:
        if text_length <= pos:
            break
        
        res = match(text, pos)
        if not res:
            warn(f"No handler for character `{str(text[pos]).__repr__()}`.")
            ch = text[pos]
            yield _Token(pos, lineno, colno, filename, -1, ch)
            if ch == b'\n':
                lineno += 1
                colno = 0
            pos += 1
            continue
        pat = res.group()
        typeid, cast_map = unionall_info[res.lastindex]
        if typeid in ignores:
            n = len(pat)
            line_inc = pat.count(b'\n')
            if line_inc:
                latest_newline_idx = pat.rindex(b'\n')
                colno = n - latest_newline_idx
                lineno += line_inc
            else:
                colno += n
            pos += n
            continue

        if cast_map:
            typeid = cast_map.get(pat, typeid)
        
        yield _Token(pos, lineno, colno, filename, typeid, pat)
        n = len(pat)
        line_inc = pat.count(b'\n')
        if line_inc:
            latest_newline_idx = pat.rindex(b'\n')
            colno = n - latest_newline_idx
            lineno += line_inc
        else:
            colno += n
        pos += n

    yield _Token(pos, lineno, colno, filename, EOF, "")
EOF = 26
BOF = 25
REGEX = '(\\s+)|("([^\\\\\\"]+|\\\\.)*?"|\'([^\\\\\\\']+|\\\\.)*?\')|(\\d+)|([a-zA-Z_][a-zA-Z0-9_]*)|(\\#[\\w|\\W]*?\\n)|(%%inline[\\w|\\W]*?%%)|(\\})|(\\|)|(\\{)|(\\])|(\\[)|(\\?)|(>)|(=)|(<=>)|(<)|(;)|(:=)|(::=)|(:)|(\\.)|(,)|(\\))|(\\()|(%parametric)|(%include)|(\\$)'
REGEX_STR = __import__('re').compile(REGEX)
REGEX_BYTES = __import__('re').compile(REGEX.encode())
IGNORES = (27, 28)
UNIONALL_INFO = ((None, None), (27, None), (2, None), (None, None), (None, None), (19, None), (1, None), (28, None), (24, None), (13, None), (11, None), (12, None), (8, None), (7, None), (10, None), (4, None), (9, None), (17, None), (3, None), (18, None), (15, None), (16, None), (14, None), (21, None), (0, None), (6, None), (5, None), (23, None), (22, None), (20, None))
UNIONALL_INFO_BYTES = ((None, None), (27, None), (2, None), (None, None), (None, None), (19, None), (1, None), (28, None), (24, None), (13, None), (11, None), (12, None), (8, None), (7, None), (10, None), (4, None), (9, None), (17, None), (3, None), (18, None), (15, None), (16, None), (14, None), (21, None), (0, None), (6, None), (5, None), (23, None), (22, None), (20, None))
numbering = {'quote ,': 0, 'Ident': 1, 'QuotedStr': 2, 'quote <': 3, 'quote >': 4, 'quote (': 5, 'quote )': 6, 'quote [': 7, 'quote ]': 8, 'quote =': 9, 'quote ?': 10, 'quote |': 11, 'quote {': 12, 'quote }': 13, 'quote :': 14, 'quote :=': 15, 'quote ::=': 16, 'quote <=>': 17, 'quote ;': 18, 'Int': 19, 'quote $': 20, 'quote .': 21, 'quote %include': 22, 'quote %parametric': 23, 'Code': 24, 'BOF': 25, 'EOF': 26, 'WS': 27, 'comment': 28}
