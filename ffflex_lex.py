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

def lexer(filename, text: str, *, pos=0, use_bof=True, use_eof=True):
    text_length = len(text)
    colno = 0
    lineno = 0
    newline = "\n"
    match = REGEX_STR.match
    ignores = IGNORES
    unionall_info = UNIONALL_INFO
    _Token = Token
    tokens = []
    append = tokens.append
    if use_bof:
        append(_Token(0, 0, 0, filename, BOF, ""))
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

    if use_eof:
        append(Token(pos, lineno, colno, filename, EOF, ""))
    return tokens

def lexer_lazy_bytes(filename, text: bytes, *, pos=0, use_bof=True, use_eof=True):
    text_length = len(text)
    colno = 0
    lineno = 0
    match = REGEX_BYTES.match
    ignores = IGNORES
    unionall_info = UNIONALL_INFO_BYTES
    _Token = Token
    if use_bof:
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

    if use_eof:
        yield _Token(pos, lineno, colno, filename, EOF, "")

EOF = 1
BOF = 0
REGEX = '(\\s+)|("([^\\\\\\"]+|\\\\.)*?"|\'([^\\\\\\\']+|\\\\.)*?\')|([a-zA-Z_][a-zA-Z0-9_]*)|(,)|(%reserve)|(%include)|(%ignore)'
REGEX_STR = __import__('re').compile(REGEX)
REGEX_BYTES = __import__('re').compile(REGEX.encode())
IGNORES = (8,)
UNIONALL_INFO = ((None, None), (8, None), (2, None), (None, None), (None, None), (3, None), (4, None), (6, None), (7, None), (5, None))
UNIONALL_INFO_BYTES = ((None, None), (8, None), (2, None), (None, None), (None, None), (3, None), (4, None), (6, None), (7, None), (5, None))
numbering = {'BOF': 0, 'EOF': 1, 'QuotedStr': 2, 'Ident': 3, 'quote ,': 4, 'quote %ignore': 5, 'quote %reserve': 6, 'quote %include': 7, 'WS': 8}
