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



def mk():
    from warnings import warn
    import re
    import abc
    import typing as t
    Lexer = t.Callable[[str, int], t.Optional[t.Tuple[int, str]]]
    class LexerDescriptor(abc.ABC):
        typeid: int

        @abc.abstractmethod
        def to_lexer(self) -> Lexer:
            return self.to_lexer()


    class RegexLexerDescriptor(LexerDescriptor):
        def __init__(self, typeid: int, regex_pat: t.Pattern):
            self.typeid = typeid
            self.regex_pat = regex_pat

        def to_lexer(self):
            match = self.regex_pat.match
            typeid = self.typeid

            def lex(string, pos):
                m = match(string, pos)
                if m:
                    return m.group()

            return typeid, lex


    class LiteralLexerDescriptor(LexerDescriptor):
        def __init__(self, typeid: int, *string_pats: str):
            assert string_pats
            self.typeid = typeid
            self.contents = string_pats

        def to_lexer(self):
            pats = self.contents
            typeid = self.typeid
            if len(pats) is 1:
                pats = pats[0]

                def lex(string: str, pos):
                    if string.startswith(pats, pos):
                        return pats

            else:

                def lex(string: str, pos):
                    for pat in pats:
                        if string.startswith(pat, pos):
                            return pat

            return typeid, lex


    def lexer_reduce(lexer_descriptors: t.List[LexerDescriptor]) -> t.List[LexerDescriptor]:
        def _chunk(stream: t.Iterable[LexerDescriptor]):
            grouped = []
            _append = grouped.append
            last = None
            for _e in stream:
                e = type(_e), _e.typeid
                if last is None:
                    grouped = [_e]
                    _append = grouped.append
                elif last == e:
                    _append(_e)
                else:
                    yield (last, grouped)
                    grouped = [_e]
                    _append = grouped.append
                last = e
            else:
                if last:
                    yield (last, grouped)

        groups = list(_chunk(lexer_descriptors))

        ret = []
        for (lexer_type, typeid), descriptors in groups:
            if lexer_type is RegexLexerDescriptor:
                ret.extend(descriptors)
            else:
                assert lexer_type is LiteralLexerDescriptor
                descriptors: t.List[LiteralLexerDescriptor]
                contents = sum(tuple(each.contents for each in descriptors), ())

                ret.append(LiteralLexerDescriptor(typeid, *contents))

        return ret


    def lexing(filename: str, text: str, lexer_table: list, reserved_map: dict, Token=Token, len=len):
        text_length = len(text)
        colno = 0
        lineno = 0
        pos = 0
        newline = "\n"

        while True:
            if text_length <= pos:
                break

            for typeid, case in lexer_table:
                origin_word = case(text, pos)

                if origin_word is None:
                    continue
                pat = origin_word
                casted_typeid = reserved_map.get(pat)
                if casted_typeid is None:
                    yield Token(pos, lineno, colno, filename, typeid, origin_word)
                else:
                    yield Token(pos, lineno, colno, filename, casted_typeid, origin_word)
                n = len(pat)
                line_inc = pat.count(newline)
                if line_inc:
                    latest_newline_idx = pat.rindex(newline)
                    colno = n - latest_newline_idx
                    lineno += line_inc
                else:
                    colno += n
                pos += n
                break

            else:
                warn(f"No handler for character `{text[pos].__repr__()}`.")
                ch = text[pos]
                origin_word = ch
                yield Token(pos, lineno, colno, filename, -1, origin_word)
                if ch == "\n":
                    lineno += 1
                    colno = 0
                pos += 1

    def lexing_no_reserve(filename: str, text: str, lexer_table: list, Token=Token, len=len):
        text_length = len(text)
        colno = 0
        lineno = 0
        pos = 0
        newline = "\n"

        while True:
            if text_length <= pos:
                break

            for typeid, case in lexer_table:
                origin_word = case(text, pos)

                if origin_word is None:
                    continue
                pat = origin_word
                yield Token(pos, lineno, colno, filename, typeid, origin_word)

                n = len(pat)
                line_inc = pat.count(newline)
                if line_inc:
                    latest_newline_idx = pat.rindex(newline)
                    colno = n - latest_newline_idx
                    lineno += line_inc
                else:
                    colno += n
                pos += n
                break

            else:
                warn(f"No handler for character `{text[pos].__repr__()}`.")
                ch = text[pos]
                origin_word = ch
                yield Token(pos, lineno, colno, filename, -1, origin_word)
                if ch == "\n":
                    lineno += 1
                    colno = 0
                pos += 1


    def reg_rule(i, a, b):
        b = re.compile(b)
        return i, a, b

    def lit_rule(i, a, b):
        return i, a, b


    def mk_lexer(
        *subrules: t.Tuple[int, str, t.Union[re.Pattern, str]],
        ignores=(),
        reserved_map: dict = None,
    ):

        reserved_map = reserved_map or {}
        BOF = 0
        EOF = 1
        numbering = {"BOF": BOF, "EOF": EOF}
        sublexers = []

        for number, name, each in subrules:
            numbering[name] = number
            if isinstance(each, str):
                sublexers.append(LiteralLexerDescriptor(number, each))
            elif isinstance(each, re.Pattern):
                sublexers.append(RegexLexerDescriptor(number, each))
            else:
                raise TypeError(type(each))

        table = [e.to_lexer() for e in sublexers]

        reserved_map = {k: numbering[v] for k, v in reserved_map.items()}
        ignores = tuple(numbering[i] for i in ignores)
        if len(ignores) > 5:
            ignores = set(ignores)

        if reserved_map:
            if ignores:

                def run_lexer(filename: str, text: str):
                    yield Token(0, 0, 0, filename, BOF, "")
                    yield from (
                        token
                        for token in lexing(filename, text, table, reserved_map)
                        if token.idint not in ignores
                    )
                    yield Token(0, 0, 0, filename, EOF, "")

            else:

                def run_lexer(filename: str, text: str):
                    yield Token(0, 0, 0, filename, BOF, "")
                    yield from lexing(filename, text, table, reserved_map)
                    yield Token(0, 0, 0, filename, EOF, "")

        else:
            if ignores:

                def run_lexer(filename: str, text: str):
                    yield Token(0, 0, 0, filename, BOF, "")
                    yield from (
                        token
                        for token in lexing_no_reserve(filename, text, table)
                        if token.idint not in ignores
                    )
                    yield Token(0, 0, 0, filename, EOF, "")

            else:

                def run_lexer(filename: str, text: str):
                    yield Token(0, 0, 0, filename, BOF, "")
                    yield from lexing_no_reserve(filename, text, table)
                    yield Token(0, 0, 0, filename, EOF, "")

        return numbering, run_lexer

    return mk_lexer

mk_lexer = mk()