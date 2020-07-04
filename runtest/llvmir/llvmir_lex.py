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
REGEX = __import__('re').compile('([-+]?[0-9]+\\.([eE][-+]?\\d+))|(\\d+)|(@[-a-zA-Z$._][-a-zA-Z$._0-9]*)|(%[-a-zA-Z$._][-a-zA-Z$._0-9]*)|([-a-zA-Z$._][-a-zA-Z$._0-9]*)|("([^\\\\"]+|\\\\.)*?")|(\\s+)|(\\#\\d+)|(\\})|(\\{)|(\\])|(\\[)|(>)|(=)|(<)|(,)|(\\+)|(\\*)|(\\))|(\\()')
IGNORES = (189,)
UNIONALL_INFO = ((None, None), (10, None), (None, None), (2, None), (5, None), (4, None), (3, {'speculatable': 183, 'i1': 23, 'returned': 148, 'ole': 105, 'or': 66, 'define': 76, 'i64': 27, 'dso_local': 123, 'c': 44, 'sgt': 95, 'type': 14, 'bitcast': 73, 'argmemonly': 156, 'one': 107, 'sanitize_hwaddress': 180, 'une': 110, 'constant': 131, 'void': 15, 'noredzone': 172, 'nonnull': 145, 'false': 39, 'sspreq': 185, 'linkonce': 116, 'uge': 98, '-': 7, 'select': 87, 'icmp': 84, 'noimplicitfloat': 168, 'optnone': 175, 'x': 34, 'private': 118, 'exact': 48, 'naked': 165, 'writeonly': 153, 'zeroext': 154, 'noduplicate': 167, 'opaque': 13, 'jumptable': 163, 'sanitize_thread': 182, 'align': 20, 'ugt': 99, 'swifterror': 151, 'tail': 88, 'dso_preemptable': 124, 'switch': 81, 'zeroinitializer': 45, 'nuw': 50, 'frem': 61, 'inbounds': 71, 'safestack': 178, 'sdiv': 58, 'sub': 53, 'load': 83, 'true': 38, 'volatile': 78, 'cold': 158, 'noreturn': 173, 'i16': 25, 'hidden': 126, 'inrange': 72, 'byval': 139, 'nocapture': 144, 'inaccessiblemem_or_argmemonly': 160, 'shl': 62, 'null': 40, 'oeq': 102, 'fsub': 54, 'ueq': 109, 'extern_weak': 121, 'local_unnamed_addr': 128, 'sanitize_memory': 181, 'uwtable': 188, 'builtin': 157, 'alignstack': 134, 'label': 32, 'strictfp': 187, 'sspstrong': 186, 'inlinehint': 162, 'returns_twice': 177, 'inreg': 141, 'lshr': 63, 'minsize': 164, 'nobuiltin': 166, 'alloca': 82, 'oge': 103, 'ashr': 64, 'comdat': 136, 'convergent': 159, 'signext': 149, 'E': 9, 'ne': 93, 'srem': 60, 'call': 91, 'and': 65, '$': 137, 'nounwind': 174, 'nsw': 49, 'sle': 96, 'ogt': 104, 'readnone': 146, 'musttail': 90, 'common': 114, '...': 18, 'slt': 97, 'xor': 67, 'weak_odr': 120, 'ssp': 184, 'protected': 127, 'addrspace': 31, 'available_externally': 113, 'global': 132, 'add': 51, 'external': 122, 'urem': 59, 'nest': 142, 'sge': 94, 'source_filename': 11, 'i32': 26, 'f32': 28, 'noalias': 143, 'inaccessiblememonly': 161, 'norecurse': 171, 'fmul': 56, 'extractvalue': 68, 'inalloca': 140, 'ret': 79, 'notail': 89, 'nonlazybind': 170, 'fadd': 52, 'externally_initialized': 130, 'internal': 115, 'blockaddress': 47, 'to': 74, 'dereferenceable': 21, 'default': 125, 'udiv': 57, 'mul': 55, 'phi': 86, 'ule': 100, 'section': 138, 'alwaysinline': 155, 'optsize': 176, 'weak': 119, 'f64': 29, 'store': 77, 'uno': 111, 'dereferenceable_or_null': 22, 'ult': 101, 'readonly': 147, 'declare': 75, 'fcmp': 85, 'unnamed_addr': 129, 'appending': 112, 'insertvalue': 69, 'none': 41, 'br': 80, 'noinline': 169, 'ord': 108, 'sret': 150, 'olt': 106, 'sanitize_address': 179, 'i8': 24, 'allocsize': 133, 'eq': 92, 'linkonce_odr': 117, 'swiftself': 152, 'getelementptr': 70, 'undef': 46}), (8, None), (None, None), (189, None), (135, None), (37, None), (36, None), (35, None), (33, None), (43, None), (12, None), (42, None), (19, None), (6, None), (30, None), (17, None), (16, None))
numbering = {'BOF': 0, 'EOF': 1, 'int': 2, 'identifier': 3, 'localIdent': 4, 'globalIdent': 5, 'quote +': 6, 'quote -': 7, 'str': 8, 'quote E': 9, 'float': 10, 'quote source_filename': 11, 'quote =': 12, 'quote opaque': 13, 'quote type': 14, 'quote void': 15, 'quote (': 16, 'quote )': 17, 'quote ...': 18, 'quote ,': 19, 'quote align': 20, 'quote dereferenceable': 21, 'quote dereferenceable_or_null': 22, 'quote i1': 23, 'quote i8': 24, 'quote i16': 25, 'quote i32': 26, 'quote i64': 27, 'quote f32': 28, 'quote f64': 29, 'quote *': 30, 'quote addrspace': 31, 'quote label': 32, 'quote [': 33, 'quote x': 34, 'quote ]': 35, 'quote {': 36, 'quote }': 37, 'quote true': 38, 'quote false': 39, 'quote null': 40, 'quote none': 41, 'quote <': 42, 'quote >': 43, 'quote c': 44, 'quote zeroinitializer': 45, 'quote undef': 46, 'quote blockaddress': 47, 'quote exact': 48, 'quote nsw': 49, 'quote nuw': 50, 'quote add': 51, 'quote fadd': 52, 'quote sub': 53, 'quote fsub': 54, 'quote mul': 55, 'quote fmul': 56, 'quote udiv': 57, 'quote sdiv': 58, 'quote urem': 59, 'quote srem': 60, 'quote frem': 61, 'quote shl': 62, 'quote lshr': 63, 'quote ashr': 64, 'quote and': 65, 'quote or': 66, 'quote xor': 67, 'quote extractvalue': 68, 'quote insertvalue': 69, 'quote getelementptr': 70, 'quote inbounds': 71, 'quote inrange': 72, 'quote bitcast': 73, 'quote to': 74, 'quote declare': 75, 'quote define': 76, 'quote store': 77, 'quote volatile': 78, 'quote ret': 79, 'quote br': 80, 'quote switch': 81, 'quote alloca': 82, 'quote load': 83, 'quote icmp': 84, 'quote fcmp': 85, 'quote phi': 86, 'quote select': 87, 'quote tail': 88, 'quote notail': 89, 'quote musttail': 90, 'quote call': 91, 'quote eq': 92, 'quote ne': 93, 'quote sge': 94, 'quote sgt': 95, 'quote sle': 96, 'quote slt': 97, 'quote uge': 98, 'quote ugt': 99, 'quote ule': 100, 'quote ult': 101, 'quote oeq': 102, 'quote oge': 103, 'quote ogt': 104, 'quote ole': 105, 'quote olt': 106, 'quote one': 107, 'quote ord': 108, 'quote ueq': 109, 'quote une': 110, 'quote uno': 111, 'quote appending': 112, 'quote available_externally': 113, 'quote common': 114, 'quote internal': 115, 'quote linkonce': 116, 'quote linkonce_odr': 117, 'quote private': 118, 'quote weak': 119, 'quote weak_odr': 120, 'quote extern_weak': 121, 'quote external': 122, 'quote dso_local': 123, 'quote dso_preemptable': 124, 'quote default': 125, 'quote hidden': 126, 'quote protected': 127, 'quote local_unnamed_addr': 128, 'quote unnamed_addr': 129, 'quote externally_initialized': 130, 'quote constant': 131, 'quote global': 132, 'quote allocsize': 133, 'quote alignstack': 134, 'attr_group_id': 135, 'quote comdat': 136, 'quote $': 137, 'quote section': 138, 'quote byval': 139, 'quote inalloca': 140, 'quote inreg': 141, 'quote nest': 142, 'quote noalias': 143, 'quote nocapture': 144, 'quote nonnull': 145, 'quote readnone': 146, 'quote readonly': 147, 'quote returned': 148, 'quote signext': 149, 'quote sret': 150, 'quote swifterror': 151, 'quote swiftself': 152, 'quote writeonly': 153, 'quote zeroext': 154, 'quote alwaysinline': 155, 'quote argmemonly': 156, 'quote builtin': 157, 'quote cold': 158, 'quote convergent': 159, 'quote inaccessiblemem_or_argmemonly': 160, 'quote inaccessiblememonly': 161, 'quote inlinehint': 162, 'quote jumptable': 163, 'quote minsize': 164, 'quote naked': 165, 'quote nobuiltin': 166, 'quote noduplicate': 167, 'quote noimplicitfloat': 168, 'quote noinline': 169, 'quote nonlazybind': 170, 'quote norecurse': 171, 'quote noredzone': 172, 'quote noreturn': 173, 'quote nounwind': 174, 'quote optnone': 175, 'quote optsize': 176, 'quote returns_twice': 177, 'quote safestack': 178, 'quote sanitize_address': 179, 'quote sanitize_hwaddress': 180, 'quote sanitize_memory': 181, 'quote sanitize_thread': 182, 'quote speculatable': 183, 'quote ssp': 184, 'quote sspreq': 185, 'quote sspstrong': 186, 'quote strictfp': 187, 'quote uwtable': 188, 'whitespace': 189}
