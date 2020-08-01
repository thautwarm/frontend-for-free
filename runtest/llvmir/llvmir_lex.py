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
REGEX = '([-+]?[0-9]+\\.([eE][-+]?\\d+))|(\\d+)|(@[-a-zA-Z$._][-a-zA-Z$._0-9]*)|(%[-a-zA-Z$._][-a-zA-Z$._0-9]*)|([-a-zA-Z$._][-a-zA-Z$._0-9]*)|("([^\\\\"]+|\\\\.)*?")|(\\s+)|(\\#\\d+)|(\\})|(\\{)|(\\])|(\\[)|(>)|(=)|(<)|(,)|(\\+)|(\\*)|(\\))|(\\()'
REGEX_STR = __import__('re').compile(REGEX)
REGEX_BYTES = __import__('re').compile(REGEX.encode())
IGNORES = (189,)
UNIONALL_INFO = ((None, None), (10, None), (None, None), (2, None), (5, None), (4, None), (3, {'sub': 53, 'nest': 142, 'inbounds': 71, 'source_filename': 11, 'nonlazybind': 170, 'safestack': 178, 'declare': 75, 'true': 38, 'extractvalue': 68, 'ole': 105, 'slt': 97, 'local_unnamed_addr': 128, 'readonly': 147, 'weak': 119, 'linkonce_odr': 117, 'label': 32, 'f64': 29, 'nocapture': 144, 'jumptable': 163, 'tail': 88, 'E': 9, 'bitcast': 73, 'builtin': 157, 'ret': 79, 'global': 132, 'sge': 94, 'musttail': 90, 'opaque': 13, 'noredzone': 172, 'fsub': 54, 'one': 107, 'returns_twice': 177, 'sanitize_hwaddress': 180, 'uge': 98, 'addrspace': 31, 'ule': 100, 'i1': 23, 'switch': 81, 'swifterror': 151, 'inalloca': 140, 'sret': 150, 'swiftself': 152, 'noduplicate': 167, 'inlinehint': 162, 'blockaddress': 47, 'call': 91, 'ueq': 109, 'internal': 115, 'hidden': 126, 'une': 110, '$': 137, 'comdat': 136, 'alloca': 82, 'eq': 92, 'uwtable': 188, 'allocsize': 133, 'i64': 27, '-': 7, 'ord': 108, 'appending': 112, 'constant': 131, 'nsw': 49, 'shl': 62, 'define': 76, 'udiv': 57, 'ugt': 99, 'add': 51, 'sanitize_address': 179, 'void': 15, 'i16': 25, 'urem': 59, 'store': 77, 'sle': 96, 'minsize': 164, 'naked': 165, 'to': 74, 'c': 44, 'private': 118, 'select': 87, 'icmp': 84, 'signext': 149, 'externally_initialized': 130, 'common': 114, 'weak_odr': 120, 'optsize': 176, 'returned': 148, 'null': 40, 'fcmp': 85, 'inaccessiblemem_or_argmemonly': 160, 'extern_weak': 121, 'frem': 61, 'alwaysinline': 155, 'load': 83, '...': 18, 'sspreq': 185, 'available_externally': 113, 'byval': 139, 'noimplicitfloat': 168, 'nounwind': 174, 'ne': 93, 'xor': 67, 'zeroext': 154, 'cold': 158, 'or': 66, 'default': 125, 'none': 41, 'inreg': 141, 'linkonce': 116, 'strictfp': 187, 'zeroinitializer': 45, 'norecurse': 171, 'alignstack': 134, 'ashr': 64, 'mul': 55, 'notail': 89, 'br': 80, 'exact': 48, 'oge': 103, 'section': 138, 'dereferenceable_or_null': 22, 'olt': 106, 'nobuiltin': 166, 'phi': 86, 'type': 14, 'argmemonly': 156, 'sgt': 95, 'volatile': 78, 'dereferenceable': 21, 'nonnull': 145, 'ssp': 184, 'readnone': 146, 'noreturn': 173, 'optnone': 175, 'i8': 24, 'ult': 101, 'noalias': 143, 'unnamed_addr': 129, 'f32': 28, 'noinline': 169, 'external': 122, 'sspstrong': 186, 'align': 20, 'and': 65, 'getelementptr': 70, 'x': 34, 'convergent': 159, 'insertvalue': 69, 'false': 39, 'dso_local': 123, 'dso_preemptable': 124, 'sanitize_thread': 182, 'oeq': 102, 'uno': 111, 'writeonly': 153, 'undef': 46, 'ogt': 104, 'sanitize_memory': 181, 'sdiv': 58, 'srem': 60, 'fmul': 56, 'inrange': 72, 'i32': 26, 'lshr': 63, 'speculatable': 183, 'nuw': 50, 'fadd': 52, 'protected': 127, 'inaccessiblememonly': 161}), (8, None), (None, None), (189, None), (135, None), (37, None), (36, None), (35, None), (33, None), (43, None), (12, None), (42, None), (19, None), (6, None), (30, None), (17, None), (16, None))
UNIONALL_INFO_BYTES = ((None, None), (10, None), (None, None), (2, None), (5, None), (4, None), (3, {b'sub': 53, b'nest': 142, b'inbounds': 71, b'source_filename': 11, b'nonlazybind': 170, b'safestack': 178, b'declare': 75, b'true': 38, b'extractvalue': 68, b'ole': 105, b'slt': 97, b'local_unnamed_addr': 128, b'readonly': 147, b'weak': 119, b'linkonce_odr': 117, b'label': 32, b'f64': 29, b'nocapture': 144, b'jumptable': 163, b'tail': 88, b'E': 9, b'bitcast': 73, b'builtin': 157, b'ret': 79, b'global': 132, b'sge': 94, b'musttail': 90, b'opaque': 13, b'noredzone': 172, b'fsub': 54, b'one': 107, b'returns_twice': 177, b'sanitize_hwaddress': 180, b'uge': 98, b'addrspace': 31, b'ule': 100, b'i1': 23, b'switch': 81, b'swifterror': 151, b'inalloca': 140, b'sret': 150, b'swiftself': 152, b'noduplicate': 167, b'inlinehint': 162, b'blockaddress': 47, b'call': 91, b'ueq': 109, b'internal': 115, b'hidden': 126, b'une': 110, b'$': 137, b'comdat': 136, b'alloca': 82, b'eq': 92, b'uwtable': 188, b'allocsize': 133, b'i64': 27, b'-': 7, b'ord': 108, b'appending': 112, b'constant': 131, b'nsw': 49, b'shl': 62, b'define': 76, b'udiv': 57, b'ugt': 99, b'add': 51, b'sanitize_address': 179, b'void': 15, b'i16': 25, b'urem': 59, b'store': 77, b'sle': 96, b'minsize': 164, b'naked': 165, b'to': 74, b'c': 44, b'private': 118, b'select': 87, b'icmp': 84, b'signext': 149, b'externally_initialized': 130, b'common': 114, b'weak_odr': 120, b'optsize': 176, b'returned': 148, b'null': 40, b'fcmp': 85, b'inaccessiblemem_or_argmemonly': 160, b'extern_weak': 121, b'frem': 61, b'alwaysinline': 155, b'load': 83, b'...': 18, b'sspreq': 185, b'available_externally': 113, b'byval': 139, b'noimplicitfloat': 168, b'nounwind': 174, b'ne': 93, b'xor': 67, b'zeroext': 154, b'cold': 158, b'or': 66, b'default': 125, b'none': 41, b'inreg': 141, b'linkonce': 116, b'strictfp': 187, b'zeroinitializer': 45, b'norecurse': 171, b'alignstack': 134, b'ashr': 64, b'mul': 55, b'notail': 89, b'br': 80, b'exact': 48, b'oge': 103, b'section': 138, b'dereferenceable_or_null': 22, b'olt': 106, b'nobuiltin': 166, b'phi': 86, b'type': 14, b'argmemonly': 156, b'sgt': 95, b'volatile': 78, b'dereferenceable': 21, b'nonnull': 145, b'ssp': 184, b'readnone': 146, b'noreturn': 173, b'optnone': 175, b'i8': 24, b'ult': 101, b'noalias': 143, b'unnamed_addr': 129, b'f32': 28, b'noinline': 169, b'external': 122, b'sspstrong': 186, b'align': 20, b'and': 65, b'getelementptr': 70, b'x': 34, b'convergent': 159, b'insertvalue': 69, b'false': 39, b'dso_local': 123, b'dso_preemptable': 124, b'sanitize_thread': 182, b'oeq': 102, b'uno': 111, b'writeonly': 153, b'undef': 46, b'ogt': 104, b'sanitize_memory': 181, b'sdiv': 58, b'srem': 60, b'fmul': 56, b'inrange': 72, b'i32': 26, b'lshr': 63, b'speculatable': 183, b'nuw': 50, b'fadd': 52, b'protected': 127, b'inaccessiblememonly': 161}), (8, None), (None, None), (189, None), (135, None), (37, None), (36, None), (35, None), (33, None), (43, None), (12, None), (42, None), (19, None), (6, None), (30, None), (17, None), (16, None))
numbering = {'BOF': 0, 'EOF': 1, 'int': 2, 'identifier': 3, 'localIdent': 4, 'globalIdent': 5, 'quote +': 6, 'quote -': 7, 'str': 8, 'quote E': 9, 'float': 10, 'quote source_filename': 11, 'quote =': 12, 'quote opaque': 13, 'quote type': 14, 'quote void': 15, 'quote (': 16, 'quote )': 17, 'quote ...': 18, 'quote ,': 19, 'quote align': 20, 'quote dereferenceable': 21, 'quote dereferenceable_or_null': 22, 'quote i1': 23, 'quote i8': 24, 'quote i16': 25, 'quote i32': 26, 'quote i64': 27, 'quote f32': 28, 'quote f64': 29, 'quote *': 30, 'quote addrspace': 31, 'quote label': 32, 'quote [': 33, 'quote x': 34, 'quote ]': 35, 'quote {': 36, 'quote }': 37, 'quote true': 38, 'quote false': 39, 'quote null': 40, 'quote none': 41, 'quote <': 42, 'quote >': 43, 'quote c': 44, 'quote zeroinitializer': 45, 'quote undef': 46, 'quote blockaddress': 47, 'quote exact': 48, 'quote nsw': 49, 'quote nuw': 50, 'quote add': 51, 'quote fadd': 52, 'quote sub': 53, 'quote fsub': 54, 'quote mul': 55, 'quote fmul': 56, 'quote udiv': 57, 'quote sdiv': 58, 'quote urem': 59, 'quote srem': 60, 'quote frem': 61, 'quote shl': 62, 'quote lshr': 63, 'quote ashr': 64, 'quote and': 65, 'quote or': 66, 'quote xor': 67, 'quote extractvalue': 68, 'quote insertvalue': 69, 'quote getelementptr': 70, 'quote inbounds': 71, 'quote inrange': 72, 'quote bitcast': 73, 'quote to': 74, 'quote declare': 75, 'quote define': 76, 'quote store': 77, 'quote volatile': 78, 'quote ret': 79, 'quote br': 80, 'quote switch': 81, 'quote alloca': 82, 'quote load': 83, 'quote icmp': 84, 'quote fcmp': 85, 'quote phi': 86, 'quote select': 87, 'quote tail': 88, 'quote notail': 89, 'quote musttail': 90, 'quote call': 91, 'quote eq': 92, 'quote ne': 93, 'quote sge': 94, 'quote sgt': 95, 'quote sle': 96, 'quote slt': 97, 'quote uge': 98, 'quote ugt': 99, 'quote ule': 100, 'quote ult': 101, 'quote oeq': 102, 'quote oge': 103, 'quote ogt': 104, 'quote ole': 105, 'quote olt': 106, 'quote one': 107, 'quote ord': 108, 'quote ueq': 109, 'quote une': 110, 'quote uno': 111, 'quote appending': 112, 'quote available_externally': 113, 'quote common': 114, 'quote internal': 115, 'quote linkonce': 116, 'quote linkonce_odr': 117, 'quote private': 118, 'quote weak': 119, 'quote weak_odr': 120, 'quote extern_weak': 121, 'quote external': 122, 'quote dso_local': 123, 'quote dso_preemptable': 124, 'quote default': 125, 'quote hidden': 126, 'quote protected': 127, 'quote local_unnamed_addr': 128, 'quote unnamed_addr': 129, 'quote externally_initialized': 130, 'quote constant': 131, 'quote global': 132, 'quote allocsize': 133, 'quote alignstack': 134, 'attr_group_id': 135, 'quote comdat': 136, 'quote $': 137, 'quote section': 138, 'quote byval': 139, 'quote inalloca': 140, 'quote inreg': 141, 'quote nest': 142, 'quote noalias': 143, 'quote nocapture': 144, 'quote nonnull': 145, 'quote readnone': 146, 'quote readonly': 147, 'quote returned': 148, 'quote signext': 149, 'quote sret': 150, 'quote swifterror': 151, 'quote swiftself': 152, 'quote writeonly': 153, 'quote zeroext': 154, 'quote alwaysinline': 155, 'quote argmemonly': 156, 'quote builtin': 157, 'quote cold': 158, 'quote convergent': 159, 'quote inaccessiblemem_or_argmemonly': 160, 'quote inaccessiblememonly': 161, 'quote inlinehint': 162, 'quote jumptable': 163, 'quote minsize': 164, 'quote naked': 165, 'quote nobuiltin': 166, 'quote noduplicate': 167, 'quote noimplicitfloat': 168, 'quote noinline': 169, 'quote nonlazybind': 170, 'quote norecurse': 171, 'quote noredzone': 172, 'quote noreturn': 173, 'quote nounwind': 174, 'quote optnone': 175, 'quote optsize': 176, 'quote returns_twice': 177, 'quote safestack': 178, 'quote sanitize_address': 179, 'quote sanitize_hwaddress': 180, 'quote sanitize_memory': 181, 'quote sanitize_thread': 182, 'quote speculatable': 183, 'quote ssp': 184, 'quote sspreq': 185, 'quote sspstrong': 186, 'quote strictfp': 187, 'quote uwtable': 188, 'whitespace': 189}
