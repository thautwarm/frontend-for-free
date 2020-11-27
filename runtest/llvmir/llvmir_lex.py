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
UNIONALL_INFO = ((None, None), (10, None), (None, None), (2, None), (5, None), (4, None), (3, {'internal': 115, 'naked': 165, 'ult': 101, 'insertvalue': 69, 'f64': 29, 'i64': 27, 'convergent': 159, 'add': 51, 'and': 65, 'eq': 92, 'ugt': 99, 'fadd': 52, 'urem': 59, 'section': 138, 'exact': 48, 'une': 110, 'olt': 106, 'nocapture': 144, 'inrange': 72, 'opaque': 13, 'sle': 96, 'sanitize_memory': 181, 'undef': 46, 'fmul': 56, 'sspreq': 185, 'srem': 60, 'weak': 119, 'speculatable': 183, 'tail': 88, 'sge': 94, 'fsub': 54, 'inlinehint': 162, 'i32': 26, 'true': 38, 'ashr': 64, 'source_filename': 11, 'f32': 28, 'ord': 108, 'volatile': 78, 'select': 87, 'safestack': 178, 'nonlazybind': 170, 'minsize': 164, 'jumptable': 163, '-': 7, 'nsw': 49, 'dso_preemptable': 124, 'dereferenceable_or_null': 22, 'i16': 25, 'bitcast': 73, 'inaccessiblememonly': 161, 'returns_twice': 177, 'external': 122, 'allocsize': 133, 'global': 132, 'align': 20, 'linkonce': 116, 'builtin': 157, 'noinline': 169, 'inbounds': 71, 'getelementptr': 70, 'ne': 93, 'store': 77, 'sub': 53, 'cold': 158, 'private': 118, 'or': 66, 'readonly': 147, 'inreg': 141, 'appending': 112, 'alwaysinline': 155, 'declare': 75, 'to': 74, 'call': 91, 'common': 114, 'inaccessiblemem_or_argmemonly': 160, 'ogt': 104, 'oge': 103, 'extern_weak': 121, 'inalloca': 140, 'uno': 111, 'argmemonly': 156, 'sgt': 95, 'ret': 79, 'ueq': 109, 'hidden': 126, 'noimplicitfloat': 168, 'fcmp': 85, 'constant': 131, 'default': 125, 'dereferenceable': 21, 'null': 40, 'i8': 24, 'writeonly': 153, 'noreturn': 173, 'define': 76, 'optnone': 175, 'blockaddress': 47, 'zeroext': 154, 'sanitize_hwaddress': 180, 'addrspace': 31, 'swifterror': 151, 'sanitize_address': 179, 'switch': 81, 'slt': 97, 'notail': 89, 'ssp': 184, 'nounwind': 174, 'E': 9, 'alloca': 82, 'norecurse': 171, 'externally_initialized': 130, 'uge': 98, 'noduplicate': 167, '...': 18, 'local_unnamed_addr': 128, 'icmp': 84, 'false': 39, 'nonnull': 145, 'nobuiltin': 166, 'nuw': 50, 'frem': 61, 'shl': 62, 'none': 41, 'br': 80, 'sret': 150, 'c': 44, 'i1': 23, 'udiv': 57, 'x': 34, 'sdiv': 58, 'ule': 100, 'xor': 67, 'type': 14, 'noredzone': 172, 'lshr': 63, 'zeroinitializer': 45, 'oeq': 102, 'extractvalue': 68, 'noalias': 143, 'swiftself': 152, 'unnamed_addr': 129, 'void': 15, 'uwtable': 188, 'dso_local': 123, 'ole': 105, '$': 137, 'musttail': 90, 'byval': 139, 'comdat': 136, 'phi': 86, 'protected': 127, 'nest': 142, 'returned': 148, 'readnone': 146, 'available_externally': 113, 'sspstrong': 186, 'sanitize_thread': 182, 'optsize': 176, 'load': 83, 'weak_odr': 120, 'strictfp': 187, 'label': 32, 'linkonce_odr': 117, 'alignstack': 134, 'signext': 149, 'one': 107, 'mul': 55}), (8, None), (None, None), (189, None), (135, None), (37, None), (36, None), (35, None), (33, None), (43, None), (12, None), (42, None), (19, None), (6, None), (30, None), (17, None), (16, None))
UNIONALL_INFO_BYTES = ((None, None), (10, None), (None, None), (2, None), (5, None), (4, None), (3, {b'internal': 115, b'naked': 165, b'ult': 101, b'insertvalue': 69, b'f64': 29, b'i64': 27, b'convergent': 159, b'add': 51, b'and': 65, b'eq': 92, b'ugt': 99, b'fadd': 52, b'urem': 59, b'section': 138, b'exact': 48, b'une': 110, b'olt': 106, b'nocapture': 144, b'inrange': 72, b'opaque': 13, b'sle': 96, b'sanitize_memory': 181, b'undef': 46, b'fmul': 56, b'sspreq': 185, b'srem': 60, b'weak': 119, b'speculatable': 183, b'tail': 88, b'sge': 94, b'fsub': 54, b'inlinehint': 162, b'i32': 26, b'true': 38, b'ashr': 64, b'source_filename': 11, b'f32': 28, b'ord': 108, b'volatile': 78, b'select': 87, b'safestack': 178, b'nonlazybind': 170, b'minsize': 164, b'jumptable': 163, b'-': 7, b'nsw': 49, b'dso_preemptable': 124, b'dereferenceable_or_null': 22, b'i16': 25, b'bitcast': 73, b'inaccessiblememonly': 161, b'returns_twice': 177, b'external': 122, b'allocsize': 133, b'global': 132, b'align': 20, b'linkonce': 116, b'builtin': 157, b'noinline': 169, b'inbounds': 71, b'getelementptr': 70, b'ne': 93, b'store': 77, b'sub': 53, b'cold': 158, b'private': 118, b'or': 66, b'readonly': 147, b'inreg': 141, b'appending': 112, b'alwaysinline': 155, b'declare': 75, b'to': 74, b'call': 91, b'common': 114, b'inaccessiblemem_or_argmemonly': 160, b'ogt': 104, b'oge': 103, b'extern_weak': 121, b'inalloca': 140, b'uno': 111, b'argmemonly': 156, b'sgt': 95, b'ret': 79, b'ueq': 109, b'hidden': 126, b'noimplicitfloat': 168, b'fcmp': 85, b'constant': 131, b'default': 125, b'dereferenceable': 21, b'null': 40, b'i8': 24, b'writeonly': 153, b'noreturn': 173, b'define': 76, b'optnone': 175, b'blockaddress': 47, b'zeroext': 154, b'sanitize_hwaddress': 180, b'addrspace': 31, b'swifterror': 151, b'sanitize_address': 179, b'switch': 81, b'slt': 97, b'notail': 89, b'ssp': 184, b'nounwind': 174, b'E': 9, b'alloca': 82, b'norecurse': 171, b'externally_initialized': 130, b'uge': 98, b'noduplicate': 167, b'...': 18, b'local_unnamed_addr': 128, b'icmp': 84, b'false': 39, b'nonnull': 145, b'nobuiltin': 166, b'nuw': 50, b'frem': 61, b'shl': 62, b'none': 41, b'br': 80, b'sret': 150, b'c': 44, b'i1': 23, b'udiv': 57, b'x': 34, b'sdiv': 58, b'ule': 100, b'xor': 67, b'type': 14, b'noredzone': 172, b'lshr': 63, b'zeroinitializer': 45, b'oeq': 102, b'extractvalue': 68, b'noalias': 143, b'swiftself': 152, b'unnamed_addr': 129, b'void': 15, b'uwtable': 188, b'dso_local': 123, b'ole': 105, b'$': 137, b'musttail': 90, b'byval': 139, b'comdat': 136, b'phi': 86, b'protected': 127, b'nest': 142, b'returned': 148, b'readnone': 146, b'available_externally': 113, b'sspstrong': 186, b'sanitize_thread': 182, b'optsize': 176, b'load': 83, b'weak_odr': 120, b'strictfp': 187, b'label': 32, b'linkonce_odr': 117, b'alignstack': 134, b'signext': 149, b'one': 107, b'mul': 55}), (8, None), (None, None), (189, None), (135, None), (37, None), (36, None), (35, None), (33, None), (43, None), (12, None), (42, None), (19, None), (6, None), (30, None), (17, None), (16, None))
numbering = {'BOF': 0, 'EOF': 1, 'int': 2, 'identifier': 3, 'localIdent': 4, 'globalIdent': 5, 'quote +': 6, 'quote -': 7, 'str': 8, 'quote E': 9, 'float': 10, 'quote source_filename': 11, 'quote =': 12, 'quote opaque': 13, 'quote type': 14, 'quote void': 15, 'quote (': 16, 'quote )': 17, 'quote ...': 18, 'quote ,': 19, 'quote align': 20, 'quote dereferenceable': 21, 'quote dereferenceable_or_null': 22, 'quote i1': 23, 'quote i8': 24, 'quote i16': 25, 'quote i32': 26, 'quote i64': 27, 'quote f32': 28, 'quote f64': 29, 'quote *': 30, 'quote addrspace': 31, 'quote label': 32, 'quote [': 33, 'quote x': 34, 'quote ]': 35, 'quote {': 36, 'quote }': 37, 'quote true': 38, 'quote false': 39, 'quote null': 40, 'quote none': 41, 'quote <': 42, 'quote >': 43, 'quote c': 44, 'quote zeroinitializer': 45, 'quote undef': 46, 'quote blockaddress': 47, 'quote exact': 48, 'quote nsw': 49, 'quote nuw': 50, 'quote add': 51, 'quote fadd': 52, 'quote sub': 53, 'quote fsub': 54, 'quote mul': 55, 'quote fmul': 56, 'quote udiv': 57, 'quote sdiv': 58, 'quote urem': 59, 'quote srem': 60, 'quote frem': 61, 'quote shl': 62, 'quote lshr': 63, 'quote ashr': 64, 'quote and': 65, 'quote or': 66, 'quote xor': 67, 'quote extractvalue': 68, 'quote insertvalue': 69, 'quote getelementptr': 70, 'quote inbounds': 71, 'quote inrange': 72, 'quote bitcast': 73, 'quote to': 74, 'quote declare': 75, 'quote define': 76, 'quote store': 77, 'quote volatile': 78, 'quote ret': 79, 'quote br': 80, 'quote switch': 81, 'quote alloca': 82, 'quote load': 83, 'quote icmp': 84, 'quote fcmp': 85, 'quote phi': 86, 'quote select': 87, 'quote tail': 88, 'quote notail': 89, 'quote musttail': 90, 'quote call': 91, 'quote eq': 92, 'quote ne': 93, 'quote sge': 94, 'quote sgt': 95, 'quote sle': 96, 'quote slt': 97, 'quote uge': 98, 'quote ugt': 99, 'quote ule': 100, 'quote ult': 101, 'quote oeq': 102, 'quote oge': 103, 'quote ogt': 104, 'quote ole': 105, 'quote olt': 106, 'quote one': 107, 'quote ord': 108, 'quote ueq': 109, 'quote une': 110, 'quote uno': 111, 'quote appending': 112, 'quote available_externally': 113, 'quote common': 114, 'quote internal': 115, 'quote linkonce': 116, 'quote linkonce_odr': 117, 'quote private': 118, 'quote weak': 119, 'quote weak_odr': 120, 'quote extern_weak': 121, 'quote external': 122, 'quote dso_local': 123, 'quote dso_preemptable': 124, 'quote default': 125, 'quote hidden': 126, 'quote protected': 127, 'quote local_unnamed_addr': 128, 'quote unnamed_addr': 129, 'quote externally_initialized': 130, 'quote constant': 131, 'quote global': 132, 'quote allocsize': 133, 'quote alignstack': 134, 'attr_group_id': 135, 'quote comdat': 136, 'quote $': 137, 'quote section': 138, 'quote byval': 139, 'quote inalloca': 140, 'quote inreg': 141, 'quote nest': 142, 'quote noalias': 143, 'quote nocapture': 144, 'quote nonnull': 145, 'quote readnone': 146, 'quote readonly': 147, 'quote returned': 148, 'quote signext': 149, 'quote sret': 150, 'quote swifterror': 151, 'quote swiftself': 152, 'quote writeonly': 153, 'quote zeroext': 154, 'quote alwaysinline': 155, 'quote argmemonly': 156, 'quote builtin': 157, 'quote cold': 158, 'quote convergent': 159, 'quote inaccessiblemem_or_argmemonly': 160, 'quote inaccessiblememonly': 161, 'quote inlinehint': 162, 'quote jumptable': 163, 'quote minsize': 164, 'quote naked': 165, 'quote nobuiltin': 166, 'quote noduplicate': 167, 'quote noimplicitfloat': 168, 'quote noinline': 169, 'quote nonlazybind': 170, 'quote norecurse': 171, 'quote noredzone': 172, 'quote noreturn': 173, 'quote nounwind': 174, 'quote optnone': 175, 'quote optsize': 176, 'quote returns_twice': 177, 'quote safestack': 178, 'quote sanitize_address': 179, 'quote sanitize_hwaddress': 180, 'quote sanitize_memory': 181, 'quote sanitize_thread': 182, 'quote speculatable': 183, 'quote ssp': 184, 'quote sspreq': 185, 'quote sspstrong': 186, 'quote strictfp': 187, 'quote uwtable': 188, 'whitespace': 189}
