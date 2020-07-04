from json.decoder import py_scanstring
def S(tk):
    return tk.value

def unesc(x, f=py_scanstring):
    """from the raw form of a double quoted string to a python string,
    e.g.,
        unesc('"asd"') == "asd"
    """
    return f(x, 1)[0]

def append(x, a, f=list.append):
    f(x, a)
    return x
