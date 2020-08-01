from wisepy2 import wise
from string import Template
import re

subs = {f'FFFFFF{i}': f'${i+1}' for i in range(100)}

def fix(filename):
    with open(filename) as r:
        
        src = r.read()
    src = re.sub("\$(\d)", "$FFFFFF\g<1>", src)
    print(Template(src).safe_substitute(**subs))

if __name__ == "__main__":
    import sys
    wise(fix)()