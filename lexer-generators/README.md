# Lexer Generations For Various Programming Languages


This is the page for maintaining lexer generations of various programming languages,
especially for those as back ends supported by **Frontend For Free**.

Lexer generation depending on existing regex libraries are not really difficult,
hence due to the limitations of my personal efforts, I feel like to provide supports by enumerating them.

Most of these lexer generators are runtime generators.


## File Format of `.ffflex`

- `%ignore a, b, c, "d"`

    ignore a token named `a` or `b` or `c`, or a literal `"d"`.
    
    there can be multiple `ignore` statements for your flavored code organization.
    
    
- `%require a, b, c`

    require external variables `a`, `b`, `c`.

- `%code python "a.py", "b.py", "c.py"`
    
    if the target language is python, generated code will include "a.py", "b.py" and "c.py".
    
    relative path to `.ffflex`

- regex rules:
   ```
    %regex
    a [0-9]+
    b \S+
   ```    