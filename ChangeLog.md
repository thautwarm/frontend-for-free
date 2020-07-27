# Changelog for RBNF

## v0.4

1. Definition mark changed from `:=` to `:`.

    A production `a := b;` changed to `a : b;`

2. Action enhancements

   Given the example
   ```
    list[a] : a         { [$0] }
               | list[a] a { $1.append($2); $2 }
   ```

   We now have following new actions:

   1. Reading attributes(`a.b`).     
   2. Combining statements(`a; b`).

   However, due to the Python codegen issues, **reading attributes** will be slow. Python is a *statement-first* language, to support serious codegen, we need ANF transformation, which leads to an intermediate variable for `$1.append`. We haven't implemented register reallocation optimization to address it.