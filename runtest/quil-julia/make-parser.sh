fffbnf quil.g4 a.req a.ff
fff-pgen -in a.ff -k 2 -out quil_parser.jl -be julia --noinline --trace
fff-lex -in a.ff -out quil-lexer-index.txt
