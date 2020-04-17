python hs_gen.py test/list.rbnf test/list.ff
stack exec fff-pgen -- -in test/list.ff -k 1 -out test/list.jl -be julia --noinline