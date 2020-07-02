fffbnf test/list.rbnf test/list.ff
stack exec fff-pgen -- -in test/list.ff -k 1 -out test/list.py -be python --noinline
ffflex test/list.ff.out test/list.lex test/list_lex.py
fffpylinker test/list.py test/list_opt.py
python test/test.py