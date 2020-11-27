[ -d hsbin-dist ] || mkdir hsbin-dist
cp `stack path --dist-dir`/build/fff-lex/fff-lex hsbin-dist/
cp `stack path --dist-dir`/build/fff-pgen/fff-pgen hsbin-dist/
export ZIP_FILE="fff-`python release-env.py plat`-`python version.py`.zip"
zip -r $ZIP_FILE hsbin-dist/*
