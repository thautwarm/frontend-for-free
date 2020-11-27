[ -d hsbin-dist ] || mkdir hsbin-dist

if command -v python
then
    PY=python
else
    PY=python3
fi

cp `stack path --dist-dir`/build/fff-lex/fff-lex hsbin-dist/
cp `stack path --dist-dir`/build/fff-pgen/fff-pgen hsbin-dist/
export RELEASE_TAG="`$PY version.py`"
export ZIP_FILE="fff-`$PY release-env.py plat`-`$PY version.py`.zip"
zip -r $ZIP_FILE hsbin-dist/*
