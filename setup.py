from setuptools import setup

with open('./README.md', encoding='utf-8') as f:
    readme = f.read()

setup(
    name='frontend-for-free',
    version="0.3",
    keywords='parser, lexer, context-free',
    description="Statically generating standablone regex-based lexers and highly optimized LL(k) parsers",
    long_description=readme,
    long_description_content_type="text/markdown",
    license='MIT',
    url='https://github.com/thautwarm/frontend-for-free',
    author='thautwarm',
    author_email='twshere@outlook.com',
    py_modules=[
        "fffparser_template",
        "ffflex_template",
        "ffflex_parser",
        "ffflex",
        "fffbnf_parser",
        "fffbnf",
        "fffpylinker",
        "fffmain",

    ],
    install_requires=['ast-compat', 'wisepy2', 'rbnf-rts'],
    entry_points = {
         "console_scripts": [
             "ffflex=ffflex:entry",
             "fffbnf=fffbnf:entry",
             "fffpylinker=fffpylinker:entry",
             "fff=fffmain:entry",
         ]
    },
    platforms='any',
    classifiers=[
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7',
        'Programming Language :: Python :: 3.8',
        'Programming Language :: Python :: 3.9',
        'Programming Language :: Python :: Implementation :: CPython'
    ],
    zip_safe=False
)
