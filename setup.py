from setuptools import setup

with open('./README.md', encoding='utf-8') as f:
    readme = f.read()

setup(
    name='fff',
    version="0.1",
    keywords='parsing, context-free',
    description="solution to context-free parsing",
    long_description=readme,
    long_description_content_type="text/markdown",
    license='MIT',
    url='https://github.com/thautwarm/frontend-for-free',
    author='thautwarm',
    author_email='twshere@outlook.com',
    py_modules=[
        "ffflex_template",
        "ffflex_parser",
        "ffflex",
        "fffbnf_parser",
        "fffbnf"
    ],
    entry_points = {
         "console_scripts": [
             "ffflex=ffflex:entry",
             "fffbnf=fffbnf:entry"
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