# py2hy

py2hy is a transpiler that compiles Python AST to
[Hy](https://github.com/hylang/hy).

Currently work in progress.

Working demos (in the current setting) are available in `demo/`. Try:

```bash
hy py2hy.hy demo/makengamesolver.py > demo/makengamesolver.hy
hy demo/makengamesolver.hy
```

The newest version uses the Python AST class, making it transpile in a usable speed.


## Requirements

py2hy requires the newest version of hy pulled and built from its
[repo](https://github.com/hylang/hy). (I am currently using
[e8ffd41](https://github.com/hylang/hy/commit/e8ffd412028232cc2cc4fe4bfb10f21ce8ab2565))


## Usage

```bash
hy py2hy.hy path/to/src.hy
```

To parse the [Python AST specs](https://docs.python.org/3.6/library/ast.html)
and create the grammar template, do

```bash
make template  # i.e. `python parse_pygrammarspecs.py > template.hy`
```

## How it works
The main idea is to make the Python AST into S-expression form, then treat
Python AST keywords as Hy macros, and let Hy recursively `macroexpand-1` it.
Running the [parser](lib/parse_pygrammarspecs.py) for the
[Python AST specs](https://docs.python.org/3.6/library/ast.html) creates a
[template script](template.hy) to be [filled in](py2hy.hy) to create
`py2hy.hy`, a set of definitions of the transformations from Python AST to Hy.


## Notes
### The `return` statement
At the time of writing, the `return` statement is not implemented in Hy. Here,
`return` is implemented in a similar manner mentioned in
[the Hy issues](https://github.com/hylang/hy/issues/739#issuecomment-68392695).
For every `def` that contains a `return` statement, the entire function body is
wrapped in a `try` clause that catches a `Py2HyReturnException`. The `return`
statement is replaced by a `raise` statement that raises `Py2HyReturnException`
containing the value to be returned. `Py2HyReturnException` is defined in the
top of the entire script when the script contains any `return` statements.