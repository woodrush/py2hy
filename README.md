# py2hy

py2hy is a transpiler that compiles Python AST to [Hy](https://github.com/hylang/hy).

Currently work in progress.

A working demo (in the current setting) is available at `demo/game.hy`. The original code is `demo/game.py`. Try 
```bash
hy py2hy.hy demo/game.py > demo/game.hy && hy demo/game.hy
```
and see that the Python script is correctly transpiled. 

## How it works
The main idea is to make the Python AST into S-expression form, then treat Python AST keywords as Hy macros, and let Hy recursively `macroexpand-1` it. Running the [parser](lib/parse_pygrammarspecs.py) for the [Python AST specs](https://docs.python.org/3.6/library/ast.html) creates a [template code](template.hy) to be [filled in](py2hy.hy) to create` py2hy.hy`, a set of definitions of the transformations from Python AST to Hy.

## Usage

```bash
hy py2hy.hy path/to/src.hy
```

## Major TODOs

- Implement the `return` statement (Hy is [planning](https://github.com/hylang/hy/issues/739) on supporting it)