# py2hy

py2hy is a transpiler that compiles Python AST to [Hy](https://github.com/hylang/hy).

Currently work in progress.

A working demo (in the current setting) is available at `demo/game.hy`. The original code is `demo/game.py`. Try 
```bash
hy py2hy.hy demo/game.py > demo/game.hy && hy demo/game.hy
```
and see that the Python script is correctly transpiled. 


## Usage

```bash
hy py2hy.hy path/to/src.hy
```

## Major TODOs

- Implement the `return` statement (Hy is [planning](https://github.com/hylang/hy/issues/739) on supporting it)