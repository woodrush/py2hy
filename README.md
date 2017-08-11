# py2hy

py2hy is a compiler that compiles Python AST to
[Hy](https://github.com/hylang/hy).

py2hy was used to create [HyHy](https://github.com/woodrush/hy/tree/hyhy),
the Hy language rewritten entirely in Hy.

Other Working demos are available in `demo/`. 


## Requirements

- Python >= 3.6
- Hy == 0.13.0

Currently, py2hy is based on Python 3.6's
[AST specs](https://docs.python.org/3.6/library/ast.html), and most tested in
Python 3.6.


## Usage

```bash
py2hy src.py
```

### Generating the Grammar Template
To parse the [Python AST specs](https://docs.python.org/3.6/library/ast.html)
and generate the grammar template, do

```bash
make template  # i.e. `python res/parse_pygrammarspecs.py > res/template.hy`
```

## How it works
### The Transformation
The main idea is to treat the Python AST as if it was an S-expression, then
treat the Python AST keywords as if they were Hy macros, and let Hy
recursively `macroexpand-1` the expression. For example, the `Return` Python
AST node has a field named `value`, so it would first be seen as the Hy code
`(Return :value value)`. `py2hy` then treats this as if it was a
macroexpansion of a macro named `Return`, producing Hy code.

The original implementation actually used Hy's macro system and
`macroexpand-1` for the transformation. The current implementation uses the
Python class system for an equivalent functionality and improved speed.

### Generating the Grammar Template
Running the [parser](lib/parse_pygrammarspecs.py) for the
[Python AST specs](https://docs.python.org/3.6/library/ast.html) creates a
[template script](template.hy) to be [filled in](py2hy.hy) to create
`py2hy.hy`, a set of definitions of the transformations from Python AST to Hy.
This tool would be useful to create a `py2hy` for different versions of the
Python AST specs.


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


## Contributing
py2hy currently does not have a full set of `pytest` tests. Contribution is
highly appreciated!


## License
All of the code is licensed under the GNU Lesser General Public License version
3. See `LICENSE` for details.