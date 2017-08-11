(import [hy]
        [hy.extra.reserved]
        [ast]
        [re]
        [argparse]
        [py2hy.prettyprinter [prettyprinter]])

;;; Stable version Hy compatibility
;;; For splicing None as an empty list
(defsharp A [expr]
  `(if (is None ~expr)
     []
     ~expr))
;;; deftag -> defsharp
(defmacro deftag [name args &rest body]
  `(defsharp ~(hy.models.HySymbol name) ~args
     ~@body))

(defn expand-form [x]
  (if (hasattr x "expand")
    (x.expand)
    (cond
      [(is None x) None]
      [(= int (type x)) (hy.models.HyInteger x)]
      [(= float (type x)) (hy.models.HyFloat x)]
      [(= complex (type x)) (hy.models.HyComplex x)]
      [(= bool (type x)) (hy.models.HySymbol (hy.models.HySymbol (str x)))]
      [(= list (type x)) (hy.models.HyList x)]
      [(= bytes (type x)) (hy.models.HyBytes x)]
      [True (hy.models.HySymbol x)])))

(defn do-if-long [l]
  (setv l (list l))
  (if (= 1 (len l)) (first l) `(do ~@#A l)))

;;; TODO: use (hy.extra.reserved.names)
(setv hy_reserved_keywords
      `[fn defn defclass cond])
(defn py2hy_mangle_identifier [x]
  (if (in x hy_reserved_keywords)
    (hy.models.HySymbol (+ x "_py2hy_mangling"))
    x))

(deftag l [body]
  `(hy.models.HyList (list (map expand-form ~body))))

(deftag e [x]
  `(expand-form ~x))

(deftag k [key]
  `(. self ~key))

(defmacro defsyntax [name keys &rest body]
  `(do
     (setv (. (. ast ~name) expand)
           (fn [self]
             ~@#A body))))

;;;=============================================================================
;;; Classgroup `mod`
;;;=============================================================================
(defsyntax Module [body]
  "Args:
      body (stmt*) [list]"
  (setv bodylist #l #k body
        body (iter bodylist))
  (setv n (first body))
  (setv r (if (in "Py2HyReturnException" (bodylist.__repr__))
            `[(defclass Py2HyReturnException [Exception]
                (defn __init__ [self retvalue]
                  (setv self.retvalue retvalue)))]))

  ; `from __future__ import *` must be imported at the top of the file
  `(do
     ~@#A (if (and (= hy.models.HyExpression (type n))
                (= 'import (first n)))
         `[~n ~@#A r]
         `[~@#A r ~n])
     ~@#A body))

(defsyntax Interactive [body]
  "Args:
      body (stmt*) [list]"
  None)

(defsyntax Expression [body]
  "Args:
      body (expr)"
  None)

(defsyntax Suite [body]
  "Args:
      body (stmt*) [list]"
  None)


;;;=============================================================================
;;; Classgroup `stmt`
;;;=============================================================================
(defsyntax FunctionDef [name args body decorator_list returns lineno col_offset]
  "Args:
      name (identifier)
      args (arguments)
      body (stmt*) [list]
      decorator_list (expr*) [list]
      returns (expr?) [optional]
      lineno (int)
      col_offset (int)"
  (setv body #l #k body
        decorator_list #l #k decorator_list)
  (setv main_body
        (cond
          ; If there are no `return` statements in the entire body,
          ; don't add the `try` construct
          ; Note: - `return` statements must be recursively searched
          ;         inside `if`, `when` statements.
          ;       - This is a simple solution (a necessary condition) to that.
          [(not-in "Py2HyReturnException" (.__repr__ body))
           `(defn ~(py2hy_mangle_identifier #e #k name) ~#e #k args
              ~@#A body)]
          ; Optimize tail-returns to tail expressions.
          ; i.e. if there is only one `return` statement and it is in the tail
          ; of the function body, optimize it as a tail expression.
          ; Note: This cannot find `return` statements that are inside
          ; other AST nodes such as `if`, `for`, etc.
          [(and (not-in "Py2HyReturnException"
                        (.__repr__ (list (drop-last 1 body))))
                (= ast.Return (type (last #k body))))
           `(defn ~(py2hy_mangle_identifier #e #k name) ~#e #k args
              ~@#A #l (drop-last 1 #k body)
              ~#e (. (last #k body) value))]
          ; Keep docstrings
          [(= hy.models.HyString (type (first body)))
           `(defn ~(py2hy_mangle_identifier #e #k name) ~#e #k args
              ~(first body)
              (try
                ~(do-if-long (rest body))
                (except [e Py2HyReturnException]
                  e.retvalue)))]
          [True
           `(defn ~(py2hy_mangle_identifier #e #k name) ~#e #k args
              (try
                ~(do-if-long body)
                (except [e Py2HyReturnException]
                  e.retvalue)))]))
  (if decorator_list
    `(with-decorator
       ~@#A decorator_list
       ~main_body)
    main_body))

(defsyntax AsyncFunctionDef [name args body decorator_list returns lineno
                             col_offset]
  "Args:
      name (identifier)
      args (arguments)
      body (stmt*) [list]
      decorator_list (expr*) [list]
      returns (expr?) [optional]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax ClassDef [name bases keywords body decorator_list lineno col_offset]
  "Args:
      name (identifier)
      bases (expr*) [list]
      keywords (keyword*) [list]
      body (stmt*) [list]
      decorator_list (expr*) [list]
      lineno (int)
      col_offset (int)"
  ; TODO: defclass
  `(defclass ~(py2hy_mangle_identifier #e #k name) [~@#A #l #k bases]
     ~@#A #l #k body))

(defsyntax Return [value lineno col_offset]
  "Args:
      value (expr?) [optional]
      lineno (int)
      col_offset (int)"
  `(raise (Py2HyReturnException ~#e #k value)))

(defsyntax Delete [targets lineno col_offset]
  "Args:
      targets (expr*) [list]
      lineno (int)
      col_offset (int)"
  `(del ~@#A #l #k targets))

(defsyntax Assign [targets value lineno col_offset]
  "Args:
      targets (expr*) [list]
      value (expr)
      lineno (int)
      col_offset (int)"

  (setv targets #l #k targets)
  (setv g (if (or (< 1 (len targets))
                  (= ', (first (first targets))))
            (hy.models.HySymbol (+ "_py2hy_anon_var_"
                                   (.join "" (drop 1 (gensym)))))
            #e #k value))
  (setv typedict {ast.Tuple
                  (fn [target value]
                    (reduce + (map (fn [l] ((get typedict (type (first l)))
                                            (first l)
                                            (second l)))
                                   (zip target.elts
                                        (map
                                          (fn [t] `(get ~(second t) ~(first t)))
                                          (enumerate (repeat value)))))))
                  ast.Subscript
                  (fn [target value]
                    (setv target #e target)
                    `[(assoc ~(get target 1) ~(get target 2) ~value)])
                  ast.Attribute
                  (fn [target value]
                    (setv target #e target)
                    `[(setv ~target ~value)])
                  ast.Name
                  (fn [target value]
                    (setv target #e target)
                    (if (= '_ target)
                      `[(do)]
                      `[(setv ~target ~value)]))})
  (setv ret `[~@#A (if (or (< 1 (len targets))
                        (= ', (first (first targets))))
                  [`(setv ~g ~#e #k value)])
              ~@#A (reduce +
                        (map (fn [l] `[~@#A ((get typedict
                                               (type (first l)))
                                          (first l) (second l))])
                             (zip #k targets
                                  (repeat g))))])
  ; Optimization
  (setv ret `[~@#A (list-comp x [x ret] (not (= '(do) x)))])
  (if (= 1 (len ret))
    (first ret)
    `(do ~@#A ret)))

(defsyntax AugAssign [target op value lineno col_offset]
  "Args:
      target (expr)
      op (operator)
      value (expr)
      lineno (int)
      col_offset (int)"
  (setv op2aug {`+  `+=
                `-  `-=
                `*  `*=
                `/  `/=
                `%  `%=
                `** `**=
                `<< `<<=
                `>> `>>=
                `|  `|=
                `^  `^=
                `// `//=
                `bitand `&=})
  `(~(get op2aug #e #k op) ~#e #k target ~#e #k value))

(defsyntax AnnAssign [target annotation value simple lineno col_offset]
  "Args:
      target (expr)
      annotation (expr)
      value (expr?) [optional]
      simple (int)
      lineno (int)
      col_offset (int)"
  None)

(defsyntax For [target iter body orelse lineno col_offset]
  "Args:
      target (expr)
      iter (expr)
      body (stmt*) [list]
      orelse (stmt*) [list]
      lineno (int)
      col_offset (int)"
  (setv target #e #k target)
  `(for [~@#A (if (= ', (first target))
             [`[~@#A (rest target)]]
             [target])
         ~#e #k iter]
     ~@#A #l #k body))

(defsyntax AsyncFor [target iter body orelse lineno col_offset]
  "Args:
      target (expr)
      iter (expr)
      body (stmt*) [list]
      orelse (stmt*) [list]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax While [test body orelse lineno col_offset]
  "Args:
      test (expr)
      body (stmt*) [list]
      orelse (stmt*) [list]
      lineno (int)
      col_offset (int)"
  `(while ~#e #k test
     ~@#A #l #k body))

(defsyntax If [test body orelse lineno col_offset]
  "Args:
      test (expr)
      body (stmt*) [list]
      orelse (stmt*) [list]
      lineno (int)
      col_offset (int)"
  (setv orelseast #k orelse
        orelse #l orelseast
        body #l #k body)
  (cond
    [(= 'cond (first orelse))
     `(cond
        [~#e #k
         test ~(do-if-long body)]
        ~@#A (drop 1 body))]
    [(and (-> orelseast (len) (= 1))
          (-> orelseast (first) (type) (= ast.If)))
     `(cond
        [~#e #k test
         ~(do-if-long #l #k body)]
        [~#e (. (first orelseast) test)
         ~(do-if-long #l (. (first orelseast) body))]
        [True
         ~(do-if-long #l (. (first orelseast) orelse))])]
    [orelse
     `(if ~#e #k test
        (do
          ~@#A #l #k body)
        (do
          ~@#A orelse))]
    [True
     `(when ~#e #k test
        ~@#A #l #k body)]))

(defsyntax With [items body lineno col_offset]
  "Args:
      items (withitem*) [list]
      body (stmt*) [list]
      lineno (int)
      col_offset (int)"
  (defn nest-with [l]
    (if (empty? l)
      #l #k body
      `[(with [~@#A (first l)]
             ~@#A (nest-with (list (drop 1 l))))]))
  (first (nest-with #l #k items)))

(defsyntax AsyncWith [items body lineno col_offset]
  "Args:
      items (withitem*) [list]
      body (stmt*) [list]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Raise [exc cause lineno col_offset]
  "Args:
      exc (expr?) [optional]
      cause (expr?) [optional]
      lineno (int)
      col_offset (int)"
  ; TODO: cause
  (setv exc #e #k exc)
  `(raise ~@#A (if exc [exc])))

(defsyntax Try [body handlers orelse finalbody lineno col_offset]
  "Args:
      body (stmt*) [list]
      handlers (excepthandler*) [list]
      orelse (stmt*) [list]
      finalbody (stmt*) [list]
      lineno (int)
      col_offset (int)"
  (setv orelse #l #k orelse
        finalbody #l #k finalbody)
  `(try
     ~(do-if-long #l #k body)
     (except [e Py2HyReturnException]
       (raise e))
     ~@#A #l #k handlers
     ~@#A (if (< 0 (len orelse))
         `[(else
            ~@#A orelse)])
     ~@#A (if (< 0 (len finalbody))
         `[(finally
            ~@#A finalbody)])))

(defsyntax Assert [test msg lineno col_offset]
  "Args:
      test (expr)
      msg (expr?) [optional]
      lineno (int)
      col_offset (int)"
  (setv msg #e #k msg)
  `(assert ~#e #k test ~@#A (if msg [msg])))

(defsyntax Import [names lineno col_offset]
  "Args:
      names (alias*) [list]
      lineno (int)
      col_offset (int)"
  `(import ~@#A #l #k names))

(defsyntax ImportFrom [module names level lineno col_offset]
  "Args:
      module (identifier?) [optional]
      names (alias*) [list]
      level (int?) [optional]
      lineno (int)
      col_offset (int)"
  `(import [~#e #k module [~@#A (reduce + #l #k names)]]))

(defsyntax Global [names lineno col_offset]
  "Args:
      names (identifier*) [list]
      lineno (int)
      col_offset (int)"
  `(global ~@#A (map py2hy_mangle_identifier #l #k names)))

(defsyntax Nonlocal [names lineno col_offset]
  "Args:
      names (identifier*) [list]
      lineno (int)
      col_offset (int)"
  `(nonlocal ~@#A (map py2hy_mangle_identifier #l #k names)))

(defsyntax Expr [value lineno col_offset]
  "Args:
      value (expr)
      lineno (int)
      col_offset (int)"
  #e #k value)

(defsyntax Pass [lineno col_offset]
  "Args:
      lineno (int)
      col_offset (int)"
  `(do))

(defsyntax Break [lineno col_offset]
  "Args:
      lineno (int)
      col_offset (int)"
  `(break))

(defsyntax Continue [lineno col_offset]
  "Args:
      lineno (int)
      col_offset (int)"
  `(continue))


;;;=============================================================================
;;; Classgroup `expr`
;;;=============================================================================
(defsyntax BoolOp [op values lineno col_offset]
  "Args:
      op (boolop)
      values (expr*) [list]
      lineno (int)
      col_offset (int)"
  `(~#e #k op ~@#A #l #k values))

(defsyntax BinOp [left op right lineno col_offset]
  "Args:
      left (expr)
      op (operator)
      right (expr)
      lineno (int)
      col_offset (int)"
  `(~#e #k op ~#e #k left ~#e #k right))

(defsyntax UnaryOp [op operand lineno col_offset]
  "Args:
      op (unaryop)
      operand (expr)
      lineno (int)
      col_offset (int)"
  `(~#e #k op ~#e #k operand))

(defsyntax Lambda [args body lineno col_offset]
  "Args:
      args (arguments)
      body (expr)
      lineno (int)
      col_offset (int)"
  `(fn ~#e #k args ~#e #k body))

(defsyntax IfExp [test body orelse lineno col_offset]
  "Args:
      test (expr)
      body (expr)
      orelse (expr)
      lineno (int)
      col_offset (int)"
  `(if ~#e #k test
     ~#e #k body
     ~#e #k orelse))

(defsyntax Dict [keys values lineno col_offset]
  "Args:
      keys (expr*) [list]
      values (expr*) [list]
      lineno (int)
      col_offset (int)"
  `{~@#A (interleave #l #k keys #l #k values)})

(defsyntax Set [elts lineno col_offset]
  "Args:
      elts (expr*) [list]
      lineno (int)
      col_offset (int)"
  `(set ~@#A #l #k elts))

(defsyntax ListComp [elt generators lineno col_offset]
  "Args:
      elt (expr)
      generators (comprehension*) [list]
      lineno (int)
      col_offset (int)"
  `(list-comp ~#e #k elt
              ~@#A (reduce (fn [x y] (+ x y)) #l #k generators)))

(defsyntax SetComp [elt generators lineno col_offset]
  "Args:
      elt (expr)
      generators (comprehension*) [list]
      lineno (int)
      col_offset (int)"
  `(set-comp ~#e #k elt
             ~@#A (reduce (fn [x y] (+ x y)) #l #k generators)))

(defsyntax DictComp [key value generators lineno col_offset]
  "Args:
      key (expr)
      value (expr)
      generators (comprehension*) [list]
      lineno (int)
      col_offset (int)"
  `(dict-comp ~#e #k key
              ~#e #k value
              ~@#A (reduce (fn [x y] (+ x y)) #l #k generators)))

(defsyntax GeneratorExp [elt generators lineno col_offset]
  "Args:
      elt (expr)
      generators (comprehension*) [list]
      lineno (int)
      col_offset (int)"
  `(genexpr ~#e #k elt
            ~@#A (reduce (fn [x y] (+ x y)) #l #k generators)))

(defsyntax Await [value lineno col_offset]
  "Args:
      value (expr)
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Yield [value lineno col_offset]
  "Args:
      value (expr?) [optional]
      lineno (int)
      col_offset (int)"
  (setv value #e #k value)
  `(yield ~@#A (if value [value])))

(defsyntax YieldFrom [value lineno col_offset]
  "Args:
      value (expr)
      lineno (int)
      col_offset (int)"
  `(yield_from ~@#A (if value [value])))

(defsyntax Compare [left ops comparators lineno col_offset]
  "Args:
      left (expr)
      ops (cmpop*) [list]
      comparators (expr*) [list]
      lineno (int)
      col_offset (int)"
  (setv ops #l #k ops
        comparators #l #k comparators
        left #e #k left)
  (setv resultlist
        (map
         (fn [x]
           `(~(first x) ~(second x) ~(get x 2)))
         (zip ops
              (+ [left] comparators)
              comparators)))
  (if (< 1 (len comparators))
    `(and ~@#A resultlist)
    (first resultlist)))

(defsyntax Call [func args keywords lineno col_offset]
  "Args:
      func (expr)
      args (expr*) [list]
      keywords (keyword*) [list]
      lineno (int)
      col_offset (int)"
  (setv keywords #l #k keywords)
  `(~#e #k func
    ~@#A #l #k args
    ~@#A (if keywords
        (reduce (fn [x y] (if (first y)
                            (+ x y)
                            `[(~'unpack_mapping ~(second y))]))
                (map (fn [l] [(if (get l 0)
                                (hy.models.HyKeyword (+ ":" (get l 0)))
                                None) (get l 1)])
                     #l #k keywords)
                []))))

(defsyntax Num [n lineno col_offset]
  "Args:
      n (object)
      lineno (int)
      col_offset (int)"
  #e #k n)

(defsyntax Str [s lineno col_offset]
  "Args:
      s (string)
      lineno (int)
      col_offset (int)"
  (hy.models.HyString #e #k s))

(defsyntax FormattedValue [value conversion format_spec lineno col_offset]
  "Args:
      value (expr)
      conversion (int?) [optional]
      format_spec (expr?) [optional]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax JoinedStr [values lineno col_offset]
  "Args:
      values (expr*) [list]
      lineno (int)
      col_offset (int)"
  #e #k values)

(defsyntax Bytes [s lineno col_offset]
  "Args:
      s (bytes)
      lineno (int)
      col_offset (int)"
  `(hy.models.HyBytes ~#e #k s))

(defsyntax NameConstant [value lineno col_offset]
  "Args:
      value (Constant)
      lineno (int)
      col_offset (int)"
  #e #k value)

(defsyntax Ellipsis [lineno col_offset]
  "Args:
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Constant [value lineno col_offset]
  "Args:
      value (constant)
      lineno (int)
      col_offset (int)"
  #e #k value)

(defsyntax Attribute [value attr ctx lineno col_offset]
  "Args:
      value (expr)
      attr (identifier)
      ctx (expr_context)
      lineno (int)
      col_offset (int)"
  ; (setv s (gensym)
  ;       a (hy.models.HySymbol (+ s "." #e #k attr)))
  ; (print (type a))
  ; (print a)
  ; `(do
  ;    (setv ~s ~#e #k value)
  ;    ~a)
  (setv value #e #k value)
  (cond
    [
     ; False
     (= hy.models.HySymbol (type value))
     (hy.models.HySymbol (+ (str value) "." #k attr))]
    [True
     `(. ~#e #k value ~#e #k attr)]))

(defsyntax Subscript [value slice ctx lineno col_offset]
  "Args:
      value (expr)
      slice (slice)
      ctx (expr_context)
      lineno (int)
      col_offset (int)"
  `(get ~#e #k value ~#e #k slice))

(defsyntax Starred [value ctx lineno col_offset]
  "Args:
      value (expr)
      ctx (expr_context)
      lineno (int)
      col_offset (int)"
  `(~'unpack_iterable ~#e #k value))

(defsyntax Name [id ctx lineno col_offset]
  "Args:
      id (identifier)
      ctx (expr_context)
      lineno (int)
      col_offset (int)"
  (py2hy_mangle_identifier #e #k id))

(defsyntax List [elts ctx lineno col_offset]
  "Args:
      elts (expr*) [list]
      ctx (expr_context)
      lineno (int)
      col_offset (int)"
  `[~@#A #l #k elts])

(defsyntax Tuple [elts ctx lineno col_offset]
  "Args:
      elts (expr*) [list]
      ctx (expr_context)
      lineno (int)
      col_offset (int)"
  `(, ~@#A #l #k elts))


;;;=============================================================================
;;; Classgroup `expr_context`
;;;=============================================================================
(defsyntax Load []
  "Constant expression")

(defsyntax Store []
  "Constant expression")

(defsyntax Del []
  "Constant expression")

(defsyntax AugLoad []
  "Constant expression")

(defsyntax AugStore []
  "Constant expression")

(defsyntax Param []
  "Constant expression")


;;;=============================================================================
;;; Classgroup `slice`
;;;=============================================================================
(defsyntax Slice [lower upper step]
  "Args:
      lower (expr?) [optional]
      upper (expr?) [optional]
      step (expr?) [optional]"
  `(slice ~#e #k lower ~#e #k upper ~#e #k step))

(defsyntax ExtSlice [dims]
  "Args:
      dims (slice*) [list]"
  None)

(defsyntax Index [value]
  "Args:
      value (expr)"
  #e #k value)


;;;=============================================================================
;;; Classgroup `boolop`
;;;=============================================================================
(defsyntax And []
  "Constant expression" `and)

(defsyntax Or []
  "Constant expression" `or)


;;;=============================================================================
;;; Classgroup `operator`
;;;=============================================================================
(defsyntax Add []
  "Constant expression" `+)

(defsyntax Sub []
  "Constant expression" `-)

(defsyntax Mult []
  "Constant expression" `*)

(defsyntax MatMult []
  "Constant expression" `@)

(defsyntax Div []
  "Constant expression" `/)

(defsyntax Mod []
  "Constant expression" `%)

(defsyntax Pow []
  "Constant expression" `**)

(defsyntax LShift []
  "Constant expression" `<<)

(defsyntax RShift []
  "Constant expression" `>>)

(defsyntax BitOr []
  "Constant expression" `|)

(defsyntax BitXor []
  "Constant expression" `^)

(defsyntax BitAnd []
  "Constant expression" `&)

(defsyntax FloorDiv []
  "Constant expression" `//)


;;;=============================================================================
;;; Classgroup `unaryop`
;;;=============================================================================
(defsyntax Invert []
  "Constant expression" `~)

(defsyntax Not []
  "Constant expression" `not)

(defsyntax UAdd []
  "Constant expression" `+)

(defsyntax USub []
  "Constant expression" `-)


;;;=============================================================================
;;; Classgroup `cmpop`
;;;=============================================================================
(defsyntax Eq []
  "Constant expression" `=)

(defsyntax NotEq []
  "Constant expression" `!=)

(defsyntax Lt []
  "Constant expression" `<)

(defsyntax LtE []
  "Constant expression" `<=)

(defsyntax Gt []
  "Constant expression" `>)

(defsyntax GtE []
  "Constant expression" `>=)

(defsyntax Is []
  "Constant expression" `is)

(defsyntax IsNot []
  "Constant expression" `is-not)

(defsyntax In []
  "Constant expression" `in)

(defsyntax NotIn []
  "Constant expression" `not-in)


;;;=============================================================================
;;; Datatype `comprehension`
;;;=============================================================================
(defsyntax comprehension [target iter ifs is_async]
  "Args:
      target (expr)
      iter (expr)
      ifs (expr*) [list]
      is_async (int)"
  (setv target #e #k target
        ifs #l #k ifs)
  `[[~@#A (if (= ', (first target))
         [`[~@#A (rest target)]]
         [target])
     ~#e #k iter]
    ~@#A (if (< 0 (len ifs))
        `[(and ~@#A ifs)])])


;;;=============================================================================
;;; Classgroup `excepthandler`
;;;=============================================================================
(defsyntax ExceptHandler [type name body lineno col_offset]
  "Args:
      type (expr?) [optional]
      name (identifier?) [optional]
      body (stmt*) [list]
      lineno (int)
      col_offset (int)"
  (setv e_name (py2hy_mangle_identifier #e #k name)
        e_type #e #k type)
  `(except [~@#A (if e_name [e_name])
            ~@#A (cond
                [(is None e_type) None]
                [(= ', (first e_type)) [`[~@#A (rest e_type)]]]
                [True [e_type]])]
     ~@#A #l #k body))


;;;=============================================================================
;;; Datatype `arguments`
;;;=============================================================================
(defsyntax arguments [args vararg kwonlyargs kw_defaults kwarg defaults]
  "Args:
      args (arg*) [list]
      vararg (arg?) [optional]
      kwonlyargs (arg*) [list]
      kw_defaults (expr*) [list]
      kwarg (arg?) [optional]
      defaults (expr*) [list]"
  ; TODO: kwonlyargs
  (setv args #l #k args
        vararg #e #k vararg
        kwarg #e #k kwarg
        defaults #l #k defaults
        kwonlyargs #l #k kwonlyargs
        kw_defaults #l #k kw_defaults)
  (defn take-last [n l]
    (defn len [iter]
      (sum (list-comp 1 [x iter])))
    (drop (- (len l) n) l))
  `[~@#A (drop-last (len defaults) args)
    ~@#A (if defaults
        `[&optional
          ~@#A (list-comp `[~x ~y]
                       [[x y] (zip (take-last (len defaults) args)
                                   defaults)])])
    ~@#A (if kwonlyargs
        `[&kwonly
          ~@#A (drop-last (len kw_defaults) kwonlyargs)
          ~@#A (list-comp `[~x ~y]
                       [[x y] (zip (take-last (len kw_defaults) kwonlyargs)
                                   kw_defaults)])])
    ~@#A (if kwarg `[&kwargs ~kwarg])
    ~@#A (if vararg `[&rest ~vararg])])


;;;=============================================================================
;;; Datatype `arg`
;;;=============================================================================
(defsyntax arg [arg annotation lineno col_offset]
  "Args:
      arg (identifier)
      annotation (expr?) [optional]
      lineno (int)
      col_offset (int)"
  ; TODO: use `annotation`
  `~(py2hy_mangle_identifier #e #k arg))


;;;=============================================================================
;;; Datatype `keyword`
;;;=============================================================================
(defsyntax keyword [arg value]
  "Args:
      arg (identifier?) [optional]
      value (expr)"
  ; The python code
  ;
  ;     f(*args, **kwargs)
  ;
  ; Becomes compiled to the AST
  ;
  ;     Call(func=Name(id='f', ctx=Load()),
  ;          args=[Starred(value=Name(id='args', ctx=Load()), ctx=Load())],
  ;          keywords=[keyword(arg=None, value=Name(id='kwargs', ctx=Load()))])
  `(~(py2hy_mangle_identifier #e #k arg) ~#e #k value))


;;;=============================================================================
;;; Datatype `alias`
;;;=============================================================================
(defsyntax alias [name asname]
  "Args:
      name (identifier)
      asname (identifier?) [optional]"
  (if #e #k asname
    `[~#e #k name :as ~#e #k asname]
    `[~#e #k name]))


;;;=============================================================================
;;; Datatype `withitem`
;;;=============================================================================
(defsyntax withitem [context_expr optional_vars]
  "Args:
      context_expr (expr)
      optional_vars (expr?) [optional]"
  (setv optional_vars #e #k optional_vars)
  `(~@#A (if optional_vars [optional_vars])
    ~#e #k context_expr))

(defn py2hy_print [ast]
  (print (-> ast (.expand) (prettyprinter))))

(defn py2hy_file [filepath &optional [dumpast False]]
  (setv astobj (-> filepath (open "r") (.read) (ast.parse)))
  (if dumpast
      (do
        (print (ast.dump codeobj)))
      (do
        (py2hy_print astobj))))

(defn py2hy [ast]
  (ast.expand))

(defn main []
  (setv parser (argparse.ArgumentParser))
  (parser.add_argument "filepath")
  (parser.add_argument "--ast" :action "store_true")
  (setv args (parser.parse_args))
  (py2hy_file args.filepath args.ast))

(if (= __name__ "__main__")
  (main))