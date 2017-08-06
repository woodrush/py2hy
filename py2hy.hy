(import [hy]
        [sys]
        [ast]
        [re]
        [argparse]
        [py2ast [Py2ast]])

(deftag k [key]
  `(. self ~(hy.models.HySymbol (.join "" (drop 2 key)))))

(deftag l [body]
  `(hy.models.HyList (list (map (fn [x]
                                  (if (hasattr x "expand")
                                    (x.expand)
                                    (cond
                                      [(is None x) None]
                                      [(= int (type x)) (hy.models.HyInteger x)]
                                      [(= float (type x)) (hy.models.HyFloat x)]
                                      [(= complex (type x)) (hy.models.HyComplex x)]
                                      [(= bool (type x)) (hy.models.HySymbol (hy.models.HySymbol (str x)))]
                                      [(= list (type x)) (hy.models.HyList x)]
                                      ; [(.startswith x ":") (hy.models.HyKeyword x)]
                                      [True (hy.models.HySymbol x)])))
                                ~body))))

(defclass Py2HyNewline [object]
  (defn __repr__ [self]
    "\n"))
(defn newliner [iter]
  (drop-last 1 (interleave iter (repeat (Py2HyNewline)))))
(deftag m [x]
  `(do
     (if (hasattr ~x "expand")
       ((. ~x expand))
       (cond
         [(is None ~x) None]
         [(= int (type ~x)) (hy.models.HyInteger ~x)]
         [(= float (type ~x)) (hy.models.HyFloat ~x)]
         [(= complex (type ~x)) (hy.models.HyComplex ~x)]
         [(= bool (type ~x)) (hy.models.HySymbol (hy.models.HySymbol (str ~x)))]
         [(= list (type ~x)) (hy.models.HyList ~x)]
         ; [(.startswith ~x ":") (hy.models.HyKeyword ~x)]
         [True (hy.models.HySymbol ~x)]))))

(defmacro defsyntax [name keys &rest body]
  ; Uncomment this for checking progress
  ; (print "; Defining syntax" name "...")
  `(setv (. (. ast ~name) expand)
         (fn [self]
           ; Uncomment this for checking progress
           ; (print "; Expanding" '~name "...")
           ~@body)))

(setv hy_reserved_keywords
      `[fn defn defclass])
(defn mangle_identifier [x]
  (if (in x hy_reserved_keywords)
    (hy.models.HySymbol (+ x "_py2hy_mangling"))
    x))

;==============================================================================
; Classgroup `mod`
;==============================================================================
(defsyntax Module [:body]
  "Args:
      [list] :body (stmt*)"
  (setv bodylist #l #k :body
        body (iter bodylist))
  (setv n (first body))
  ; (setv r (if (in "Return" (bodylist.__repr__))
  ;           `[(defclass Py2HyReturnException [Exception]
  ;               (defn __init__ [self retvalue]
  ;                 (setv self.retvalue retvalue)))]))
  (setv r `[(defclass Py2HyReturnException [Exception]
              (defn __init__ [self retvalue]
                (setv self.retvalue retvalue)))])

  ; `from __future__ import *` must be imported at the top of the file
  `(do
     ~@(if (and (= hy.models.HyExpression (type n))
                (= 'import (first n)))
         `[~n ~@r]
         `[~@r ~n])
     ~@body))

(defsyntax Interactive [:body]
  "Args:
      [list] :body (stmt*)"
  None)

(defsyntax Expression [:body]
  "Args:
      :body (expr)"
  None)

(defsyntax Suite [:body]
  "Args:
      [list] :body (stmt*)"
  None)


;==============================================================================
; Classgroup `stmt`
;==============================================================================
(defsyntax FunctionDef [:name :args :body :decorator_list :returns :lineno :col_offset]
  "Args:
      :name (identifier)
      :args (arguments)
      [list] :body (stmt*)
      [list] :decorator_list (expr*)
      [optional] :returns (expr?)
      :lineno (int)
      :col_offset (int)"
  (setv body #l #k :body
        decorator_list #l #k :decorator_list)
  (setv main_body
        (cond
          ; If there are no `return` statements, don't add the `try` construct
          [(not-in "Return" (body.__repr__))
           `(defn ~(mangle_identifier #m #k :name) ~#m #k :args
              ~(Py2HyNewline)
              ~@(newliner body))]
          ; If there are no docstrings, put one
          [(!= hy.models.HyString (type (first body)))
           `(defn ~(mangle_identifier #m #k :name) ~#m #k :args
              ~(Py2HyNewline)
              "Using a hacky implementation of `return`"
              ~(Py2HyNewline)
              (try
                (do
                  ~@(newliner body))
                (except [e Py2HyReturnException]
                  e.retvalue)))]
          ; If there are docstrings, keep them, and append another
          [True
           `(defn ~(mangle_identifier #m #k :name) ~#m #k :args
              ~(Py2HyNewline)
              ~(first body)
              ~(Py2HyNewline)
              "Using a hacky implementation of `return`"
              ~(Py2HyNewline)
              (try
                (do
                  ~@(newliner (rest body)))
                (except [e Py2HyReturnException]
                  e.retvalue)))]))
  (if decorator_list
    `(with-decorator
       ~(Py2HyNewline)
       ~@decorator_list
       ~(Py2HyNewline)
       ~main_body)
    main_body))

(defsyntax AsyncFunctionDef [:name :args :body :decorator_list :returns :lineno :col_offset]
  "Args:
      :name (identifier)
      :args (arguments)
      [list] :body (stmt*)
      [list] :decorator_list (expr*)
      [optional] :returns (expr?)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax ClassDef [:name :bases :keywords :body :decorator_list :lineno :col_offset]
  "Args:
      :name (identifier)
      [list] :bases (expr*)
      [list] :keywords (keyword*)
      [list] :body (stmt*)
      [list] :decorator_list (expr*)
      :lineno (int)
      :col_offset (int)"
  ; TODO: defclass
  `(defclass ~(mangle_identifier #m #k :name) [~@#l #k :bases]
     ~@(newliner #l #k :body)))

(defsyntax Return [:value :lineno :col_offset]
  "Args:
      [optional] :value (expr?)
      :lineno (int)
      :col_offset (int)"
  `(raise (Py2HyReturnException ~#m #k :value)))

(defsyntax Delete [:targets :lineno :col_offset]
  "Args:
      [list] :targets (expr*)
      :lineno (int)
      :col_offset (int)"
  `(del ~@#l #k :targets))

(defsyntax Assign [:targets :value :lineno :col_offset]
  "Args:
      [list] :targets (expr*)
      :value (expr)
      :lineno (int)
      :col_offset (int)"

  (setv targets #l #k :targets)
  (setv g (if (or (< 1 (len targets))
                  (= ', (first (first targets))))
            (hy.models.HySymbol (+ "_py2hy_anon_var_" (.join "" (drop 1 (gensym)))))
            #m #k :value))
  (setv typedict {ast.Tuple
                  (fn [target value]
                    `(do
                       ~@(map (fn [l] ((get typedict (type (first l)))
                                       (first l)
                                       (second l)))
                              (zip target.elts
                                   (map
                                     (fn [t] `(nth ~(second t) ~(first t)))
                                     (enumerate (repeat value)))))))
                  ast.Subscript
                  (fn [target value]
                    (setv target #m target)
                    `(assoc ~(nth target 1) ~(nth target 2) ~value))
                  ast.Attribute
                  (fn [target value]
                    (setv target #m target)
                    `(setv ~target ~value))
                  ast.Name
                  (fn [target value]
                    (setv target #m target)
                    (if (= '_ target)
                      `(do)
                      `(setv ~target ~value)))})
  `(do
     ~@(if (or (< 1 (len targets))
               (= ', (first (first targets))))
         [`(setv ~g ~#m #k :value)])
     ~@(map (fn [l] ((get typedict (type (first l))) (first l) (second l)))
            (zip #k :targets
                 (repeat g)))))

(defsyntax AugAssign [:target :op :value :lineno :col_offset]
  "Args:
      :target (expr)
      :op (operator)
      :value (expr)
      :lineno (int)
      :col_offset (int)"
      ; TODO
  `(setv ~#m #k :target (~#m #k :op ~#m #k :target ~#m #k :value)))

(defsyntax AnnAssign [:target :annotation :value :simple :lineno :col_offset]
  "Args:
      :target (expr)
      :annotation (expr)
      [optional] :value (expr?)
      :simple (int)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax For [:target :iter :body :orelse :lineno :col_offset]
  "Args:
      :target (expr)
      :iter (expr)
      [list] :body (stmt*)
      [list] :orelse (stmt*)
      :lineno (int)
      :col_offset (int)"
  (setv target #m #k :target)
  `(for [~@(if (= ', (first target))
             [`[~@(rest target)]]
             [target])
         ~#m #k :iter]
     ~(Py2HyNewline)
     ~@(newliner #l #k :body)))

(defsyntax AsyncFor [:target :iter :body :orelse :lineno :col_offset]
  "Args:
      :target (expr)
      :iter (expr)
      [list] :body (stmt*)
      [list] :orelse (stmt*)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax While [:test :body :orelse :lineno :col_offset]
  "Args:
      :test (expr)
      [list] :body (stmt*)
      [list] :orelse (stmt*)
      :lineno (int)
      :col_offset (int)"
  `(while ~#m #k :test
     ~@#l #k :body))

(defsyntax If [:test :body :orelse :lineno :col_offset]
  "Args:
      :test (expr)
      [list] :body (stmt*)
      [list] :orelse (stmt*)
      :lineno (int)
      :col_offset (int)"
  (setv orelse #l #k :orelse)
  (if orelse
    `(if ~#m #k :test
       (do
         ~@#l #k :body)
       (do
         ~@orelse))
    `(when ~#m #k :test
       (do ~@(newliner #l #k :body)))))

(defsyntax With [:items :body :lineno :col_offset]
  "Args:
      [list] :items (withitem*)
      [list] :body (stmt*)
      :lineno (int)
      :col_offset (int)"
  (defn nest-with [l]
    (if (empty? l)
      (newliner #l #k :body)
      `[(with [~@(first l)]
             ~@(nest-with (list (drop 1 l))))]))
  (first (nest-with #l #k :items)))

(defsyntax AsyncWith [:items :body :lineno :col_offset]
  "Args:
      [list] :items (withitem*)
      [list] :body (stmt*)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Raise [:exc :cause :lineno :col_offset]
  "Args:
      [optional] :exc (expr?)
      [optional] :cause (expr?)
      :lineno (int)
      :col_offset (int)"
  ; TODO: cause
  (setv exc #m #k :exc)
  `(raise ~@(if exc [exc])))

(defsyntax Try [:body :handlers :orelse :finalbody :lineno :col_offset]
  "Args:
      [list] :body (stmt*)
      [list] :handlers (excepthandler*)
      [list] :orelse (stmt*)
      [list] :finalbody (stmt*)
      :lineno (int)
      :col_offset (int)"
  (setv orelse #l #k :orelse
        finalbody #l #k :finalbody)
  `(try
     (do
       ~@(newliner #l #k :body))
     (except [e Py2HyReturnException]
       (raise e))
     ~@#l #k :handlers
     ~@(if (< 0 (len orelse))
         `[(else
            ~@orelse)])
     ~@(if (< 0 (len finalbody))
         `[(finally
            ~@finalbody)])))

(defsyntax Assert [:test :msg :lineno :col_offset]
  "Args:
      :test (expr)
      [optional] :msg (expr?)
      :lineno (int)
      :col_offset (int)"
  `(assert ~#m #k :test ~#m #k :msg))

(defsyntax Import [:names :lineno :col_offset]
  "Args:
      [list] :names (alias*)
      :lineno (int)
      :col_offset (int)"
  `(import ~@#l #k :names))

(defsyntax ImportFrom [:module :names :level :lineno :col_offset]
  "Args:
      [optional] :module (identifier?)
      [list] :names (alias*)
      [optional] :level (int?)
      :lineno (int)
      :col_offset (int)"
  `(import [~#m #k :module [~@(reduce + #l #k :names)]]))

(defsyntax Global [:names :lineno :col_offset]
  "Args:
      [list] :names (identifier*)
      :lineno (int)
      :col_offset (int)"
  `(global ~@(map mangle_identifier #l #k :names)))

(defsyntax Nonlocal [:names :lineno :col_offset]
  "Args:
      [list] :names (identifier*)
      :lineno (int)
      :col_offset (int)"
  `(nonlocal ~@(map mangle_identifier #l #k :names)))

(defsyntax Expr [:value :lineno :col_offset]
  "Args:
      :value (expr)
      :lineno (int)
      :col_offset (int)"
  #m #k :value)

(defsyntax Pass [:lineno :col_offset]
  "Args:
      :lineno (int)
      :col_offset (int)"
  `(do))

(defsyntax Break [:lineno :col_offset]
  "Args:
      :lineno (int)
      :col_offset (int)"
  `(break))

(defsyntax Continue [:lineno :col_offset]
  "Args:
      :lineno (int)
      :col_offset (int)"
  `(continue))


;==============================================================================
; Classgroup `expr`
;==============================================================================
(defsyntax BoolOp [:op :values :lineno :col_offset]
  "Args:
      :op (boolop)
      [list] :values (expr*)
      :lineno (int)
      :col_offset (int)"
  `(~#m #k :op ~@#l #k :values))

(defsyntax BinOp [:left :op :right :lineno :col_offset]
  "Args:
      :left (expr)
      :op (operator)
      :right (expr)
      :lineno (int)
      :col_offset (int)"
  `(~#m #k :op ~#m #k :left ~#m #k :right))

(defsyntax UnaryOp [:op :operand :lineno :col_offset]
  "Args:
      :op (unaryop)
      :operand (expr)
      :lineno (int)
      :col_offset (int)"
  `(~#m #k :op ~#m #k :operand))

(defsyntax Lambda [:args :body :lineno :col_offset]
  "Args:
      :args (arguments)
      :body (expr)
      :lineno (int)
      :col_offset (int)"
  `(fn ~#m #k :args ~#m #k :body))

(defsyntax IfExp [:test :body :orelse :lineno :col_offset]
  "Args:
      :test (expr)
      :body (expr)
      :orelse (expr)
      :lineno (int)
      :col_offset (int)"
  `(if ~#m #k :test
     ~#m #k :body
     ~#m #k :orelse))

(defsyntax Dict [:keys :values :lineno :col_offset]
  "Args:
      [list] :keys (expr*)
      [list] :values (expr*)
      :lineno (int)
      :col_offset (int)"
  `{~@(interleave #l #k :keys #l #k :values)})

(defsyntax Set [:elts :lineno :col_offset]
  "Args:
      [list] :elts (expr*)
      :lineno (int)
      :col_offset (int)"
  `(set ~@#l #k :elts))

(defsyntax ListComp [:elt :generators :lineno :col_offset]
  "Args:
      :elt (expr)
      [list] :generators (comprehension*)
      :lineno (int)
      :col_offset (int)"
  `(list-comp ~#m #k :elt
              ~@(reduce (fn [x y] (+ x y)) #l #k :generators)))

(defsyntax SetComp [:elt :generators :lineno :col_offset]
  "Args:
      :elt (expr)
      [list] :generators (comprehension*)
      :lineno (int)
      :col_offset (int)"
  `(set-comp ~#m #k :elt
             ~@(reduce (fn [x y] (+ x y)) #l #k :generators)))

(defsyntax DictComp [:key :value :generators :lineno :col_offset]
  "Args:
      :key (expr)
      :value (expr)
      [list] :generators (comprehension*)
      :lineno (int)
      :col_offset (int)"
  `(dict-comp ~#m #k :key
              ~#m #k :value
              ~@(reduce (fn [x y] (+ x y)) #l #k :generators)))

(defsyntax GeneratorExp [:elt :generators :lineno :col_offset]
  "Args:
      :elt (expr)
      [list] :generators (comprehension*)
      :lineno (int)
      :col_offset (int)"
  `(genexpr ~#m #k :elt
            ~@(reduce (fn [x y] (+ x y)) #l #k :generators)))

(defsyntax Await [:value :lineno :col_offset]
  "Args:
      :value (expr)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Yield [:value :lineno :col_offset]
  "Args:
      [optional] :value (expr?)
      :lineno (int)
      :col_offset (int)"
  (setv value #m #k :value)
  `(yield ~@(if value [value])))

(defsyntax YieldFrom [:value :lineno :col_offset]
  "Args:
      :value (expr)
      :lineno (int)
      :col_offset (int)"
  `(yield_from ~@(if value [value])))

(defsyntax Compare [:left :ops :comparators :lineno :col_offset]
  "Args:
      :left (expr)
      [list] :ops (cmpop*)
      [list] :comparators (expr*)
      :lineno (int)
      :col_offset (int)"
  `(~@#l #k :ops ~#m #k :left ~@#l #k :comparators))

(defsyntax Call [:func :args :keywords :lineno :col_offset]
  "Args:
      :func (expr)
      [list] :args (expr*)
      [list] :keywords (keyword*)
      :lineno (int)
      :col_offset (int)"
  (setv keywords #l #k :keywords)
  `(~#m #k :func
    ~@#l #k :args
    ~@(if keywords
        (reduce (fn [x y] (if (first y)
                            (+ x y)
                            `[(~'unpack_mapping ~(second y))]))
                (map (fn [l] [(if (nth l 0)
                                (hy.models.HyKeyword (+ ":" (nth l 0)))
                                None) (nth l 1)])
                     #l #k :keywords)
                []))))

(defsyntax Num [:n :lineno :col_offset]
  "Args:
      :n (object)
      :lineno (int)
      :col_offset (int)"
  #m #k :n)

(defsyntax Str [:s :lineno :col_offset]
  "Args:
      :s (string)
      :lineno (int)
      :col_offset (int)"
  (hy.models.HyString #m #k :s))

(defsyntax FormattedValue [:value :conversion :format_spec :lineno :col_offset]
  "Args:
      :value (expr)
      [optional] :conversion (int?)
      [optional] :format_spec (expr?)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax JoinedStr [:values :lineno :col_offset]
  "Args:
      [list] :values (expr*)
      :lineno (int)
      :col_offset (int)"
  #m #k :values)

(defsyntax Bytes [:s :lineno :col_offset]
  "Args:
      :s (bytes)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax NameConstant [:value :lineno :col_offset]
  "Args:
      :value (Constant)
      :lineno (int)
      :col_offset (int)"
  #m #k :value)

(defsyntax Ellipsis [:lineno :col_offset]
  "Args:
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Constant [:value :lineno :col_offset]
  "Args:
      :value (constant)
      :lineno (int)
      :col_offset (int)"
  #m #k :value)

(defsyntax Attribute [:value :attr :ctx :lineno :col_offset]
  "Args:
      :value (expr)
      :attr (identifier)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  ; (setv s (gensym)
  ;       a (hy.models.HySymbol (+ s "." #m #k :attr)))
  ; (print (type a))
  ; (print a)
  ; `(do
  ;    (setv ~s ~#m #k :value)
  ;    ~a)
  (setv value #m #k :value)
  (cond
    [
     ; False
     (= hy.models.HySymbol (type value))
     (hy.models.HySymbol (+ (str value) "." #k :attr))]
    [True
     `(. ~#m #k :value ~#m #k :attr)]))

(defsyntax Subscript [:value :slice :ctx :lineno :col_offset]
  "Args:
      :value (expr)
      :slice (slice)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  `(get ~#m #k :value ~#m #k :slice))

(defsyntax Starred [:value :ctx :lineno :col_offset]
  "Args:
      :value (expr)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  `(~'unpack_iterable ~#m #k :value))

(defsyntax Name [:id :ctx :lineno :col_offset]
  "Args:
      :id (identifier)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  (mangle_identifier #m #k :id))

(defsyntax List [:elts :ctx :lineno :col_offset]
  "Args:
      [list] :elts (expr*)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  `[~@#l #k :elts])

(defsyntax Tuple [:elts :ctx :lineno :col_offset]
  "Args:
      [list] :elts (expr*)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  `(, ~@#l #k :elts))


;==============================================================================
; Classgroup `expr_context`
;==============================================================================
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


;==============================================================================
; Classgroup `slice`
;==============================================================================
(defsyntax Slice [:lower :upper :step]
  "Args:
      [optional] :lower (expr?)
      [optional] :upper (expr?)
      [optional] :step (expr?)"
  `(slice ~#m #k :lower ~#m #k :upper ~#m #k :step))

(defsyntax ExtSlice [:dims]
  "Args:
      [list] :dims (slice*)"
  None)

(defsyntax Index [:value]
  "Args:
      :value (expr)"
  #m #k :value)


;==============================================================================
; Classgroup `boolop`
;==============================================================================
(defsyntax And []
  "Constant expression" `and)

(defsyntax Or []
  "Constant expression" `or)


;==============================================================================
; Classgroup `operator`
;==============================================================================
(defsyntax Add []
  "Constant expression" `+)

(defsyntax Sub []
  "Constant expression" `-)

(defsyntax Mult []
  "Constant expression" `*)

(defsyntax MatMult []
  "Constant expression" `matmul)

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
  "Constant expression" `bitand)

(defsyntax FloorDiv []
  "Constant expression" `//)


;==============================================================================
; Classgroup `unaryop`
;==============================================================================
(defsyntax Invert []
  "Constant expression" `invert)

(defsyntax Not []
  "Constant expression" `not)

(defsyntax UAdd []
  "Constant expression" `+)

(defsyntax USub []
  "Constant expression" `-)


;==============================================================================
; Classgroup `cmpop`
;==============================================================================
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


;==============================================================================
; Datatype `comprehension`
;==============================================================================
(defsyntax comprehension [:target :iter :ifs :is_async]
  "Args:
      :target (expr)
      :iter (expr)
      [list] :ifs (expr*)
      :is_async (int)"
  (setv target #m #k :target
        ifs #l #k :ifs)
  `[[~@(if (= ', (first target))
         [`[~@(rest target)]]
         [target])
     ~#m #k :iter]
    ~@(if (< 0 (len ifs))
        `[(and ~@ifs)])])


;==============================================================================
; Classgroup `excepthandler`
;==============================================================================
(defsyntax ExceptHandler [:type :name :body :lineno :col_offset]
  "Args:
      [optional] :type (expr?)
      [optional] :name (identifier?)
      [list] :body (stmt*)
      :lineno (int)
      :col_offset (int)"
  (setv e_name (mangle_identifier #m #k :name)
        e_type #m #k :type)
  `(except [~@(if e_name [e_name])
            ~@(cond
                [(is None e_type) None]
                [(= ', (first e_type)) [`[~@(rest e_type)]]]
                [True [e_type]])]
     ~@#l #k :body))


;==============================================================================
; Datatype `arguments`
;==============================================================================
(defsyntax arguments [:args :vararg :kwonlyargs :kw_defaults :kwarg :defaults]
  "Args:
      [list] :args (arg*)
      [optional] :vararg (arg?)
      [list] :kwonlyargs (arg*)
      [list] :kw_defaults (expr*)
      [optional] :kwarg (arg?)
      [list] :defaults (expr*)"
  ; TODO: kwonlyargs
  (setv args #l #k :args
        vararg #m #k :vararg
        kwarg #m #k :kwarg
        defaults #l #k :defaults
        kwonlyargs #l #k :kwonlyargs
        kw_defaults #l #k :kw_defaults)
  (defn take-last [n l]
    (defn len [iter]
      (sum (list-comp 1 [x iter])))
    (drop (- (len l) n) l))
  `[~@(drop-last (len defaults) args)
    ~@(if defaults
        `[&optional
          ~@(list-comp `[~x ~y]
                       [[x y] (zip (take-last (len defaults) args)
                                   defaults)])])
    ~@(if kwonlyargs
        `[&kwonly
          ~@(drop-last (len kw_defaults) kwonlyargs)
          ~@(list-comp `[~x ~y]
                       [[x y] (zip (take-last (len kw_defaults) kwonlyargs)
                                   kw_defaults)])])
    ~@(if kwarg `[&kwargs ~kwarg])
    ~@(if vararg `[&rest ~vararg])])


;==============================================================================
; Datatype `arg`
;==============================================================================
(defsyntax arg [:arg :annotation :lineno :col_offset]
  "Args:
      :arg (identifier)
      [optional] :annotation (expr?)
      :lineno (int)
      :col_offset (int)"
  ; TODO: use `:annotation`
  `~(mangle_identifier #m #k :arg))


;==============================================================================
; Datatype `keyword`
;==============================================================================
(defsyntax keyword [:arg :value]
  "Args:
      [optional] :arg (identifier?)
      :value (expr)"
  ; The python code
  ;
  ;     f(*args, **kwargs)
  ;
  ; Becomes compiled to the AST
  ;
  ;     Call(func=Name(id='f', ctx=Load()),
  ;          args=[Starred(value=Name(id='args', ctx=Load()), ctx=Load())],
  ;          keywords=[keyword(arg=None, value=Name(id='kwargs', ctx=Load()))])
  `(~(mangle_identifier #m #k :arg) ~#m #k :value))


;==============================================================================
; Datatype `alias`
;==============================================================================
(defsyntax alias [:name :asname]
  "Args:
      :name (identifier)
      [optional] :asname (identifier?)"
  (if #m #k :asname
    `[~#m #k :name :as ~#m #k :asname]
    `[~#m #k :name]))


;==============================================================================
; Datatype `withitem`
;==============================================================================
(defsyntax withitem [:context_expr :optional_vars]
  "Args:
      :context_expr (expr)
      [optional] :optional_vars (expr?)"
  (setv optional_vars #m #k :optional_vars)
  `(~@(if optional_vars [optional_vars])
    ~#m #k :context_expr))


(setv parser (argparse.ArgumentParser))
(parser.add_argument "filepath")
(parser.add_argument "--ast" :action "store_true")
(setv args (parser.parse_args))
(setv codeobj (-> args.filepath (open "r") (.read) (ast.parse)))
(if args.ast
  (do
    (print (ast.dump codeobj)))
  (do
    (setv a (codeobj.expand))
    ; Modify `__repr__` to suppress `'`
    (setv hy.models.HySymbol.__repr__ (fn [self] (+ "" self)))
    (setv hy.models.HyInteger.__repr__ (fn [self] (+ "" (str self))))
    (setv hy.models.HyFloat.__repr__ (fn [self] (+ "" (str self))))
    (setv hy.models.HyComplex.__repr__ (fn [self] (+ "" (str self))))
    ; Modify `__repr__` for escaping
    (setv hy.models.HyString.__repr__ (fn [self] (+ "\"" (->> self
                                                              (re.sub "\\\\" (+ "\\\\" "\\\\"))
                                                              (re.sub "\"" "\\\"")) "\"")))
    ; Modify `__repr__` for escaping
    (setv hy.models.HyKeyword.__repr__ (fn [self] (.join "" (drop 1 self))))
    ; Modify `__repr__` for escaping
    (setv hy.models.HyList.__repr__ (fn [self] (+ "[" (.join " " (map (fn [x] (x.__repr__)) self)) "]")))
    ; (setv hy.models.HyList.__repr__ (fn [self] (.join " " (map (fn [x] (x.__repr__)) self))))
    (for [x (drop 1 a)]
      (print x))))
