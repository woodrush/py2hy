(import [hy]
        [sys]
        [ast]
        [re]
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
                                      [(= bool (type x)) (hy.models.HySymbol (hy.models.HySymbol (str x)))]
                                      [(= list (type x)) (hy.models.HyList x)]
                                      [(.startswith x ":") (hy.models.HyKeyword x)]
                                      [True (hy.models.HySymbol x)])))
                                ~body))))

(deftag m [x]
  `(do
     (if (hasattr ~x "expand")
       ((. ~x expand))
       (cond
         [(is None ~x) None]
         [(= int (type ~x)) (hy.models.HyInteger ~x)]
         [(= bool (type ~x)) (hy.models.HySymbol (hy.models.HySymbol (str ~x)))]
         [(= list (type ~x)) (hy.models.HyList ~x)]
         [(.startswith ~x ":") (hy.models.HyKeyword ~x)]
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
  (setv body #l #k :body)
  `(do
     ~@(if (in "Return" (body.__repr__))
         `[(defclass Py2HyReturnException [Exception]
             (defn __init__ [self retvalue]
               (setv self.retvalue retvalue)))])
     ~@#l #k :body))

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
  (setv body #l #k :body)
  (if (in "Return" (body.__repr__))
    `(defn ~(mangle_identifier #m #k :name) ~#m #k :args
       "Using a hacky implementation of `return`"
       (try
         (do
           ~@#l #k :body)
         (except [e Py2HyReturnException]
           e.retvalue)))
    `(defn ~(mangle_identifier #m #k :name) ~#m #k :args
       ~@#l #k :body)))

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
     ~@#l #k :body))

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
  `(setv ~@(interleave #l #k :targets (repeat #m #k :value))))

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
  `(for [~#m #k :target ~#m #k :iter]
     ~@#l #k :body))

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
       (do ~@#l #k :body))))

(defsyntax With [:items :body :lineno :col_offset]
  "Args:
      [list] :items (withitem*)
      [list] :body (stmt*)
      :lineno (int)
      :col_offset (int)"
  `(with [~@#l #k :items]
         ~@#l #k :body))

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
  `(raise ~#m #k :exc))

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
       ~@#l #k :body)
     ~@#l #k :handlers
     ~@(if (< 0 (len orelse))
         orelse)
     ~@(if (< 0 (len finalbody))
         finalbody)))

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
  None)

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
  None)

(defsyntax DictComp [:key :value :generators :lineno :col_offset]
  "Args:
      :key (expr)
      :value (expr)
      [list] :generators (comprehension*)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax GeneratorExp [:elt :generators :lineno :col_offset]
  "Args:
      :elt (expr)
      [list] :generators (comprehension*)
      :lineno (int)
      :col_offset (int)"
  None)

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
  `(yield ~#m #k :value))

(defsyntax YieldFrom [:value :lineno :col_offset]
  "Args:
      :value (expr)
      :lineno (int)
      :col_offset (int)"
  None)

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
  `(~#m #k :func
    ~@#l #k :args
    ~@(reduce (fn [x y] (+ x y)) (map (fn [l] [(hy.models.HyKeyword (+ ":" (nth l 0))) (nth l 1)]) #l #k :keywords) [])))

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
  `(. ~#m #k :value ~#m #k :attr)
  )

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
  `(dispatch_sharp_macro "*" ~#m #k :value))

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
  "Constant expression" `mod)

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
  (setv ifs #l #k :ifs)
  `[[~#m #k :target ~#m #k :iter]
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
  `(except [~@(if e_name [e_name]) ~@(if e_type [e_type])]
     ~#l #k :body))


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
  (defn len [iter]
    (sum (list-comp 1 [x iter])))
  (defn take-last [n l]
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
    ~@(if kwarg `[&kwarg ~kwarg])
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
  `(~#m #k :context_expr
     ~#m #k :optional_vars))



; (setv codestring (-> sys.argv (get 1) (open "r") (.read) (ast.parse)))
; ; (print ";" (ast.dump codestring))
; (setv grandlist (.visit (Py2ast) codestring))
; (setv a (macroexpand-1 grandlist))
(setv codeobj (-> sys.argv (get 1) (open "r") (.read) (ast.parse)))
(setv a (codeobj.expand))

; Modify `__repr__` to suppress `'`
(setv hy.models.HySymbol.__repr__ (fn [self] (+ "" self)))
; Modify `__repr__` for escaping
(setv hy.models.HyString.__repr__ (fn [self] (+ "\"" (re.sub "\"" "\\\"" self) "\"")))
; Modify `__repr__` for escaping
(setv hy.models.HyKeyword.__repr__ (fn [self] (.join "" (drop 1 self))))
; Modify `__repr__` for escaping
(setv hy.models.HyList.__repr__ (fn [self] (+ "[" (.join " " (map (fn [x] (x.__repr__)) self)) "]")))
; (setv hy.models.HyList.__repr__ (fn [self] (.join " " (map (fn [x] (x.__repr__)) self))))
(for [x (drop 1 a)]
  (print x))