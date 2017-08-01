(import [hy]
        [ast]
        [codestring]
        [py2ast [Py2ast]])

; Auto-generated template
(defsharp k [key]
  `(get kvdict ~key))
(defsharp l [body]
  `(list (map macroexpand ~body)))
(defsharp m [body]
  `(macroexpand ~body))
(defmacro defsyntax [name keys &rest body]
  `(defmacro ~name [&rest restraw]
     ; `kvdict` is accessible inside `defsyntax` with `#k:keyname`
     (setv kvdict {}
           rest (iter restraw)
           varcount 0)
     ; Associate HyKeyword arguments with its arguments
     (for [f rest]
       (cond
         [(= hy.models.HyKeyword (type f))
          (assoc kvdict f (next rest))]
         [True
          (assoc kvdict varcount f)
          (+= varcount 1)]))
     ; Associate unprovided keys with `None`
     (for [k [~@keys]]
       (when (not (in k (kvdict.keys)))
         (assoc kvdict k None)))
     ~@body))

;==============================================================================
; Classgroup `mod`
;==============================================================================
(defsyntax Module [:body]
  "Args:
      [list] :body (stmt*)"
  `(do
     ~@#l#k:body))

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
  `('defn ~#k:name [~@#k:args]
     ~@#l#k:body))

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
      :col_offset (int)"_
  `(defclass ~#k:name [~@#k:bases]
     ~@#l#k:keywords
     ~@#l#k:body))

(defsyntax Return [:value :lineno :col_offset]
  "Args:
      [optional] :value (expr?)
      :lineno (int)
      :col_offset (int)"
  `(return ~#m#k:value))

(defsyntax Delete [:targets :lineno :col_offset]
  "Args:
      [list] :targets (expr*)
      :lineno (int)
      :col_offset (int)"
  `(del ~@#l#k:targets))

(defsyntax Assign [:targets :value :lineno :col_offset]
  "Args:
      [list] :targets (expr*)
      :value (expr)
      :lineno (int)
      :col_offset (int)"
  `(setv ~@#l#k:targets ~#m#k:value))

(defsyntax AugAssign [:target :op :value :lineno :col_offset]
  "Args:
      :target (expr)
      :op (operator)
      :value (expr)
      :lineno (int)
      :col_offset (int)"
  `(~#m#k:op ~#m#k:target ~#m#k:value))

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
  `(for [~#m#k:target ~#m#k:iter]
     ~@#l#k:body))

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
  `(while ~#m#k:test
     ~@#l#k:body))

(defsyntax If [:test :body :orelse :lineno :col_offset]
  "Args:
      :test (expr)
      [list] :body (stmt*)
      [list] :orelse (stmt*)
      :lineno (int)
      :col_offset (int)"
  `('if ~#m#k:test
     (do
       ~@#l#k:body)
     (do
       ~@#l#k:orelse)))

(defsyntax With [:items :body :lineno :col_offset]
  "Args:
      [list] :items (withitem*)
      [list] :body (stmt*)
      :lineno (int)
      :col_offset (int)"
  `(with [~@#l#k:items]
         ~@#l#k:body))

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
  `(raise ~#m#k:exc ~#m#k:cause))

(defsyntax Try [:body :handlers :orelse :finalbody :lineno :col_offset]
  "Args:
      [list] :body (stmt*)
      [list] :handlers (excepthandler*)
      [list] :orelse (stmt*)
      [list] :finalbody (stmt*)
      :lineno (int)
      :col_offset (int)"
  `(try
     ~@#l#k:body
     (except
       ~@#l#k:handlers)
     (else
       ~@#l#k:orelse)
     (finally
       ~@#l#k:finalbody)))

(defsyntax Assert [:test :msg :lineno :col_offset]
  "Args:
      :test (expr)
      [optional] :msg (expr?)
      :lineno (int)
      :col_offset (int)"
  `(assert ~#m#k:test ~#m#k:msg))

(defsyntax Import [:names :lineno :col_offset]
  "Args:
      [list] :names (alias*)
      :lineno (int)
      :col_offset (int)"
  `(import ~@#l#k:names))

(defsyntax ImportFrom [:module :names :level :lineno :col_offset]
  "Args:
      [optional] :module (identifier?)
      [list] :names (alias*)
      [optional] :level (int?)
      :lineno (int)
      :col_offset (int)"
  `(import [~#k:module [~@#l#k:names]]))

(defsyntax Global [:names :lineno :col_offset]
  "Args:
      [list] :names (identifier*)
      :lineno (int)
      :col_offset (int)"
  `(global ~@#l#k:names))

(defsyntax Nonlocal [:names :lineno :col_offset]
  "Args:
      [list] :names (identifier*)
      :lineno (int)
      :col_offset (int)"
  `(nonlocal ~@#l#k:names))

(defsyntax Expr [:value :lineno :col_offset]
  "Args:
      :value (expr)
      :lineno (int)
      :col_offset (int)"
  #m#k:value)

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
  `(~#m#k:op ~@#l#k:values))

(defsyntax BinOp [:left :op :right :lineno :col_offset]
  "Args:
      :left (expr)
      :op (operator)
      :right (expr)
      :lineno (int)
      :col_offset (int)"
  `(~#m#k:op ~#m#k:left ~#m#k:right))

(defsyntax UnaryOp [:op :operand :lineno :col_offset]
  "Args:
      :op (unaryop)
      :operand (expr)
      :lineno (int)
      :col_offset (int)"
  `(~#m#k:op ~#m#k:operand))

(defsyntax Lambda [:args :body :lineno :col_offset]
  "Args:
      :args (arguments)
      :body (expr)
      :lineno (int)
      :col_offset (int)"
  `(fn [~@#k:args] ~#m#k:body))

(defsyntax IfExp [:test :body :orelse :lineno :col_offset]
  "Args:
      :test (expr)
      :body (expr)
      :orelse (expr)
      :lineno (int)
      :col_offset (int)"
  `('if ~#m#k:test
     ~#m#k:body
     ~#m#k:orelse))

(defsyntax Dict [:keys :values :lineno :col_offset]
  "Args:
      [list] :keys (expr*)
      [list] :values (expr*)
      :lineno (int)
      :col_offset (int)"
  `{~(.join ":" (zip #l#k:keys #l#k:values))})

(defsyntax Set [:elts :lineno :col_offset]
  "Args:
      [list] :elts (expr*)
      :lineno (int)
      :col_offset (int)"
  `(set ~@#l#k:elts))

(defsyntax ListComp [:elt :generators :lineno :col_offset]
  "Args:
      :elt (expr)
      [list] :generators (comprehension*)
      :lineno (int)
      :col_offset (int)"
  `(list-comp ~#m#k:elt
              ~@#l#k:generators))

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
  None)

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
  `(~@#l#k:ops ~#m#k:left ~@#l#k:comparators))

(defsyntax Call [:func :args :keywords :lineno :col_offset]
  "Args:
      :func (expr)
      [list] :args (expr*)
      [list] :keywords (keyword*)
      :lineno (int)
      :col_offset (int)"
  `(~#m#k:func ~@#l#k:args ~@#l#k:keywords))

(defsyntax Num [:n :lineno :col_offset]
  "Args:
      :n (object)
      :lineno (int)
      :col_offset (int)"
  #k:n)

(defsyntax Str [:s :lineno :col_offset]
  "Args:
      :s (string)
      :lineno (int)
      :col_offset (int)"
  #k:s)

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
  #m#k:values)

(defsyntax Bytes [:s :lineno :col_offset]
  "Args:
      :s (bytes)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax NameConstant [:value :lineno :col_offset]
  "Args:
      :value (singleton)
      :lineno (int)
      :col_offset (int)"
  #k:value)

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
  #k:value)

(defsyntax Attribute [:value :attr :ctx :lineno :col_offset]
  "Args:
      :value (expr)
      :attr (identifier)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  `(get ~#m#k:value ~#m#k:attr))

(defsyntax Subscript [:value :slice :ctx :lineno :col_offset]
  "Args:
      :value (expr)
      :slice (slice)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  `(get ~#m#k:value ~#m#k:slice))

(defsyntax Starred [:value :ctx :lineno :col_offset]
  "Args:
      :value (expr)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  `(dispatch_sharp_macro "*" ~#m#k:value))

(defsyntax Name [:id :ctx :lineno :col_offset]
  "Args:
      :id (identifier)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  #k:id)

(defsyntax List [:elts :ctx :lineno :col_offset]
  "Args:
      [list] :elts (expr*)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  `[~@#l#k:elts])

(defsyntax Tuple [:elts :ctx :lineno :col_offset]
  "Args:
      [list] :elts (expr*)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  `(, ~@#l#k:elts))


;==============================================================================
; Classgroup `expr_context`
;==============================================================================
(defsyntax Load []
  "Singleton class")

(defsyntax Store []
  "Singleton class")

(defsyntax Del []
  "Singleton class")

(defsyntax AugLoad []
  "Singleton class")

(defsyntax AugStore []
  "Singleton class")

(defsyntax Param []
  "Singleton class")


;==============================================================================
; Classgroup `slice`
;==============================================================================
(defsyntax Slice [:lower :upper :step]
  "Args:
      [optional] :lower (expr?)
      [optional] :upper (expr?)
      [optional] :step (expr?)"
  `(slice ~#m#k:lower ~#m#k:upper ~#m#k:step))

(defsyntax ExtSlice [:dims]
  "Args:
      [list] :dims (slice*)"
  None)

(defsyntax Index [:value]
  "Args:
      :value (expr)"
  #m#k:value)


;==============================================================================
; Classgroup `boolop`
;==============================================================================
(defsyntax And []
  "Singleton class" `and)

(defsyntax Or []
  "Singleton class" `or)


;==============================================================================
; Classgroup `operator`
;==============================================================================
(defsyntax Add []
  "Singleton class" `+)

(defsyntax Sub []
  "Singleton class" `-)

(defsyntax Mult []
  "Singleton class" `*)

(defsyntax MatMult []
  "Singleton class" `matmul)

(defsyntax Div []
  "Singleton class" `/)

(defsyntax Mod []
  "Singleton class" `mod)

(defsyntax Pow []
  "Singleton class" `**)

(defsyntax LShift []
  "Singleton class" `<<)

(defsyntax RShift []
  "Singleton class" `>>)

(defsyntax BitOr []
  "Singleton class" `|)

(defsyntax BitXor []
  "Singleton class" `^)

(defsyntax BitAnd []
  "Singleton class" `bitand)

(defsyntax FloorDiv []
  "Singleton class" `//)


;==============================================================================
; Classgroup `unaryop`
;==============================================================================
(defsyntax Invert []
  "Singleton class" `invert)

(defsyntax Not []
  "Singleton class" `not)

(defsyntax UAdd []
  "Singleton class" `+)

(defsyntax USub []
  "Singleton class" `-)


;==============================================================================
; Classgroup `cmpop`
;==============================================================================
(defsyntax Eq []
  "Singleton class" `=)

(defsyntax NotEq []
  "Singleton class" `neq)

(defsyntax Lt []
  "Singleton class" `<)

(defsyntax LtE []
  "Singleton class" `<=)

(defsyntax Gt []
  "Singleton class" `>)

(defsyntax GtE []
  "Singleton class" `>=)

(defsyntax Is []
  "Singleton class" `is)

(defsyntax IsNot []
  "Singleton class" `isnot)

(defsyntax In []
  "Singleton class" `in)

(defsyntax NotIn []
  "Singleton class" `notin)


;==============================================================================
; Datatype `comprehension`
;==============================================================================
(defsyntax comprehension [:target :iter :ifs :is_async]
  "Args:
      :target (expr)
      :iter (expr)
      [list] :ifs (expr*)
      :is_async (int)"
  `(list-comp ~#m#k:target
              [~#m#k:target ~#m#k:iter]
              (and ~@#l#k:ifs)))


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
  None)


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
  `(~@#l#k:args
     ~#m#k:vararg
     ~@#l#k:kwonlyargs
     ~@#l#k:kw_defaults
     ~#m#k:kwarg
     ~@#l#k:defaults))


;==============================================================================
; Datatype `arg`
;==============================================================================
(defsyntax arg [:arg :annotation :lineno :col_offset]
  "Args:
      :arg (identifier)
      [optional] :annotation (expr?)
      :lineno (int)
      :col_offset (int)"
  `(~#k:arg ~#k:expr))


;==============================================================================
; Datatype `keyword`
;==============================================================================
(defsyntax keyword [:arg :value]
  "Args:
      [optional] :arg (identifier?)
      :value (expr)"
  `(~#k:arg ~#m#k:value))


;==============================================================================
; Datatype `alias`
;==============================================================================
(defsyntax alias [:name :asname]
  "Args:
      :name (identifier)
      [optional] :asname (identifier?)"
  (if #k:asname
    `[~#k:name :as ~#k:asname]
    `[~#k:name]))


;==============================================================================
; Datatype `withitem`
;==============================================================================
(defsyntax withitem [:context_expr :optional_vars]
  "Args:
      :context_expr (expr)
      [optional] :optional_vars (expr?)"
  `(~#m#k:context_expr
     ~#m#k:optional_vars))



(setv codestring codestring.codestring)
; (print (ast.dump codestring))
(setv grandlist (.visit (Py2ast) codestring))
; (print grandlist)
(setv a (macroexpand grandlist))
(print a)

; (defn recprint [l depth]
;     (if (= hy.models.HyExpression (type l))
;       (do
;         (print (* " " depth) "(" :end "")
;         (print (get l 0))
;         (for [x (drop 1 l)]
;           (print (* " " depth) :end " ")(recprint x (+ 1 depth)))
;         (print (* " " depth) ")"))
;       (print l)))
; (recprint a 0)