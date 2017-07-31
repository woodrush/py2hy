; Auto-generated template

;==============================================================================
; Classgroup `mod`
;==============================================================================
(defsyntax Module [:body]
  "Args:
      [list] :body (stmt*)"
  )

(defsyntax Interactive [:body]
  "Args:
      [list] :body (stmt*)"
  )

(defsyntax Expression [:body]
  "Args:
      :body (expr)"
  )

(defsyntax Suite [:body]
  "Args:
      [list] :body (stmt*)"
  )


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
  )

(defsyntax AsyncFunctionDef [:name :args :body :decorator_list :returns :lineno :col_offset]
  "Args:
      :name (identifier)
      :args (arguments)
      [list] :body (stmt*)
      [list] :decorator_list (expr*)
      [optional] :returns (expr?)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax ClassDef [:name :bases :keywords :body :decorator_list :lineno :col_offset]
  "Args:
      :name (identifier)
      [list] :bases (expr*)
      [list] :keywords (keyword*)
      [list] :body (stmt*)
      [list] :decorator_list (expr*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Return [:value :lineno :col_offset]
  "Args:
      [optional] :value (expr?)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Delete [:targets :lineno :col_offset]
  "Args:
      [list] :targets (expr*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Assign [:targets :value :lineno :col_offset]
  "Args:
      [list] :targets (expr*)
      :value (expr)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax AugAssign [:target :op :value :lineno :col_offset]
  "Args:
      :target (expr)
      :op (operator)
      :value (expr)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax AnnAssign [:target :annotation :value :simple :lineno :col_offset]
  "Args:
      :target (expr)
      :annotation (expr)
      [optional] :value (expr?)
      :simple (int)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax For [:target :iter :body :orelse :lineno :col_offset]
  "Args:
      :target (expr)
      :iter (expr)
      [list] :body (stmt*)
      [list] :orelse (stmt*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax AsyncFor [:target :iter :body :orelse :lineno :col_offset]
  "Args:
      :target (expr)
      :iter (expr)
      [list] :body (stmt*)
      [list] :orelse (stmt*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax While [:test :body :orelse :lineno :col_offset]
  "Args:
      :test (expr)
      [list] :body (stmt*)
      [list] :orelse (stmt*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax If [:test :body :orelse :lineno :col_offset]
  "Args:
      :test (expr)
      [list] :body (stmt*)
      [list] :orelse (stmt*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax With [:items :body :lineno :col_offset]
  "Args:
      [list] :items (withitem*)
      [list] :body (stmt*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax AsyncWith [:items :body :lineno :col_offset]
  "Args:
      [list] :items (withitem*)
      [list] :body (stmt*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Raise [:exc :cause :lineno :col_offset]
  "Args:
      [optional] :exc (expr?)
      [optional] :cause (expr?)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Try [:body :handlers :orelse :finalbody :lineno :col_offset]
  "Args:
      [list] :body (stmt*)
      [list] :handlers (excepthandler*)
      [list] :orelse (stmt*)
      [list] :finalbody (stmt*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Assert [:test :msg :lineno :col_offset]
  "Args:
      :test (expr)
      [optional] :msg (expr?)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Import [:names :lineno :col_offset]
  "Args:
      [list] :names (alias*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax ImportFrom [:module :names :level :lineno :col_offset]
  "Args:
      [optional] :module (identifier?)
      [list] :names (alias*)
      [optional] :level (int?)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Global [:names :lineno :col_offset]
  "Args:
      [list] :names (identifier*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Nonlocal [:names :lineno :col_offset]
  "Args:
      [list] :names (identifier*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Expr [:value :lineno :col_offset]
  "Args:
      :value (expr)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Pass [:lineno :col_offset]
  "Args:
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Break [:lineno :col_offset]
  "Args:
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Continue [:lineno :col_offset]
  "Args:
      :lineno (int)
      :col_offset (int)"
  )


;==============================================================================
; Classgroup `expr`
;==============================================================================
(defsyntax BoolOp [:op :values :lineno :col_offset]
  "Args:
      :op (boolop)
      [list] :values (expr*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax BinOp [:left :op :right :lineno :col_offset]
  "Args:
      :left (expr)
      :op (operator)
      :right (expr)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax UnaryOp [:op :operand :lineno :col_offset]
  "Args:
      :op (unaryop)
      :operand (expr)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Lambda [:args :body :lineno :col_offset]
  "Args:
      :args (arguments)
      :body (expr)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax IfExp [:test :body :orelse :lineno :col_offset]
  "Args:
      :test (expr)
      :body (expr)
      :orelse (expr)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Dict [:keys :values :lineno :col_offset]
  "Args:
      [list] :keys (expr*)
      [list] :values (expr*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Set [:elts :lineno :col_offset]
  "Args:
      [list] :elts (expr*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax ListComp [:elt :generators :lineno :col_offset]
  "Args:
      :elt (expr)
      [list] :generators (comprehension*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax SetComp [:elt :generators :lineno :col_offset]
  "Args:
      :elt (expr)
      [list] :generators (comprehension*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax DictComp [:key :value :generators :lineno :col_offset]
  "Args:
      :key (expr)
      :value (expr)
      [list] :generators (comprehension*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax GeneratorExp [:elt :generators :lineno :col_offset]
  "Args:
      :elt (expr)
      [list] :generators (comprehension*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Await [:value :lineno :col_offset]
  "Args:
      :value (expr)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Yield [:value :lineno :col_offset]
  "Args:
      [optional] :value (expr?)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax YieldFrom [:value :lineno :col_offset]
  "Args:
      :value (expr)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Compare [:left :ops :comparators :lineno :col_offset]
  "Args:
      :left (expr)
      [list] :ops (cmpop*)
      [list] :comparators (expr*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Call [:func :args :keywords :lineno :col_offset]
  "Args:
      :func (expr)
      [list] :args (expr*)
      [list] :keywords (keyword*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Num [:n :lineno :col_offset]
  "Args:
      :n (object)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Str [:s :lineno :col_offset]
  "Args:
      :s (string)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax FormattedValue [:value :conversion :format_spec :lineno :col_offset]
  "Args:
      :value (expr)
      [optional] :conversion (int?)
      [optional] :format_spec (expr?)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax JoinedStr [:values :lineno :col_offset]
  "Args:
      [list] :values (expr*)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Bytes [:s :lineno :col_offset]
  "Args:
      :s (bytes)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax NameConstant [:value :lineno :col_offset]
  "Args:
      :value (singleton)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Ellipsis [:lineno :col_offset]
  "Args:
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Constant [:value :lineno :col_offset]
  "Args:
      :value (constant)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Attribute [:value :attr :ctx :lineno :col_offset]
  "Args:
      :value (expr)
      :attr (identifier)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Subscript [:value :slice :ctx :lineno :col_offset]
  "Args:
      :value (expr)
      :slice (slice)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Starred [:value :ctx :lineno :col_offset]
  "Args:
      :value (expr)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Name [:id :ctx :lineno :col_offset]
  "Args:
      :id (identifier)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax List [:elts :ctx :lineno :col_offset]
  "Args:
      [list] :elts (expr*)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  )

(defsyntax Tuple [:elts :ctx :lineno :col_offset]
  "Args:
      [list] :elts (expr*)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  )


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
  )

(defsyntax ExtSlice [:dims]
  "Args:
      [list] :dims (slice*)"
  )

(defsyntax Index [:value]
  "Args:
      :value (expr)"
  )


;==============================================================================
; Classgroup `boolop`
;==============================================================================
(defsyntax And []
  "Singleton class")

(defsyntax Or []
  "Singleton class")


;==============================================================================
; Classgroup `operator`
;==============================================================================
(defsyntax Add []
  "Singleton class")

(defsyntax Sub []
  "Singleton class")

(defsyntax Mult []
  "Singleton class")

(defsyntax MatMult []
  "Singleton class")

(defsyntax Div []
  "Singleton class")

(defsyntax Mod []
  "Singleton class")

(defsyntax Pow []
  "Singleton class")

(defsyntax LShift []
  "Singleton class")

(defsyntax RShift []
  "Singleton class")

(defsyntax BitOr []
  "Singleton class")

(defsyntax BitXor []
  "Singleton class")

(defsyntax BitAnd []
  "Singleton class")

(defsyntax FloorDiv []
  "Singleton class")


;==============================================================================
; Classgroup `unaryop`
;==============================================================================
(defsyntax Invert []
  "Singleton class")

(defsyntax Not []
  "Singleton class")

(defsyntax UAdd []
  "Singleton class")

(defsyntax USub []
  "Singleton class")


;==============================================================================
; Classgroup `cmpop`
;==============================================================================
(defsyntax Eq []
  "Singleton class")

(defsyntax NotEq []
  "Singleton class")

(defsyntax Lt []
  "Singleton class")

(defsyntax LtE []
  "Singleton class")

(defsyntax Gt []
  "Singleton class")

(defsyntax GtE []
  "Singleton class")

(defsyntax Is []
  "Singleton class")

(defsyntax IsNot []
  "Singleton class")

(defsyntax In []
  "Singleton class")

(defsyntax NotIn []
  "Singleton class")


;==============================================================================
; Datatype `comprehension`
;==============================================================================
(defsyntax comprehension [:target :iter :ifs :is_async]
  "Args:
      :target (expr)
      :iter (expr)
      [list] :ifs (expr*)
      :is_async (int)"
  )


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
  )


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
  )


;==============================================================================
; Datatype `arg`
;==============================================================================
(defsyntax arg [:arg :annotation :lineno :col_offset]
  "Args:
      :arg (identifier)
      [optional] :annotation (expr?)
      :lineno (int)
      :col_offset (int)"
  )


;==============================================================================
; Datatype `keyword`
;==============================================================================
(defsyntax keyword [:arg :value]
  "Args:
      [optional] :arg (identifier?)
      :value (expr)"
  )


;==============================================================================
; Datatype `alias`
;==============================================================================
(defsyntax alias [:name :asname]
  "Args:
      :name (identifier)
      [optional] :asname (identifier?)"
  )


;==============================================================================
; Datatype `withitem`
;==============================================================================
(defsyntax withitem [:context_expr :optional_vars]
  "Args:
      :context_expr (expr)
      [optional] :optional_vars (expr?)"
  )


