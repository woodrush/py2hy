; Auto-generated template

;==============================================================================
; Classgroup `mod`
;==============================================================================
(defsyntax Module [:body]
  "Args:
      [list] :body (stmt*)"
  None)

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
  None)

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
  None)

(defsyntax Return [:value :lineno :col_offset]
  "Args:
      [optional] :value (expr?)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Delete [:targets :lineno :col_offset]
  "Args:
      [list] :targets (expr*)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Assign [:targets :value :lineno :col_offset]
  "Args:
      [list] :targets (expr*)
      :value (expr)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax AugAssign [:target :op :value :lineno :col_offset]
  "Args:
      :target (expr)
      :op (operator)
      :value (expr)
      :lineno (int)
      :col_offset (int)"
  None)

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
  None)

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
  None)

(defsyntax If [:test :body :orelse :lineno :col_offset]
  "Args:
      :test (expr)
      [list] :body (stmt*)
      [list] :orelse (stmt*)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax With [:items :body :lineno :col_offset]
  "Args:
      [list] :items (withitem*)
      [list] :body (stmt*)
      :lineno (int)
      :col_offset (int)"
  None)

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
  None)

(defsyntax Try [:body :handlers :orelse :finalbody :lineno :col_offset]
  "Args:
      [list] :body (stmt*)
      [list] :handlers (excepthandler*)
      [list] :orelse (stmt*)
      [list] :finalbody (stmt*)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Assert [:test :msg :lineno :col_offset]
  "Args:
      :test (expr)
      [optional] :msg (expr?)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Import [:names :lineno :col_offset]
  "Args:
      [list] :names (alias*)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax ImportFrom [:module :names :level :lineno :col_offset]
  "Args:
      [optional] :module (identifier?)
      [list] :names (alias*)
      [optional] :level (int?)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Global [:names :lineno :col_offset]
  "Args:
      [list] :names (identifier*)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Nonlocal [:names :lineno :col_offset]
  "Args:
      [list] :names (identifier*)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Expr [:value :lineno :col_offset]
  "Args:
      :value (expr)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Pass [:lineno :col_offset]
  "Args:
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Break [:lineno :col_offset]
  "Args:
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Continue [:lineno :col_offset]
  "Args:
      :lineno (int)
      :col_offset (int)"
  None)


;==============================================================================
; Classgroup `expr`
;==============================================================================
(defsyntax BoolOp [:op :values :lineno :col_offset]
  "Args:
      :op (boolop)
      [list] :values (expr*)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax BinOp [:left :op :right :lineno :col_offset]
  "Args:
      :left (expr)
      :op (operator)
      :right (expr)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax UnaryOp [:op :operand :lineno :col_offset]
  "Args:
      :op (unaryop)
      :operand (expr)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Lambda [:args :body :lineno :col_offset]
  "Args:
      :args (arguments)
      :body (expr)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax IfExp [:test :body :orelse :lineno :col_offset]
  "Args:
      :test (expr)
      :body (expr)
      :orelse (expr)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Dict [:keys :values :lineno :col_offset]
  "Args:
      [list] :keys (expr*)
      [list] :values (expr*)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Set [:elts :lineno :col_offset]
  "Args:
      [list] :elts (expr*)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax ListComp [:elt :generators :lineno :col_offset]
  "Args:
      :elt (expr)
      [list] :generators (comprehension*)
      :lineno (int)
      :col_offset (int)"
  None)

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
  None)

(defsyntax Call [:func :args :keywords :lineno :col_offset]
  "Args:
      :func (expr)
      [list] :args (expr*)
      [list] :keywords (keyword*)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Num [:n :lineno :col_offset]
  "Args:
      :n (object)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Str [:s :lineno :col_offset]
  "Args:
      :s (string)
      :lineno (int)
      :col_offset (int)"
  None)

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
  None)

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
  None)

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
  None)

(defsyntax Attribute [:value :attr :ctx :lineno :col_offset]
  "Args:
      :value (expr)
      :attr (identifier)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Subscript [:value :slice :ctx :lineno :col_offset]
  "Args:
      :value (expr)
      :slice (slice)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Starred [:value :ctx :lineno :col_offset]
  "Args:
      :value (expr)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Name [:id :ctx :lineno :col_offset]
  "Args:
      :id (identifier)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax List [:elts :ctx :lineno :col_offset]
  "Args:
      [list] :elts (expr*)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  None)

(defsyntax Tuple [:elts :ctx :lineno :col_offset]
  "Args:
      [list] :elts (expr*)
      :ctx (expr_context)
      :lineno (int)
      :col_offset (int)"
  None)


;==============================================================================
; Classgroup `expr_context`
;==============================================================================
(defsyntax Load []
  "Constant expression" None)

(defsyntax Store []
  "Constant expression" None)

(defsyntax Del []
  "Constant expression" None)

(defsyntax AugLoad []
  "Constant expression" None)

(defsyntax AugStore []
  "Constant expression" None)

(defsyntax Param []
  "Constant expression" None)


;==============================================================================
; Classgroup `slice`
;==============================================================================
(defsyntax Slice [:lower :upper :step]
  "Args:
      [optional] :lower (expr?)
      [optional] :upper (expr?)
      [optional] :step (expr?)"
  None)

(defsyntax ExtSlice [:dims]
  "Args:
      [list] :dims (slice*)"
  None)

(defsyntax Index [:value]
  "Args:
      :value (expr)"
  None)


;==============================================================================
; Classgroup `boolop`
;==============================================================================
(defsyntax And []
  "Constant expression" None)

(defsyntax Or []
  "Constant expression" None)


;==============================================================================
; Classgroup `operator`
;==============================================================================
(defsyntax Add []
  "Constant expression" None)

(defsyntax Sub []
  "Constant expression" None)

(defsyntax Mult []
  "Constant expression" None)

(defsyntax MatMult []
  "Constant expression" None)

(defsyntax Div []
  "Constant expression" None)

(defsyntax Mod []
  "Constant expression" None)

(defsyntax Pow []
  "Constant expression" None)

(defsyntax LShift []
  "Constant expression" None)

(defsyntax RShift []
  "Constant expression" None)

(defsyntax BitOr []
  "Constant expression" None)

(defsyntax BitXor []
  "Constant expression" None)

(defsyntax BitAnd []
  "Constant expression" None)

(defsyntax FloorDiv []
  "Constant expression" None)


;==============================================================================
; Classgroup `unaryop`
;==============================================================================
(defsyntax Invert []
  "Constant expression" None)

(defsyntax Not []
  "Constant expression" None)

(defsyntax UAdd []
  "Constant expression" None)

(defsyntax USub []
  "Constant expression" None)


;==============================================================================
; Classgroup `cmpop`
;==============================================================================
(defsyntax Eq []
  "Constant expression" None)

(defsyntax NotEq []
  "Constant expression" None)

(defsyntax Lt []
  "Constant expression" None)

(defsyntax LtE []
  "Constant expression" None)

(defsyntax Gt []
  "Constant expression" None)

(defsyntax GtE []
  "Constant expression" None)

(defsyntax Is []
  "Constant expression" None)

(defsyntax IsNot []
  "Constant expression" None)

(defsyntax In []
  "Constant expression" None)

(defsyntax NotIn []
  "Constant expression" None)


;==============================================================================
; Datatype `comprehension`
;==============================================================================
(defsyntax comprehension [:target :iter :ifs :is_async]
  "Args:
      :target (expr)
      :iter (expr)
      [list] :ifs (expr*)
      :is_async (int)"
  None)


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
  None)


;==============================================================================
; Datatype `arg`
;==============================================================================
(defsyntax arg [:arg :annotation :lineno :col_offset]
  "Args:
      :arg (identifier)
      [optional] :annotation (expr?)
      :lineno (int)
      :col_offset (int)"
  None)


;==============================================================================
; Datatype `keyword`
;==============================================================================
(defsyntax keyword [:arg :value]
  "Args:
      [optional] :arg (identifier?)
      :value (expr)"
  None)


;==============================================================================
; Datatype `alias`
;==============================================================================
(defsyntax alias [:name :asname]
  "Args:
      :name (identifier)
      [optional] :asname (identifier?)"
  None)


;==============================================================================
; Datatype `withitem`
;==============================================================================
(defsyntax withitem [:context_expr :optional_vars]
  "Args:
      :context_expr (expr)
      [optional] :optional_vars (expr?)"
  None)


