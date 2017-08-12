; Auto-generated template

;==============================================================================
; Classgroup `mod`
;==============================================================================
(defsyntax Module [body]
  "Args:
      body (stmt*) [list]"
  None)

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


;==============================================================================
; Classgroup `stmt`
;==============================================================================
(defsyntax FunctionDef [name args body decorator_list returns lineno col_offset]
  "Args:
      name (identifier)
      args (arguments)
      body (stmt*) [list]
      decorator_list (expr*) [list]
      returns (expr?) [optional]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax AsyncFunctionDef [name args body decorator_list returns lineno col_offset]
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
  None)

(defsyntax Return [value lineno col_offset]
  "Args:
      value (expr?) [optional]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Delete [targets lineno col_offset]
  "Args:
      targets (expr*) [list]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Assign [targets value lineno col_offset]
  "Args:
      targets (expr*) [list]
      value (expr)
      lineno (int)
      col_offset (int)"
  None)

(defsyntax AugAssign [target op value lineno col_offset]
  "Args:
      target (expr)
      op (operator)
      value (expr)
      lineno (int)
      col_offset (int)"
  None)

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
  None)

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
  None)

(defsyntax If [test body orelse lineno col_offset]
  "Args:
      test (expr)
      body (stmt*) [list]
      orelse (stmt*) [list]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax With [items body lineno col_offset]
  "Args:
      items (withitem*) [list]
      body (stmt*) [list]
      lineno (int)
      col_offset (int)"
  None)

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
  None)

(defsyntax Try [body handlers orelse finalbody lineno col_offset]
  "Args:
      body (stmt*) [list]
      handlers (excepthandler*) [list]
      orelse (stmt*) [list]
      finalbody (stmt*) [list]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Assert [test msg lineno col_offset]
  "Args:
      test (expr)
      msg (expr?) [optional]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Import [names lineno col_offset]
  "Args:
      names (alias*) [list]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax ImportFrom [module names level lineno col_offset]
  "Args:
      module (identifier?) [optional]
      names (alias*) [list]
      level (int?) [optional]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Global [names lineno col_offset]
  "Args:
      names (identifier*) [list]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Nonlocal [names lineno col_offset]
  "Args:
      names (identifier*) [list]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Expr [value lineno col_offset]
  "Args:
      value (expr)
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Pass [lineno col_offset]
  "Args:
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Break [lineno col_offset]
  "Args:
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Continue [lineno col_offset]
  "Args:
      lineno (int)
      col_offset (int)"
  None)


;==============================================================================
; Classgroup `expr`
;==============================================================================
(defsyntax BoolOp [op values lineno col_offset]
  "Args:
      op (boolop)
      values (expr*) [list]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax BinOp [left op right lineno col_offset]
  "Args:
      left (expr)
      op (operator)
      right (expr)
      lineno (int)
      col_offset (int)"
  None)

(defsyntax UnaryOp [op operand lineno col_offset]
  "Args:
      op (unaryop)
      operand (expr)
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Lambda [args body lineno col_offset]
  "Args:
      args (arguments)
      body (expr)
      lineno (int)
      col_offset (int)"
  None)

(defsyntax IfExp [test body orelse lineno col_offset]
  "Args:
      test (expr)
      body (expr)
      orelse (expr)
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Dict [keys values lineno col_offset]
  "Args:
      keys (expr*) [list]
      values (expr*) [list]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Set [elts lineno col_offset]
  "Args:
      elts (expr*) [list]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax ListComp [elt generators lineno col_offset]
  "Args:
      elt (expr)
      generators (comprehension*) [list]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax SetComp [elt generators lineno col_offset]
  "Args:
      elt (expr)
      generators (comprehension*) [list]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax DictComp [key value generators lineno col_offset]
  "Args:
      key (expr)
      value (expr)
      generators (comprehension*) [list]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax GeneratorExp [elt generators lineno col_offset]
  "Args:
      elt (expr)
      generators (comprehension*) [list]
      lineno (int)
      col_offset (int)"
  None)

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
  None)

(defsyntax YieldFrom [value lineno col_offset]
  "Args:
      value (expr)
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Compare [left ops comparators lineno col_offset]
  "Args:
      left (expr)
      ops (cmpop*) [list]
      comparators (expr*) [list]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Call [func args keywords lineno col_offset]
  "Args:
      func (expr)
      args (expr*) [list]
      keywords (keyword*) [list]
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Num [n lineno col_offset]
  "Args:
      n (object)
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Str [s lineno col_offset]
  "Args:
      s (string)
      lineno (int)
      col_offset (int)"
  None)

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
  None)

(defsyntax Bytes [s lineno col_offset]
  "Args:
      s (bytes)
      lineno (int)
      col_offset (int)"
  None)

(defsyntax NameConstant [value lineno col_offset]
  "Args:
      value (singleton)
      lineno (int)
      col_offset (int)"
  None)

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
  None)

(defsyntax Attribute [value attr ctx lineno col_offset]
  "Args:
      value (expr)
      attr (identifier)
      ctx (expr_context)
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Subscript [value slice ctx lineno col_offset]
  "Args:
      value (expr)
      slice (slice)
      ctx (expr_context)
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Starred [value ctx lineno col_offset]
  "Args:
      value (expr)
      ctx (expr_context)
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Name [id ctx lineno col_offset]
  "Args:
      id (identifier)
      ctx (expr_context)
      lineno (int)
      col_offset (int)"
  None)

(defsyntax List [elts ctx lineno col_offset]
  "Args:
      elts (expr*) [list]
      ctx (expr_context)
      lineno (int)
      col_offset (int)"
  None)

(defsyntax Tuple [elts ctx lineno col_offset]
  "Args:
      elts (expr*) [list]
      ctx (expr_context)
      lineno (int)
      col_offset (int)"
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
(defsyntax Slice [lower upper step]
  "Args:
      lower (expr?) [optional]
      upper (expr?) [optional]
      step (expr?) [optional]"
  None)

(defsyntax ExtSlice [dims]
  "Args:
      dims (slice*) [list]"
  None)

(defsyntax Index [value]
  "Args:
      value (expr)"
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
(defsyntax comprehension [target iter ifs is_async]
  "Args:
      target (expr)
      iter (expr)
      ifs (expr*) [list]
      is_async (int)"
  None)


;==============================================================================
; Classgroup `excepthandler`
;==============================================================================
(defsyntax ExceptHandler [type name body lineno col_offset]
  "Args:
      type (expr?) [optional]
      name (identifier?) [optional]
      body (stmt*) [list]
      lineno (int)
      col_offset (int)"
  None)


;==============================================================================
; Datatype `arguments`
;==============================================================================
(defsyntax arguments [args vararg kwonlyargs kw_defaults kwarg defaults]
  "Args:
      args (arg*) [list]
      vararg (arg?) [optional]
      kwonlyargs (arg*) [list]
      kw_defaults (expr*) [list]
      kwarg (arg?) [optional]
      defaults (expr*) [list]"
  None)


;==============================================================================
; Datatype `arg`
;==============================================================================
(defsyntax arg [arg annotation lineno col_offset]
  "Args:
      arg (identifier)
      annotation (expr?) [optional]
      lineno (int)
      col_offset (int)"
  None)


;==============================================================================
; Datatype `keyword`
;==============================================================================
(defsyntax keyword [arg value]
  "Args:
      arg (identifier?) [optional]
      value (expr)"
  None)


;==============================================================================
; Datatype `alias`
;==============================================================================
(defsyntax alias [name asname]
  "Args:
      name (identifier)
      asname (identifier?) [optional]"
  None)


;==============================================================================
; Datatype `withitem`
;==============================================================================
(defsyntax withitem [context_expr optional_vars]
  "Args:
      context_expr (expr)
      optional_vars (expr?) [optional]"
  None)


