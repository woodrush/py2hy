(import [hy]
        [ast]
        [pytest]
        [py2hy.py2hy [py2hy :as py2hy_]])

(defn py2hy [py]
  (setv x (->> py (ast.parse) (py2hy_) (drop 1) (list)))
  (if (= 1 (len x))
    (first x)
    x))

(defmacro asserteq [x y]
  `(assert (= ~x ~y)))

(defsharp t [f]
  `#@((pytest.mark.skip :reason "TODO")
     ~f))

#t
(defn test_Module []
  )

#t
(defn test_Interactive []
  )

#t
(defn test_Expression []
  )

#t
(defn test_Suite []
  )

#t
(defn test_FunctionDef []
  )

#t
(defn test_AsyncFunctionDef []
  )

#t
(defn test_ClassDef []
  )

#t
(defn test_Return []
  )

#t
(defn test_Delete []
  )

#t
(defn test_Assign []
  )

#t
(defn test_AugAssign []
  )

#t
(defn test_AnnAssign []
  )

#t
(defn test_For []
  )

#t
(defn test_AsyncFor []
  )

#t
(defn test_While []
  )

#t
(defn test_If []
  )

#t
(defn test_With []
  )

#t
(defn test_AsyncWith []
  )

#t
(defn test_Raise []
  )

#t
(defn test_Try []
  )

#t
(defn test_Assert []
  )

#t
(defn test_Import []
  )

#t
(defn test_ImportFrom []
  )

#t
(defn test_Global []
  )

#t
(defn test_Nonlocal []
  )

#t
(defn test_Expr []
  )

#t
(defn test_Pass []
  )

#t
(defn test_Break []
  )

#t
(defn test_Continue []
  )

#t
(defn test_BoolOp []
  )

#t
(defn test_BinOp []
  )

#t
(defn test_UnaryOp []
  )

#t
(defn test_Lambda []
  )

#t
(defn test_IfExp []
  )

#t
(defn test_Dict []
  )

#t
(defn test_Set []
  )

#t
(defn test_ListComp []
  )

#t
(defn test_SetComp []
  )

#t
(defn test_DictComp []
  )

#t
(defn test_GeneratorExp []
  )

#t
(defn test_Await []
  )

#t
(defn test_Yield []
  )

#t
(defn test_YieldFrom []
  )

#t
(defn test_Compare []
  )

#t
(defn test_Call []
  )

#t
(defn test_Num []
  )

#t
(defn test_Str []
  )

#t
(defn test_FormattedValue []
  )

#t
(defn test_JoinedStr []
  )

#t
(defn test_Bytes []
  )

#t
(defn test_NameConstant []
  )

#t
(defn test_Ellipsis []
  )

#t
(defn test_Constant []
  )

#t
(defn test_Attribute []
  )

#t
(defn test_Subscript []
  )

#t
(defn test_Starred []
  )

#t
(defn test_Name []
  )

#t
(defn test_List []
  )

#t
(defn test_Tuple []
  )

#t
(defn test_Load []
  )

#t
(defn test_Store []
  )

#t
(defn test_Del []
  )

#t
(defn test_AugLoad []
  )

#t
(defn test_AugStore []
  )

#t
(defn test_Param []
  )

#t
(defn test_Slice []
  )

#t
(defn test_ExtSlice []
  )

#t
(defn test_Index []
  )

#t
(defn test_comprehension []
  )

#t
(defn test_ExceptHandler []
  )

#t
(defn test_arguments []
  )

#t
(defn test_arg []
  )

#t
(defn test_keyword []
  )

#t
(defn test_alias []
  )

#t
(defn test_withitem []
  )

