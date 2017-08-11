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

(deftag todo [f]
  `#@((pytest.mark.skip :reason "TODO")
     ~f))

#todo
(defn test_Module []
  )

#todo
(defn test_Interactive []
  )

#todo
(defn test_Expression []
  )

#todo
(defn test_Suite []
  )

#todo
(defn test_FunctionDef []
  )

#todo
(defn test_AsyncFunctionDef []
  )

#todo
(defn test_ClassDef []
  )

#todo
(defn test_Return []
  )

#todo
(defn test_Delete []
  )

#todo
(defn test_Assign []
  )

#todo
(defn test_AugAssign []
  )

#todo
(defn test_AnnAssign []
  )

#todo
(defn test_For []
  )

#todo
(defn test_AsyncFor []
  )

#todo
(defn test_While []
  )

#todo
(defn test_If []
  )

#todo
(defn test_With []
  )

#todo
(defn test_AsyncWith []
  )

#todo
(defn test_Raise []
  )

#todo
(defn test_Try []
  )

#todo
(defn test_Assert []
  )

#todo
(defn test_Import []
  )

#todo
(defn test_ImportFrom []
  )

#todo
(defn test_Global []
  )

#todo
(defn test_Nonlocal []
  )

#todo
(defn test_Expr []
  )

#todo
(defn test_Pass []
  )

#todo
(defn test_Break []
  )

#todo
(defn test_Continue []
  )

#todo
(defn test_BoolOp []
  )

#todo
(defn test_BinOp []
  )

#todo
(defn test_UnaryOp []
  )

#todo
(defn test_Lambda []
  )

#todo
(defn test_IfExp []
  )

#todo
(defn test_Dict []
  )

#todo
(defn test_Set []
  )

#todo
(defn test_ListComp []
  )

#todo
(defn test_SetComp []
  )

#todo
(defn test_DictComp []
  )

#todo
(defn test_GeneratorExp []
  )

#todo
(defn test_Await []
  )

#todo
(defn test_Yield []
  )

#todo
(defn test_YieldFrom []
  )

#todo
(defn test_Compare []
  )

#todo
(defn test_Call []
  )

#todo
(defn test_Num []
  )

#todo
(defn test_Str []
  )

#todo
(defn test_FormattedValue []
  )

#todo
(defn test_JoinedStr []
  )

#todo
(defn test_Bytes []
  )

#todo
(defn test_NameConstant []
  )

#todo
(defn test_Ellipsis []
  )

#todo
(defn test_Constant []
  )

#todo
(defn test_Attribute []
  )

#todo
(defn test_Subscript []
  )

#todo
(defn test_Starred []
  )

#todo
(defn test_Name []
  )

#todo
(defn test_List []
  )

#todo
(defn test_Tuple []
  )

#todo
(defn test_Load []
  )

#todo
(defn test_Store []
  )

#todo
(defn test_Del []
  )

#todo
(defn test_AugLoad []
  )

#todo
(defn test_AugStore []
  )

#todo
(defn test_Param []
  )

#todo
(defn test_Slice []
  )

#todo
(defn test_ExtSlice []
  )

#todo
(defn test_Index []
  )

#todo
(defn test_comprehension []
  )

#todo
(defn test_ExceptHandler []
  )

#todo
(defn test_arguments []
  )

#todo
(defn test_arg []
  )

#todo
(defn test_keyword []
  )

#todo
(defn test_alias []
  )

#todo
(defn test_withitem []
  )

