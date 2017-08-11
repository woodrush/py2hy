(import [hy]
        [ast]
        [pytest]
        [py2hy.py2hy [py2hy :as py2hy_]]
        [builtins])

;; Remove the `do` at the top for simplicity
(defn py2hy [py]
  (setv x (->> py (ast.parse) (py2hy_) (drop 1) (list)))
  (if (= 1 (len x))
    (first x)
    x))

(defn assert-eqexpr-eqvalue
  [pysource hyexpr]
  (defn helper [x y]
    (assert (= x y)))
  ;; Readability for pytest failures
  (helper (py2hy pysource) hyexpr)
  (assert (= (builtins.eval pysource) (eval hyexpr))))

(defmacro defoptest [testname oplist testbody]
  `(defmacro ~(hy.models.HySymbol testname) []
     (setv oplist ~oplist)
     `(do
        ~@(map (fn [x]
                 ((fn [astname hyop pyop]
                    `(defn ~(hy.models.HySymbol (+ "test_" astname)) []
                       ~~testbody))
                  #* x))
               oplist))))

(defoptest test_binaryops
  [["Add" `+ "+"]
   ["Sub" `- "-"]
   ["Mult" `* "*"]
   ["Div" `/ "/"]
   ["Mod" `% "%"]
   ["LShift" `<< "<<"]
   ["RShift" `>> ">>"]
   ["BitOr" `| "|"]
   ["BitXor" `^ "^"]
   ["BitAnd" `& "&"]
   ["FloorDiv" `// "//"]]
  `(do
     (assert-eqexpr-eqvalue
       (+ "5" ~pyop "4" ~pyop "3")
       `(~'~hyop (~'~hyop 5 4) 3))
     (assert-eqexpr-eqvalue
       (+ "5" ~pyop "(4" ~pyop "3)")
       `(~'~hyop 5 (~'~hyop 4 3)))
     (assert-eqexpr-eqvalue
       (+ "(5" ~pyop "4)" ~pyop "3")
       `(~'~hyop (~'~hyop 5 4) 3))))

(defoptest test_pow
  [["Pow" `** "**"]]
  `(do
     ;; `**` is right associative
     (assert-eqexpr-eqvalue
       (+ "5" ~pyop "4" ~pyop "3")
       `(~'~hyop 5 (~'~hyop 4 3)))
     (assert-eqexpr-eqvalue
       (+ "5" ~pyop "(4" ~pyop "3)")
       `(~'~hyop 5 (~'~hyop 4 3)))
     (assert-eqexpr-eqvalue
       (+ "(5" ~pyop "4)" ~pyop "3")
       `(~'~hyop (~'~hyop 5 4) 3))))

(defoptest test_matmult
  [["MatMult" `@ "@"]]
  `(do
     (assert (= (py2hy (+ "5" ~pyop "4" ~pyop "3"))
                `(~'~hyop (~'~hyop 5 4) 3)))
     (assert (= (py2hy (+ "5" ~pyop "(4" ~pyop "3)"))
                `(~'~hyop 5 (~'~hyop 4 3))))
     (assert (= (py2hy (+ "(5" ~pyop "4)" ~pyop "3"))
                `(~'~hyop (~'~hyop 5 4) 3)))))

(defoptest test_boolops
  [["And" `and "and"]
   ["Or" `or "or"]]
  `(do
     (assert-eqexpr-eqvalue
       ~(.join " " ["True" pyop "False" pyop "True"])
       `(~'~hyop True False True))
     (assert-eqexpr-eqvalue
       ~(.join " " ["True" pyop "(False" pyop "True)"])
       `(~'~hyop True (~'~hyop False True)))
     (assert-eqexpr-eqvalue
       ~(.join " " ["(True" pyop "False" pyop "True)"])
       `(~'~hyop True False True))))

(defoptest test_unaryops
  [["Invert" `~ "~"]
   ["Not" `not "not"]
   ["UAdd" `+ "+"]
   ["USub" `- "-"]]
  `(do
     (assert-eqexpr-eqvalue
       ~(.join " " [pyop "True"])
       `(~'~hyop True))))

(defoptest test_compops
  [["Eq" `= "=="]
   ["NotEq" `!= "!="]
   ["Lt" `< "<"]
   ["LtE" `<= "<="]
   ["Gt" `> ">"]
   ["GtE" `>= ">="]
   ["Is" `is "is"]
   ["IsNot" `is-not "is not"]]
  `(do
     (assert-eqexpr-eqvalue
       (.join " " ["5" ~pyop "4" ~pyop "3"])
       `(and (~'~hyop 5 4) (~'~hyop 4 3)))
     (assert-eqexpr-eqvalue
       (.join " " ["5" ~pyop "(4" ~pyop "3)"])
       `(~'~hyop 5 (~'~hyop 4 3)))
     (assert-eqexpr-eqvalue
       (.join " " ["(5" ~pyop "4)" ~pyop "3"])
       `(~'~hyop (~'~hyop 5 4) 3))))

(defoptest test_compops_list
  [["In" `in "in"]
   ["NotIn" `not-in "not in"]]
  `(do
     (assert-eqexpr-eqvalue
       (.join " " ["5" ~pyop "[5, 3]"])
       `(~'~hyop 5 [5 3]))))

(defn test_compops_mixed []
  (assert-eqexpr-eqvalue
    "1<2<=3" `(and (< 1 2) (<= 2 3))))

(defn test_arithmeticops_mixed []
  (assert-eqexpr-eqvalue
    "1*2+3/4*5" `(+ (* 1 2) (* (/ 3 4) 5))))

(test_binaryops)
(test_pow)
(test_matmult)
(test_boolops)
(test_unaryops)
(test_compops)
(test_compops_list)
