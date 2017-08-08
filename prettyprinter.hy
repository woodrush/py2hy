(import [hy]
        [re])

(defclass Py2HyNewline [object]
  (setv indent-stack [-1])
  (setv indent-head -1)
  
  (with-decorator classmethod
    (defn printer [self]
      (setv self.indent-head (last self.indent-stack))
      (+ "\n" (* " " self.indent-head))))
  
  (with-decorator classmethod
    (defn push-indent-stack [self]
      (self.indent-stack.append self.indent-head)))
  
  (with-decorator classmethod
    (defn pop-indent-stack [self]
      (self.indent-stack.pop)
      (setv self.indent-head (last self.indent-stack))))
  
  (with-decorator classmethod
    (defn add-indent-head [self level]
      (+= self.indent-head level))))

(defn newliner [iter]
  (drop-last 1 (interleave iter (repeat (Py2HyNewline)))))

(defn format-newline [l]
  (cond
    [(= hy.models.HyExpression (type l))
     (do
       (setv f (first l))
       (cond
         [(= f 'defclass) `(defclass ~(nth l 1)
                             ~@(newliner (map format-newline (drop 2 l))))]
         [(= f 'defn)     `(defn ~(nth l 1)
                             ~@(newliner (map format-newline (drop 2 l))))]
         [(= f 'except) `(except ~@(newliner (map format-newline (drop 1 l))))]
         [(= f 'while)  `(while  ~@(newliner (map format-newline (drop 1 l))))]
         [(= f 'when)   `(when   ~@(newliner (map format-newline (drop 1 l))))]
         [(= f 'for)    `(for    ~@(newliner (map format-newline (drop 1 l))))]
         [(= f 'if)     `(if     ~@(newliner (map format-newline (drop 1 l))))]
         [(= f 'with-decorator)  `(~@(newliner (map format-newline l)))]
         [(= f 'try)             `(~@(newliner (map format-newline l)))]
         [(= f 'do)              `(~@(newliner (map format-newline l)))]
         [(= f 'cond)
          `(cond ~(Py2HyNewline)
                 ~@(newliner (map (fn [x] `[~@(newliner (map format-newline x))])
                                  (drop 1 l))))]
         [True `(~@(map format-newline l))]))]
    [True
     l]))

(defn recursiveprint [l]
  (cond
    [(= hy.models.HyExpression (type l))
     (do
       (if (= hy.models.HyExpression (type (first l)))
         (Py2HyNewline.add-indent-head 1)
         (Py2HyNewline.add-indent-head 2))
       (Py2HyNewline.push-indent-stack)
       (if (= 0 (len l))
         (do
           (setv ret "()"))
         (do
           (setv ret (+ "("
                        (recursiveprint (first l))
                        (reduce +
                                (map (fn [x]
                                       (+
                                         (if (= Py2HyNewline (type x))
                                           "" " ")
                                         (recursiveprint x)))
                                     (drop 1 l))
                                "")
                        ")"))))
       (Py2HyNewline.pop-indent-stack)
       ret)]
    [(= hy.models.HyList (type l))
     (do
       (Py2HyNewline.add-indent-head 1)
       (Py2HyNewline.push-indent-stack)
       (if (= 0 (len l))
         (do
           (setv ret "[]"))
         (do
           (setv ret (+ "["
                        (recursiveprint (first l))
                        (reduce +
                                (map (fn [x]
                                       (+
                                         (if (= Py2HyNewline (type x))
                                           "" " ")
                                         (recursiveprint x)))
                                     (drop 1 l))
                                "")
                        "]"))))
       ; (setv ret (+ "[" (.join " " (map recursiveprint l)) "]"))
       (Py2HyNewline.pop-indent-stack)
       ret)]
    [(= hy.models.HySymbol (type l))
     (do
       (setv r (l.__repr__))
       (Py2HyNewline.add-indent-head (+ 1 (len r)))
       r)]
    [(= Py2HyNewline (type l))
     (Py2HyNewline.printer)]
    [True
     (l.__repr__)]))

(defn prettyprint [expr]
  ; Modify `__repr__` to suppress `'` and for escaping
  (setv hy.models.HySymbol.__repr__
        (fn [self] (+ "" self))
        hy.models.HyInteger.__repr__
        (fn [self] (+ "" (str self)))
        hy.models.HyFloat.__repr__
        (fn [self] (+ "" (str self)))
        hy.models.HyComplex.__repr__
        (fn [self] (+ "" (str self)))
        hy.models.HyKeyword.__repr__
        (fn [self] (.join "" (drop 1 self)))
        hy.models.HyString.__repr__
        (fn [self] (+ "\"" (->> self
                                (re.sub "\\\\" (+ "\\\\" "\\\\"))
                                (re.sub "\"" "\\\"")) "\""))
        hy.models.HyBytes.__repr__
        (fn [self]  (.__repr__ `[~@(list-comp (int x) [x self])])))
  (->> expr (format-newline) (take-nth 2) (drop 1) (map recursiveprint) (.join "\n")))