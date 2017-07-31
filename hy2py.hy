(import [hy]
        [ast]
        [codestring])

(defclass py2ast [ast.NodeVisitor]
  (defn recapply [self f l]
    (if (= list (type l))
      (hy.models.HyList (list (map (fn [l] (self.recapply f l)) l)))
      (f l)))
  
  (defn generic_visit [self node]
    (hy.models.HyExpression
      (list (self.recapply (fn [x]
                             (if (issubclass (type x) ast.AST)
                               (self.visit x)
                               (if (.startswith x ":")
                                 (hy.models.HyKeyword x)
                                 (hy.models.HySymbol x))))
                           (+ [node.__class__.__name__]
                              (reduce +
                                      (list-comp [(+ ":" n) c]
                                                 [[n c] (ast.iter_fields node)]
                                                 (not (is None c)))
                                      []))))))
  
  (defn visit_alias [self node]
    (print "alias")
    (self.generic_visit node)))

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

(defsyntax Module [:body]
  "
  :body [...]
  "
  `(do
     ~@#l#k:body))
(defsyntax Import [:names]
  "
  :names [...]
  "
  `(import ~@#l#k:names))
(defsyntax alias [:name :asname]
  "
  :name .
  :asname .
  "
  (if #k:asname
    `[~#k:name :as ~#k:asname]
    `[~#k:name]))
(defsyntax Expr [:value]
  "
  :value TODO
  "
  #m#k:value)
(defsyntax BinOp [:left :op :right]
  "
  :left  TODO
  :op    TODO
  :right TODO
  "
  `(~#m#k:op ~#m#k:left ~#m#k:right))
(defsyntax Add []
  "
  "
  `+)
(defsyntax Name [:id :ctx]
  "
  "
  #k:id)

(setv codestring codestring.codestring)
(print (ast.dump codestring))
(setv grandlist (.visit (py2ast) codestring))
(print (macroexpand grandlist))
