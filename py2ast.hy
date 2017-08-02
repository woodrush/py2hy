(import [hy]
        [ast])

(defclass Py2ast [ast.NodeVisitor]
  (defn recapply [self f l]
    (if (= list (type l))
      (hy.models.HyList (list (map (fn [l] (self.recapply f l)) l)))
      (f l)))

  (defn generic_visit [self node]
    (hy.models.HyExpression
      (list (self.recapply (fn [x]
                             (if (issubclass (type x) ast.AST)
                               (self.visit x)
                               (cond
                                 [(= int (type x)) (hy.models.HyInteger x)]
                                 [(= bool (type x)) (hy.models.HySymbol (hy.models.HySymbol (str x)))]
                                 [(.startswith x ":") (hy.models.HyKeyword x)]
                                 [True (hy.models.HySymbol x)])))
                           (+ [node.__class__.__name__]
                              (reduce +
                                      (list-comp [(+ ":" n) c]
                                                 [[n c] (ast.iter_fields node)]
                                                 (not (is None c)))
                                      [])))))))
