(import [itertools [combinations]])
(defclass Py2HyReturnException [Exception]
  (defn __init__ [self retvalue]
    (setv self.retvalue retvalue)))
(import [collections [Counter]])
(import [operator [*]])
(import [random])
(random.seed 0)
(setv numlist (sorted (list_comp (random.randint 1 100) [i (range 100)])))
(setv targetNum 10)
(setv searchMode 0)
(setv oplist (if (= searchMode 0)
                [sub add truediv mul]
                [add sub truediv mul]))
(setv ophash {add "+" sub "-" mul "*" truediv "/"})
(setv commutativeops [add mul])
(defn allEquationsFrom [numComb]
  (if (= (len numComb) 1)
    (do
      (yield (, (str (get numComb 0)) (get numComb 0))))
    (do
      (for [op oplist]
        (for [i (if (not_in op commutativeops) (shuffled (range 1 (len numComb))) (shuffled (range 1 (+ 2 (int (/ (- (len numComb) 1) 2))))))]
          (for [leftComb (combinations (Counter numComb) i)]
            (for [leftEq (allEquationsFrom leftComb)]
              (for [rightEq (allEquationsFrom (listSubtraction numComb leftComb))]
                (yield (eqValPair op leftEq rightEq))))))))))
(defn eqValPair [op leftPair rightPair]
  (try
    (do
      (try
        (setv val (op (get leftPair 1) (get rightPair 1)))
        (except [e Py2HyReturnException]
          (raise e))
        (except []
          (setv val "NaN")))
      (raise (Py2HyReturnException (, (+ (+ (+ (+ "(" (get leftPair 0)) (get ophash op)) (get rightPair 0)) ")") val))))
    (except [e Py2HyReturnException]
      e.retvalue)))
(defn shuffled [orig]
  (setv dest (list orig))
  (random.shuffle dest)
  dest)
(defn listSubtraction [U S]
  (list ((. (- (Counter U) (Counter S)) elements))))
(defn randomizedSearch [targetNum numlist]
  (try
    (while True
      (setv iterCounter 0)
      (for [x (allEquationsFrom numlist)]
        (+= iterCounter 1)
        (cond
          [(>= iterCounter 100)
           (break)]
          [(= (get x 1) targetNum)
           (do
             (print (+ (+ (str targetNum) " = ") (get x 0)))
             (raise (Py2HyReturnException None)))]
          [True
           (do)])))
    (except [e Py2HyReturnException]
      e.retvalue)))
(defn orderedSearch [targetNum numlist]
  (try
    (do
      (setv solutionCount 0)
      (for [x (allEquationsFrom numlist)]
        (when (= (get x 1) targetNum)
          (print (+ (+ (str targetNum) " = ") (get x 0)))
          (+= solutionCount 1)
          (when (= searchMode 1)
            (raise (Py2HyReturnException None)))))
      (print (+ (+ (+ (str solutionCount) " equations make ") (str targetNum)) " for these numbers (searched exhaustively)")))
    (except [e Py2HyReturnException]
      e.retvalue)))
(when (= __name__ "__main__")
  (print numlist)
  (print "Calculating...")
  ((if (= searchMode 0)
     randomizedSearch
     orderedSearch) targetNum numlist))
