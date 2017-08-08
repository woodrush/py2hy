(import [sys])
(defclass Py2HyReturnException [Exception]
  (defn __init__ [self retvalue]
    (setv self.retvalue retvalue)))
(defn game []
  (setv args [])
  (setv inventory [])
  (defn setargs [l]
    (nonlocal args)
    (setv args (l.rstrip)))
  (defn getPromptObj [l]
    (, "p" l setargs))
  (defn getNextObj []
    (, "n"))
  (print "You wake up to find yourself laying down inside a dark cave.")
  (print "What will you do?")
  (yield (getPromptObj (, (, "0" "Check your bag") (, "1" "Call for help"))))
  (cond
    [(= args "0")
     (do
       (print "You found a lighter inside your bag.")
       (inventory.append "lighter"))]
    [(= args "1")
     (print "Your voice echos through the cave...")]
    [True
     (do)])
  (yield (getNextObj))
  (while True
    (print "What will you do next?")
    (yield (getPromptObj (, (, "0" "Stand up") (, "1" "Continue laying down"))))
    (if (= args "0")
      (do
        (print "You find a way leading outside of the cave.")
        (break))
      (do
        (print "You spend several minutes in the dark, but no help seems to arrive..."))))
  (print "To be continued..."))
(defn gameloop [game]
  (for [o game]
    (when (not o)
      (break))
    (cond
      [(= (get o 0) "n")
       (do
         (print "Hit enter to continue...")
         (sys.stdin.readline)
         (yield))]
      [(= (get o 0) "p")
       (do
         (setv d (get o 1))
         (for [k d]
           (print (get k 0) (get k 1)))
         (print "> " :end "" :flush True)
         (setv choice (sys.stdin.readline))
         ((get o 2) choice)
         (yield))]
      [True
       (do)])))
(defn trynext [g]
  (try
    (try
      (next g)
      (except [e Py2HyReturnException]
        (raise e))
      (except [e Exception]
        (do)))
    (except [e Py2HyReturnException]
      e.retvalue)))
(when (= __name__ "__main__")
  (for [i (gameloop (game))]
    (do)))
