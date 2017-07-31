(import [ast])
(setv codestring (ast.parse "
import hy, cv2
import numpy as np
(a + c) + b"))
