#coding: utf-8
from itertools import combinations
from collections import Counter
from operator import *
import random

random.seed(0)
numlist = sorted([random.randint(1,100) for i in range(100)])
targetNum = 10
searchMode = 0
# 0: Randomized search (fast for long number lists, doesn't stop if no solutions exist)
# 1: Ordered search (good for short number lists, stops as soon as one solution is found)
# 2: Exhaustive search

#======================================================================
oplist = [sub, add, truediv, mul] if searchMode == 0 else [add, sub, truediv, mul] # Order is important
ophash = {add:"+", sub:"-", mul:"*", truediv:"/"}
commutativeops = [add, mul]
#======================================================================
def allEquationsFrom(numComb):
  if len(numComb) == 1:
    yield (str(numComb[0]), numComb[0])
  else:
    for op in oplist:
      for i in shuffled(range(1,len(numComb))) if (op not in commutativeops) else shuffled(range(1,2+int((len(numComb)-1)/2))):
        for leftComb in combinations(Counter(numComb), i):
          for leftEq in allEquationsFrom(leftComb):
            for rightEq in allEquationsFrom(listSubtraction(numComb,leftComb)):
              yield eqValPair(op, leftEq, rightEq)

def eqValPair(op, leftPair, rightPair):
  try: val = op(leftPair[1],rightPair[1])
  except: val = "NaN"
  return ("(" + leftPair[0] + ophash[op] + rightPair[0] + ")", val)

def shuffled(orig):
    dest = list(orig)
    random.shuffle(dest)
    return dest

def listSubtraction(U,S):
  return list((Counter(U) - Counter(S)).elements())

#======================================================================
def randomizedSearch(targetNum, numlist):
  while True:
    iterCounter = 0
    for x in allEquationsFrom(numlist):
      iterCounter += 1
      if iterCounter >= 100:
        break
      elif(x[1] == targetNum):
        print(str(targetNum) + " = " + x[0])
        return

def orderedSearch(targetNum, numlist):
  solutionCount = 0
  for x in allEquationsFrom(numlist):
    if(x[1] == targetNum):
      print(str(targetNum) + " = " + x[0])
      solutionCount += 1
      if searchMode == 1:
        return
  print(str(solutionCount) + " equations make " + str(targetNum) + " for these numbers (searched exhaustively)")

#======================================================================
if __name__ == '__main__':
  print(numlist)
  print("Calculating...")
  (randomizedSearch if searchMode == 0 else orderedSearch)(targetNum, numlist)