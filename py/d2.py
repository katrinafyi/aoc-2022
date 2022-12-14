
import sys 
from .lib import *

def one():
  d = sys.stdin.read()
  print(d)
  d = [x.split() for x in d.split('\n')]
  print(d)

  a = 0
  beats = {
    # A beats Z
    'A': 'Z',
    'B': 'X',
    'C': 'Y',
    'X': 'C',
    'Y': 'A',
    'Z': 'B'
  }

  # Z is beaten by A
  beaten = {v:k for k, v in beats.items()}
  
  aa = dict(X=1, Y=2, Z=3, A=1, B=2, C=3)

  for x in d:
    if len(x) != 2: continue 
    they, you = x
    
    if you in beats[they]:
      x, y = 'lose', 0
    elif they in beats[you]:
      x, y= 'win', 6
    else:
      x, y = 'draw', 3
    a += aa[you] + y

  print(a)

  a=0
  for x in d:
    if len(x) != 2: continue 
    they, goal = x
    
    if goal == 'X': # lose 
      you , y= beats[they], 0
    elif goal == 'Z': # win
      you, y = beaten[they], 6
    else:
      you, y = they, 3

    a += aa[you] + y

    
  print(a)

  

if __name__ == '__main__':
  one()
