import sys 
from .lib import *

def one():
  d = sys.stdin.read()
  d = d.split('\n\n')
  print(d)

  d = [ints(x) for x in d]
  print(d)
  d.sort(key=sum)
  print(sum(d[-1]))

  print(sum(map(sum, d[-3:])))

if __name__ == '__main__':
  one()
