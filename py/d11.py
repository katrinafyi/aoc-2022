import sys 
from .lib import *

from collections import defaultdict

def one():
  d = sys.stdin.read()

  # list of stack
  stacks = defaultdict(lambda: [])
  monkeys = defaultdict(lambda: [None,None,[None,None]])

  ops = {
    '*': lambda x,y: x*y,
    '+': lambda x,y: x+y
  }

  for l in d.split('\n'):
    if not l: continue 
    l = l.split()
    if l[0] == 'Monkey':
      m = int(l[1].strip(':'))
    elif l[0] == 'Starting':
      stacks[m] = [int(x.strip(',')) for x in l[2:]]
    elif l[0] == 'Operation:':
      # print(l)
      o = ops[l[4]]
      if l[5] == 'old':
        l = (lambda o: lambda x: o(x,x))(o)
      else:
        n = int(l[5])
        l = (lambda o,n: (lambda x: o(x,n)))(o,n)
      monkeys[m][0] = l
    elif l[0] == 'Test:':
      d = int(l[3])
      monkeys[m][1] = d 
    elif l[1] == 'true:':
      monkeys[m][2][0] = int(l[-1])
    elif l[1] == 'false:':
      monkeys[m][2][1] = int(l[-1])

  counts = defaultdict(int)

  divv = 1 
  for _,d,_ in monkeys.values():
    divv *= d

  print(stacks)
  print(monkeys)

  def step():
    for m, y in monkeys.items():
      (op, mod, x) = y
      t,f = x
      for x in list(stacks[m]):
        counts[m] += 1
        w = op(x) % divv
        # w = w // 3 
        if w % mod == 0:
          stacks[t].append(w)
        else:
          stacks[f].append(w)
      stacks[m].clear()

  for i in range(10000): 
    # print(i)
    step()

  print(counts)
  x,y = sorted(counts.values(), reverse=True)[:2]
      
  print(x*y)  

if __name__ == '__main__':
  one()
