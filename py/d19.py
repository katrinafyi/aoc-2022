from functools import lru_cache
import sys
import time 
from .lib import *

from collections import defaultdict, deque
import itertools

INF = float('inf')

def buy(ore,cla,obs, cost):
  a,b,c = cost
  if ore>=a and cla>=b and obs>=c: 
    return (ore-a, cla-b, obs-c)
  return (None,None,None)

ore_cost,cla_cost,obs_cost,geo_cost = [None]*4

TIME = 24
TIMES = range(TIME)
# each cost is a (ore,clay,obs,geo) tuple
@lru_cache(maxsize=None)
def robot(t=TIME, ore=0,cla=0,obs=0, dore=1, dcla=0, dobs=0):
  # TIME REMAINING, ore,cla,obs, dore,dcla,dobs -> total geo
  matrix = [[[[[[None for i in TIMES] for i in TIMES] for i in TIMES] for i in TIMES] for i in TIMES] for i in TIMES]
  matrix2 = [[[[[[None for i in TIMES] for i in TIMES] for i in TIMES] for i in TIMES] for i in TIMES] for i in TIMES]
  
  # t = 0 base 
  for a,b,c,d,e,f in itertools.product(TIMES, repeat=6):
    matrix[a][b][c][d][e][f] = 0
  # if t == 0: return 0
  
  for t in TIMES[1:]:
  # if 1:
    print(t)
    if t % 2 == 1:
      old, new = matrix, matrix2
    else:
      old, new = matrix2, matrix
    for ore,cla,obs, dore,dcla,dobs in itertools.product(TIMES, repeat=6):
    # if 1:
      # print(ore,cla,obs,dore,dcla,dobs)
      if max(ore+dore, cla+dcla, obs+dobs, dore+1, dcla+1, dobs+1) not in TIMES:
        continue # impossible to hhave more resources than time.
      robot = lambda t,ore,cla,obs,dore,dcla,dobs: old[ore][cla][obs][dore][dcla][dobs] if old[ore][cla][obs][dore][dcla][dobs] is not None else -INF
      options = [robot(t-1,ore+dore,cla+dcla,obs+dobs,dore,dcla,dobs)]
      # options = [robot(t-1,ore+dore,cla+dcla,obs+dobs,dore,dcla,dobs)]


      ore2,cla2,obs2 = buy(ore,cla,obs, ore_cost)
      if ore2 is not None: options.append(robot(t-1,ore2+dore,cla2+dcla,obs2+dobs,dore+1,dcla,dobs))
      ore2,cla2,obs2 = buy(ore,cla,obs, cla_cost)
      if ore2 is not None: options.append(robot(t-1,ore2+dore,cla2+dcla,obs2+dobs,dore,dcla+1,dobs))
      ore2,cla2,obs2 = buy(ore,cla,obs, obs_cost)
      if ore2 is not None: options.append(robot(t-1,ore2+dore,cla2+dcla,obs2+dobs,dore,dcla,dobs+1))

      ore2,cla2,obs2 = buy(ore,cla,obs, geo_cost)
      if ore2 is not None: 
        # print(ore2,cla2,obs2)
        options.append(robot(t-1,ore2+dore,cla2+dcla,obs2+dobs,dore,dcla,dobs) + (t-1))
      new[ore][cla][obs][dore][dcla][dobs] = max(options)
  return new[0][0][0][1][0][0]
  

def one(): 
  global ore_cost, cla_cost, obs_cost, geo_cost

  data = sys.stdin.read() 
  data = data.split('\n')

  import time

  if len(sys.argv) -1 == 2:
    a,b = list(map(int, sys.argv[1:]))

  for i,l in enumerate(data): 
    i += 1
    if not (a <= i < b): continue
    print(l)

    l = l.strip()
    if not l: continue 
    ore, clay, obs, geo = l.split('. ')
    ore_cost = (ints(ore)[1],0,0)
    cla_cost = (ints(clay)[0],0,0)
    obs_cost = tuple(ints(obs)) + (0,)
    geo_cost = (0,) + tuple(ints(geo))
    t0 = time.time()
    x = (robot())
    print(x, x * i)
    print(robot.cache_info())
    print('in', time.time() - t0)
    robot.cache_clear()

if __name__ == '__main__':
  one()
