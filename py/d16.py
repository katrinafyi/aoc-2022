from functools import lru_cache
import sys
import time 
from .lib import *

from collections import defaultdict, deque

INF = float('inf')


apsp = [("AA","AI",2),("AA","CJ",3),("AA","CU",8),("AA","EK",5),("AA","GU",8),("AA","IZ",7),("AA","KB",5),("AA","KF",2),("AA","KS",5),("AA","LG",4),("AA","MB",10),("AA","PB",2),("AA","QK",4),("AA","TJ",3),("AA","YE",10),("AI","AI",0),("AI","CJ",5),("AI","CU",8),("AI","EK",3),("AI","GU",8),("AI","IZ",6),("AI","KB",3),("AI","KF",4),("AI","KS",5),("AI","LG",6),("AI","MB",10),("AI","PB",2),("AI","QK",4),("AI","TJ",3),("AI","YE",10),("CJ","AI",5),("CJ","CJ",0),("CJ","CU",5),("CJ","EK",8),("CJ","GU",5),("CJ","IZ",6),("CJ","KB",5),("CJ","KF",3),("CJ","KS",2),("CJ","LG",5),("CJ","MB",7),("CJ","PB",5),("CJ","QK",3),("CJ","TJ",4),("CJ","YE",7),("CU","AI",8),("CU","CJ",5),("CU","CU",0),("CU","EK",11),("CU","GU",6),("CU","IZ",11),("CU","KB",10),("CU","KF",8),("CU","KS",3),("CU","LG",6),("CU","MB",8),("CU","PB",8),("CU","QK",8),("CU","TJ",5),("CU","YE",2),("EK","AI",3),("EK","CJ",8),("EK","CU",11),("EK","EK",0),("EK","GU",11),("EK","IZ",9),("EK","KB",6),("EK","KF",7),("EK","KS",8),("EK","LG",9),("EK","MB",13),("EK","PB",5),("EK","QK",7),("EK","TJ",6),("EK","YE",13),("GU","AI",8),("GU","CJ",5),("GU","CU",6),("GU","EK",11),("GU","GU",0),("GU","IZ",11),("GU","KB",10),("GU","KF",8),("GU","KS",3),("GU","LG",6),("GU","MB",2),("GU","PB",8),("GU","QK",8),("GU","TJ",5),("GU","YE",8),("IZ","AI",6),("IZ","CJ",6),("IZ","CU",11),("IZ","EK",9),("IZ","GU",11),("IZ","IZ",0),("IZ","KB",3),("IZ","KF",8),("IZ","KS",8),("IZ","LG",10),("IZ","MB",13),("IZ","PB",5),("IZ","QK",3),("IZ","TJ",8),("IZ","YE",13),("KB","AI",3),("KB","CJ",5),("KB","CU",10),("KB","EK",6),("KB","GU",10),("KB","IZ",3),("KB","KB",0),("KB","KF",7),("KB","KS",7),("KB","LG",9),("KB","MB",12),("KB","PB",4),("KB","QK",2),("KB","TJ",6),("KB","YE",12),("KF","AI",4),("KF","CJ",3),("KF","CU",8),("KF","EK",7),("KF","GU",8),("KF","IZ",8),("KF","KB",7),("KF","KF",0),("KF","KS",5),("KF","LG",2),("KF","MB",10),("KF","PB",3),("KF","QK",5),("KF","TJ",3),("KF","YE",10),("KS","AI",5),("KS","CJ",2),("KS","CU",3),("KS","EK",8),("KS","GU",3),("KS","IZ",8),("KS","KB",7),("KS","KF",5),("KS","KS",0),("KS","LG",3),("KS","MB",5),("KS","PB",5),("KS","QK",5),("KS","TJ",2),("KS","YE",5),("LG","AI",6),("LG","CJ",5),("LG","CU",6),("LG","EK",9),("LG","GU",6),("LG","IZ",10),("LG","KB",9),("LG","KF",2),("LG","KS",3),("LG","LG",0),("LG","MB",8),("LG","PB",5),("LG","QK",7),("LG","TJ",5),("LG","YE",8),("MB","AI",10),("MB","CJ",7),("MB","CU",8),("MB","EK",13),("MB","GU",2),("MB","IZ",13),("MB","KB",12),("MB","KF",10),("MB","KS",5),("MB","LG",8),("MB","MB",0),("MB","PB",10),("MB","QK",10),("MB","TJ",7),("MB","YE",10),("PB","AI",2),("PB","CJ",5),("PB","CU",8),("PB","EK",5),("PB","GU",8),("PB","IZ",5),("PB","KB",4),("PB","KF",3),("PB","KS",5),("PB","LG",5),("PB","MB",10),("PB","PB",0),("PB","QK",2),("PB","TJ",3),("PB","YE",10),("QK","AI",4),("QK","CJ",3),("QK","CU",8),("QK","EK",7),("QK","GU",8),("QK","IZ",3),("QK","KB",2),("QK","KF",5),("QK","KS",5),("QK","LG",7),("QK","MB",10),("QK","PB",2),("QK","QK",0),("QK","TJ",5),("QK","YE",10),("TJ","AI",3),("TJ","CJ",4),("TJ","CU",5),("TJ","EK",6),("TJ","GU",5),("TJ","IZ",8),("TJ","KB",6),("TJ","KF",3),("TJ","KS",2),("TJ","LG",5),("TJ","MB",7),("TJ","PB",3),("TJ","QK",5),("TJ","TJ",0),("TJ","YE",7),("YE","AI",10),("YE","CJ",7),("YE","CU",2),("YE","EK",13),("YE","GU",8),("YE","IZ",13),("YE","KB",12),("YE","KF",10),("YE","KS",5),("YE","LG",8),("YE","MB",10),("YE","PB",10),("YE","QK",10),("YE","TJ",7),("YE","YE",0)]

apspt = [("AA","BB",1),("AA","CC",2),("AA","DD",1),("AA","EE",2),("AA","HH",5),("AA","JJ",2),("BB","BB",0),("BB","CC",1),("BB","DD",2),("BB","EE",3),("BB","HH",6),("BB","JJ",3),("CC","BB",1),("CC","CC",0),("CC","DD",1),("CC","EE",2),("CC","HH",5),("CC","JJ",4),("DD","BB",2),("DD","CC",1),("DD","DD",0),("DD","EE",1),("DD","HH",4),("DD","JJ",3),("EE","BB",3),("EE","CC",2),("EE","DD",1),("EE","EE",0),("EE","HH",3),("EE","JJ",4),("HH","BB",6),("HH","CC",5),("HH","DD",4),("HH","EE",3),("HH","HH",0),("HH","JJ",7),("JJ","BB",3),("JJ","CC",4),("JJ","DD",3),("JJ","EE",4),("JJ","HH",7),("JJ","JJ",0)]

# apsp = apspt

nz = list(set([x for x,y,z in apsp]))
nz.sort()
nzidmap = {k: nz.index(k) for k in nz}
nzid = lambda x: nzidmap[x]
nzrange = range(len(nz) + 1)
print(nzidmap)

nzsp = {(nzid(x),nzid(y)): z for x,y,z in apsp}
# print(nzsp)

def one():
  d = sys.stdin.read()
  d = d.split('\n')
  valves = []

  for l in d:
    if not l.strip(): continue
    l = l.replace(',', '').split()
    name = l[1]
    flow = int(l[4].split('=')[-1].strip(';'))
    tunnels = tuple(l[9:])
    valves.append((name,flow,tunnels))

  nz = [(x,y,z) for x,y,z in valves if y > 0]
  z = [(x,y,z) for x,y,z in valves if y == 0]

  start = fst((x,y,z) for x,y,z in valves if x == 'AA')

  flows = {nzid(x): y for x,y,z in valves if x in nzidmap}

  allflow = sum(flows.values())
  print(allflow)
  # print(flows)


  def calcflow(enc):
    i = 0
    x = 0
    while enc:
      enc,bit = divmod(enc,2)
      if bit:
        x += flows[i]
      i += 1
    return x

  succsmask = (1<< (len(nz)+1)) - 1
  # efficiently enumerate all bitwise supersets of x
  def supset(x):
    A = (~x) & succsmask # variable bits
    B = x  # fixed bits
    sm = A
    while True:
      yield sm | B 
      sm = (sm - 1) & A 
      if sm == 0: break 
    yield x

  succsrange = range(2**(len(nz)+1))
  subsetflow = [calcflow(i) for i in succsrange]
  supsets = [tuple(supset(i)) for i in succsrange]

  print(len(subsetflow))
  print(len(supsets))

  print(allflow)

  # nodes are (time, current node id, bit-encoded successors via node id)

  TIME = 26
  print(len(range(TIME)), len(nzrange), len(nzrange), len(succsrange)/2)
  size = len(range(TIME)) * len(nzrange) * len(supsets[1])
  print('estimated size', size)

  # nodes only exist for the non-zero flow nodes
  allnodes = lambda init=1: ((t,i,open) for t in range(TIME) for i in nzrange for open in supsets[init])
  endnode = (TIME,-1,-1)
  startnode = (0,0,1)
  # print([bin(x) for x in supsets[1][-10:]])

  def succs(arg):
    t,i,open = arg
    cost = allflow - subsetflow[open]

    if i >= 0: # if not end
      yield (endnode,(TIME-t) * cost)

    original = open

    for i2 in range(len(nz)+1):
      bit = open & 1 
      open >>= 1
      # assert i2 <= len(nz)

      if i2 == i and not bit:
        yield ((t+1,i,original | (1<<i)), cost)
      elif not bit:
        dt = nzsp[i,i2]
        if t+dt < TIME:
          yield ((t+dt,i2,original), cost * dt )

  @lru_cache(maxsize=None)
  def go(startnode, elephant=False): # returns PROFIT

    TOTAL = allflow * TIME

    cost = {startnode: 0}
    # print(list(succs(startnode)))
    t0 = time.time()

    pi = None
    for ind,i in enumerate(allnodes(startnode[2])):
      
      if elephant and ind & ((1<<20) -1) == 0:
        t = round(time.time() - t0,2)
        eta = round(t * size / (ind+1)-t,2)
        print(ind, ind/size, 'in', t, 'eta', eta, '... ', end='', flush=True)

      if i not in cost: continue
      if pi is not None and pi != i:
        del cost[pi]
      pi = i 
      d = cost[i]
      
      for i2,cost2 in succs(i):
        d2 = d+cost2 # LOSS
        if i2[1] == -1 and elephant:
          # start another pathfinder with the remaining valves
          profit1 = TOTAL - d2 # PROFIT
          # elephant may go and open everything not already opened
          start = (0,0,i[2])
          profit2 = (go(start) - TIME * subsetflow[i[2]])
          d2 = TOTAL - (profit1 + profit2)
        if d2 < cost.get(i2, INF):
          cost[i2] = d2
    
    # print(cost)
    return TOTAL - cost[endnode]
    # print(allflow * TIME - cost[endnode])

  print(go(startnode))
  print() 
  print(go(startnode,True))


  # DP MATRIX
  #





if __name__ == '__main__':
  one()
