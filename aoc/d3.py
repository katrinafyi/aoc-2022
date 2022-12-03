import sys 
from .lib import *

def prio(x: str):
  y = ord(x.lower()) - ord('a') + 1
  if x == x.upper():
    y += 26
  return y
  

def one():
  d = sys.stdin.read()
  d = d.split('\n')

  dd = d
  d = [(x[:len(x)//2], x[len(x)//2:]) for x in d]

  dupe = [fst((set(left) & set(right))) for left, right in d]
  print(dupe)
  print(sum(map(prio, dupe)))
  # print(prio('z'))
  
  badges = []
  for a,b,c in chunks(dd, 3):
    a, b, c = set(a), set(b), set(c)
    badges.append(fst((a&b&c)))
  print(sum(map(prio, badges)))
  

if __name__ == '__main__':
  one()
