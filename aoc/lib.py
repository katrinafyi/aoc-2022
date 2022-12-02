import re

def ints(l):
  r = re.compile(r'-?\d+')
  return [int(x) for x in r.findall(l)]

def uints(l):
  r = re.compile(r'\d+')
  return [int(x) for x in r.findall(l)]

def converse(d):
  return {v:k for k,v in d.items()}
