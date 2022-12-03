import re

def ints(l):
  r = re.compile(r'-?\d+')
  return [int(x) for x in r.findall(l)]

def uints(l):
  r = re.compile(r'\d+')
  return [int(x) for x in r.findall(l)]

def converse(d):
  return {v:k for k,v in d.items()}

from itertools import zip_longest

def chunks(iterable, n, *, incomplete='strict', fillvalue=None):
  "Collect data into non-overlapping fixed-length chunks or blocks"
  # grouper('ABCDEFG', 3, fillvalue='x') --> ABC DEF Gxx
  # grouper('ABCDEFG', 3, incomplete='strict') --> ABC DEF ValueError
  # grouper('ABCDEFG', 3, incomplete='ignore') --> ABC DEF
  args = [iter(iterable)] * n
  if incomplete == 'fill':
    return zip_longest(*args, fillvalue=fillvalue)
  if incomplete == 'strict':
    return zip(*args, strict=True)
  if incomplete == 'ignore':
    return zip(*args)
  else:
    raise ValueError('Expected fill, strict, or ignore')

def fst(x):
  return next(iter(x))