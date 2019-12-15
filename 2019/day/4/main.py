#%%
from itertools import groupby
lo, hi = map(int, "146810-612564".split('-', 2))

#%%
# Part 1
%time sum(any([len(list(g)) >= 2 for k, g in groupby(s, lambda x: x)]) \
          for s in map(str, range(lo, hi + 1)) \
          if all(map(lambda x: x[0] <= x[1], zip(s, s[1:]))))
 
# Part 2
#%%
%time sum(any([len(list(g)) == 2 for k, g in groupby(s, lambda x: x)]) \
          for s in map(str, range(lo, hi + 1)) \
          if all(map(lambda x: x[0] <= x[1], zip(s, s[1:]))))