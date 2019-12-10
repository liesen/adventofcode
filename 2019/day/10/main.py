from itertools import chain, groupby, zip_longest
import numpy as np
from heapq import nlargest

with open('input.txt') as fp:
    asteroids = [np.array([x, y]) for y, ln in enumerate(fp) for x, ch in enumerate(ln) if ch == '#']

def v(a, b):
    q = b - a
    return q / np.gcd(*q)

# Part 1
X, N = nlargest(1, [(a, len(set([tuple(v(a, b)) for b in asteroids if not np.array_equal(a, b)]))) for a in asteroids], key=lambda x: x[1]).pop()
print(N)

# Part 2
# sort by angle w.r.t. the y axis, then by distance to X
bsteroids = sorted([a for a in asteroids if not np.array_equal(a, X)],
                   key=lambda a: (-np.arctan2(*(a - X)), np.linalg.norm(a - X)))
 
# group by normalized vector (ray from X), then transpose the list of list to get
# the first (closest) asteroid for each ray
x, y = list(chain.from_iterable(zip_longest(*[list(g) for k, g in groupby(bsteroids, key=lambda a: tuple(v(X, a)))])))[200 - 1]
print(x * 100 + y)