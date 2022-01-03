from bisect import bisect_left
from dataclasses import dataclass
import fileinput
import platform
import re
from tqdm import tqdm

if platform.python_implementation() != 'PyPy':
    print('Run with PyPy to save a few minutes')

@dataclass
class Step:
    value: bool
    x0: int
    x1: int
    y0: int
    y1: int
    z0: int
    z1: int

steps = []
xs, ys, zs = [], [], []

with fileinput.input() as f:
    for line in f:
        value = line.startswith('on')
        x0, x1, y0, y1, z0, z1 = map(int, re.findall('-?\d+', line))
        x1 += 1  # Make ranges [x0, x1)
        y1 += 1
        z1 += 1
        steps.append(Step(value, x0, x1, y0, y1, z0, z1))
        xs.extend([x0, x1])
        ys.extend([y0, y1])
        zs.extend([z0, z1])

xs.sort()
ys.sort()
zs.sort()
N = len(xs)
grid = [[[False for x in range(N)] for y in range(N)] for z in range(N)]

for s in tqdm(steps, desc='Run reboot steps'):
    # Compress coordinates (only necessary for part 2)
    x0 = bisect_left(xs, s.x0)
    x1 = bisect_left(xs, s.x1)
    y0 = bisect_left(ys, s.y0)
    y1 = bisect_left(ys, s.y1)
    z0 = bisect_left(zs, s.z0)
    z1 = bisect_left(zs, s.z1)

    for x in range(x0, x1):
        for y in range(y0, y1):
            for z in range(z0, z1):
                grid[x][y][z] = s.value

ans1 = 0
ans2 = 0

for x, y, z in tqdm(
    ((x, y, z) for x in range(N - 1) for y in range(N - 1) for z in range(N - 1)),
    total=(N - 1)**3,
    desc='Count cubes that are on'
):
    if grid[x][y][z]:
        x0 = xs[x]
        x1 = xs[x + 1]
        y0 = ys[y]
        y1 = ys[y + 1]
        z0 = zs[z]
        z1 = zs[z + 1]
        vol = (x1 - x0) * (y1 - y0) * (z1 - z0)
        ans2 += vol

        # Count the number of cubes in the initialization procedure area
        # that are turned on (for part 1)
        if all(-50 < a <= 50 for a in [x0, x1, y0, y1, z0, z1]):
            ans1 += vol

print(ans1)
print(ans2)
