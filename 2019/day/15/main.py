from intcode import Intcode
from collections import deque
import numpy as np

with open("input.txt") as fp:
    prog = Intcode([int(x) for ln in fp for x in ln.split(",")])

# Movement commands
DIR = {
    1: np.array([0, -1]),
    2: np.array([0, 1]),
    3: np.array([-1, 0]),
    4: np.array([1, 0]),
}

# Reverse commands
REV = {1: 2, 2: 1, 3: 4, 4: 3}

state = {}


def explore(pos):
    global prog, state
    x, y = pos

    if prog.halt:
        return

    for cmd, d in DIR.items():
        x1, y1 = new_pos = pos + d

        if (x1, y1) in state:
            continue

        prog.inputs.append(cmd)
        prog.run()
        status = prog.outputs.pop()
        state[(x1, y1)] = status

        if status == 0:
            pass
        elif status == 1:
            explore(new_pos)
            prog.inputs.append(REV[cmd])
            prog.run()
        elif status == 2:
            # There might be a faster way from Z to O which
            # is not found through this DFS, so keep exploring!
            explore(new_pos)
            prog.inputs.append(REV[cmd])
            prog.run()


# Explore the space using DFS
explore(np.array([0, 0]))

Z = 0, 0  # Initial pos
O = None  # Oxygen tank pos
mincol, minrow, maxcol, maxrow = 0, 0, 0, 0

for (x, y), z in state.items():
    mincol = min(mincol, x)
    maxcol = max(maxcol, x)
    minrow = min(minrow, x)
    maxrow = max(maxrow, x)

    if z == 2:
        O = (x, y)

tilemap = {
    -1: " ",  # unexplored
    0: "#",  # wall
    1: ".",  # not wall
    2: "O",  # oxygen tank
}

# Print state
# print('\n'.join(''.join(tilemap[state.get((x, y), -1)] for x in range(mincol, maxcol + 1))
#                 for y in range(minrow, maxrow + 1)))

# BFS from starting position
q = deque([])
q.append((Z, 0))
sp = {}

while q:
    pos, path_len = q.popleft()

    if tuple(pos) in sp:
        continue

    sp[tuple(pos)] = path_len

    for n in [pos + d for d in DIR.values() if state.get(tuple(pos + d)) > 0]:
        q.append((n, path_len + 1))

print(sp[O])

# Part 2
# Run BFS from oxygen tank
q = deque([])
q.append((O, 0))
sp = {}

while q:
    pos, path_len = q.popleft()

    if tuple(pos) in sp:
        continue

    sp[tuple(pos)] = path_len

    for n in [pos + d for d in DIR.values() if state.get(tuple(pos + d)) > 0]:
        q.append((n, path_len + 1))

print(max(n for pos, n in sp.items()))
