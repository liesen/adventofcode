from itertools import product, repeat


with open("input.txt") as f:
    N = 50
    lines = [ln.rstrip() for ln in f]
    path = lines[-1]

# Translate to a "regional" coordinate system: (region, y, x), where
# 1 <= region <= 6, 0 <= y, x < 50.
#
# Regions of the cube:
#
#   .12
#   .3.
#   54.
#   6..

# Offset of each side (in units of N) into original string
REGION_OFFSETS = {
    1: (0, 1),
    2: (0, 2),
    3: (1, 1),
    4: (2, 1),
    5: (2, 0),
    6: (3, 0),
}

grid = {
    (r, y - dy * N, x - dx * N): lines[y][x]
    for r, (dy, dx) in REGION_OFFSETS.items()
    for y, x in product(
        range(dy * N, (dy + 1) * N),
        range(dx * N, (dx + 1) * N),
    )
}

assert set(grid.values()) == {'.', '#'}
assert len(grid) == 6 * N * N

# Facings
R, D, L, U = range(4)

def left(dir):
    return (dir - 1) % 4

def right(dir):
    return (dir + 1) % 4

assert left(U) == L
assert left(L) == D
assert left(D) == R
assert left(R) == U
assert right(U) == R
assert right(L) == U
assert right(D) == L
assert right(R) == D

# R D L U
DIR = {R: (0, 1), D: (1, 0), L: (0, -1), U: (-1, 0)}

# Make graph, G :: (side, y, x) -> facing -> ((side', y', x'), facing')
G = {}

# Neighbors in the same region
for (r, y, x), ch in grid.items():
    if ch == '#':
        continue

    for dir, (dy, dx) in DIR.items():
        if grid.get((r, y + dy, x + dx)) == '.':
            G.setdefault((r, y, x), {})[dir] = ((r, y + dy, x + dx), dir)

def jump(dir0, dir1):
    def _f(y, x):
        if dir0 == R:
            tmp = y
        elif dir0 == D:
            tmp = N - 1 - x
        elif dir0 == L:
            tmp = N - 1 - y
        elif dir0 == U:
            tmp = x

        if dir1 == R:
            return (tmp, 0)
        elif dir1 == D:
            return (0, N - 1 - tmp)
        elif dir1 == L:
            return (N - 1 - tmp, N - 1)
        elif dir1 == U:
            return (N - 1, tmp)

    return _f

def glue(r0, dir0, r1, dir1):
    if dir0 == U:
        ys0 = repeat(0, N)
        xs0 = range(N)
    elif dir0 == D:
        ys0 = repeat(N - 1, N)
        xs0 = range(N)
    elif dir0 == L:
        ys0 = range(N)
        xs0 = repeat(0, N)
    elif dir0 == R:
        ys0 = range(N)
        xs0 = repeat(N - 1, N)

    f = jump(dir0, dir1)

    for y0, x0 in zip(ys0, xs0):
        y1, x1 = f(y0, x0)
        assert (r1, y1, x1) in grid

        if grid[(r1, y1, x1)] == '.':
            G.setdefault((r0, y0, x0), {})[dir0] = ((r1, y1, x1), dir1)

# R D L U
glue(1, U, 6, R)
glue(1, L, 5, R)
glue(1, D, 3, D)
glue(1, R, 2, R)

glue(2, U, 6, U)
glue(2, L, 1, L)
glue(2, D, 3, L)
glue(2, R, 4, L)

glue(3, U, 1, U)
glue(3, L, 5, D)
glue(3, D, 4, D)
glue(3, R, 2, U)

glue(4, U, 3, U)
glue(4, L, 5, L)
glue(4, D, 6, L)
glue(4, R, 2, L)

glue(5, U, 3, R)
glue(5, L, 1, R)
glue(5, D, 6, D)
glue(5, R, 4, R)

glue(6, U, 5, U)
glue(6, L, 1, D)
glue(6, D, 2, D)
glue(6, R, 4, U)

assert G[(1, 0, 16)][U] == ((6, 16, 0), R)
assert G[(4, 2, 0)][L] == ((5, 2, N - 1), L)

i = 0
pos = (1, 0, 0)
facing = R

while i < len(path):
    if path[i] == 'R':
        facing = right(facing)
        i += 1
    elif path[i] == 'L':
        facing = left(facing)
        i += 1
    elif path[i].isdigit():
        j = i + 1

        while j < len(path) and path[j].isdigit():
            j += 1

        n = int(path[i:j])
        i = j

        for _ in range(n):
            assert pos in G

            r, y, x = pos

            if facing not in G[pos]:
                break

            pos, facing = G[pos][facing]
            assert grid[pos] == '.', pos

r, row0, col0 = pos
dy, dx = REGION_OFFSETS[r]
col = N * dx + col0 + 1
row = N * dy + row0 + 1
ans = 1000 * row + 4 * col + facing
print(ans)
