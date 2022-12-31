from collections import deque


droplet_cubes = set()

with open("input.txt") as f:
    for ln in f:
        x, y, z = map(int, ln.rstrip().split(","))
        droplet_cubes.add((x, y, z))

def adj(p):
    x, y, z = p
    yield (x - 1, y, z)
    yield (x + 1, y, z)
    yield (x, y - 1, z)
    yield (x, y + 1, z)
    yield (x, y, z - 1)
    yield (x, y, z + 1)

# Part 1
print(sum(q not in droplet_cubes for p in droplet_cubes for q in adj(p)))

# Part 2

# Flood fill from outside the droplet to find all air cubes
xs = {x for x, y, z in droplet_cubes}
ys = {y for x, y, z in droplet_cubes}
zs = {z for x, y, z in droplet_cubes}

xmax = max(xs)
ymax = max(ys)
zmax = max(zs)
xmin = min(xs)
ymin = min(ys)
zmin = min(zs)

air_cube = (xmin - 1, ymin - 1, zmin - 1)
assert air_cube not in droplet_cubes

air_cubes = set()
queue = deque([air_cube])

while queue:
    x, y, z = p = queue.popleft()

    # Stay inside bounding box
    if not (
        xmin - 1 <= x <= xmax + 1
        and ymin - 1 <= y <= ymax + 1
        and zmin - 1 <= z <= zmax + 1
    ):
        continue

    if p in droplet_cubes:
        continue

    if p in air_cubes:
        continue

    air_cubes.add(p)
    queue.extend(adj(p))

# Check which air cube faces intersects with droplet cube faces
print(sum(q in droplet_cubes for p in air_cubes for q in adj(p)))
