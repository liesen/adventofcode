with open('input.txt') as f:
    grid = [[z for z in ys.strip()] for ys in f]
    # for y, ys in enumerate(f):
    #     for x, z in enumerate(ys):

def print_grid(grid):
    for ys in grid:
        print(''.join(ys))

def print_dist(dist):
    for ys in dist:
        print(''.join([' ' if d == inf else str(d) for d in ys]))

print_grid(grid)

inf = 1e9

# dist = [[inf for x in ys] for ys in grid]
dist = {}

for y, ys in enumerate(grid):
    for x, z in enumerate(ys):
        if z == '#':
            continue

        for (dx, dy) in [(0, -1), (0, 1), (-1, 0), (1, 0)]:
            x_, y_ = x + dx, y + dy

            if 0 > x_ >= len(ys) or 0 > y_ >= len(grid):
                continue

            if grid[y][x] == '#':
                continue

            if (y, x) not in dist:
                dist[(y, x)] = {}

            dist[(y,x)][(y_, x_)] = 1

print(dist)
