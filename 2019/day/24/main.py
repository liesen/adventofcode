from collections import Counter

with open('input.txt') as fp:
    S = fp.read()
    biodiversity = sum(1 << (r * 5 + c)
                       for r, xs in enumerate(S.splitlines())
                       for c, x in enumerate(xs)
                       if x == '#')
    G = Counter((0, y - 2, x - 2)
                for y, xs in enumerate(S.splitlines())
                for x, c in enumerate(xs)
                if c == '#')

# Part 1
ans = biodiversity
seen = set()

while ans not in seen:
    seen.add(ans)
    new_biodiversity = ans

    for r in range(5):
        for c in range(5):
            n = 0

            for dr, dc in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
                if 0 <= r + dr < 5 and 0 <= c + dc < 5:
                    ix = (r + dr) * 5 + c + dc
                    n += (ans & (1 << ix)) > 0

            ix = r * 5 + c

            if ans & (1 << ix) and n != 1:
                new_biodiversity &= ~(1 << ix)
            elif not (ans & (1 << ix)) and 1 <= n <= 2:
                new_biodiversity |= 1 << ix

    ans = new_biodiversity

print(ans)

# Part 2
S0 = '''
....#
#..#.
#..##
..#..
#....
'''
def adjacents(bug):
    d, y, x = bug

    for dy, dx in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        if (-2 <= dy + y <= 2 and -2 <= dx + x <= 2
            and not (y + dy == 0 and x + dx == 0)):
            yield (d, y + dy, x + dx)

    # Tiles from the grid that contains this one 
    if y == 2:
        yield (d - 1, 1, 0)

    if y == -2:
        yield (d - 1, -1, 0)

    if x == -2:
        yield (d - 1, 0, -1)

    if x == 2:
        yield (d - 1, 0, 1)

    # Get tiles from the next recursive grid
    if y == -1 and x == 0:
        for x1 in range(-2, 3):
            yield (d + 1, -2, x1)
    
    if y == 1 and x == 0:
        for x1 in range(-2, 3):
            yield (d + 1, 2, x1)
    
    if y == 0 and x == -1:
        for y1 in range(-2, 3):
            yield (d + 1, y1, -2)
    
    if y == 0 and x == 1:
        for y1 in range(-2, 3):
            yield (d + 1, y1, 2)

def step(grid):
    density = Counter(a for c in grid for a in adjacents(c))
    return Counter(c
                   for c in (grid.keys() | density.keys())
                   for n in [density.get(c, 0)]
                   if n == 1 or n == 2 and c not in grid)

for i in range(200):
    density = Counter(a for g in G for a in adjacents(g))
    G =  Counter(b
                 for b in (G.keys() | density.keys())
                 for n in [density.get(b, 0)]
                 if n == 1 or n == 2 and b not in G)

print(len(G))