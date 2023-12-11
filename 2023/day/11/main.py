from itertools import islice


S = [ln.rstrip() for ln in open("input")]
empty_rows = [r for r, row in enumerate(S) if all(ch == "." for ch in row)]
empty_columns = [c for c, col in enumerate(zip(*S)) if all(ch == "." for ch in col)]

# Expand the universes
galaxies1 = []
galaxies2 = []

r1, c1 = 0, 0
r2, c2 = 0, 0

for r, row in enumerate(S):
    if r in empty_rows:
        r1 += 2
        r2 += 1_000_000
        continue

    c1 = 0
    c2 = 0

    for c0, ch in enumerate(row):
        if c0 in empty_columns:
            c1 += 2
            c2 += 1_000_000
            continue

        if ch == "#":
            galaxies1.append((r1, c1))
            galaxies2.append((r2, c2))

        c1 += 1
        c2 += 1

    r1 += 1
    r2 += 1


# Shortest distance is the Manhattan distance
def dist(a, b):
    r1, c1 = a
    r2, c2 = b
    return abs(r2 - r1) + abs(c2 - c1)


ans1 = 0
ans2 = 0

for i, (a1, a2) in enumerate(zip(galaxies1, galaxies2)):
    for b1, b2 in islice(zip(galaxies1, galaxies2), i + 1, None):
        ans1 += dist(a1, b1)
        ans2 += dist(a2, b2)


print(ans1)
print(ans2)
