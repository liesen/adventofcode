L = [ln.rstrip() for ln in open(0)]

elems = {(r, c): x for r, row in enumerate(L) for c, x in enumerate(row)}
walls = {(r, c) for (r, c), x in elems.items() if x == "#"}

S = next((r, c) for (r, c), x in elems.items() if x == "S")
E = next((r, c) for (r, c), x in elems.items() if x == "E")

# Single path from S to E
r, c = S
n = 0
visited = {(r, c): 0}

while (r, c) != E:
    adj = {
        (r + dr, c + dc)
        for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]
        if (
            (r + dr, c + dc) not in walls
            and (r + dr, c + dc) not in visited
            and (r + dr, c + dc) in elems
        )
    }

    if not adj:
        continue

    assert len(adj) == 1
    r, c = adj.pop()
    n += 1
    visited[(r, c)] = n


# Part 1 & 2
ans1 = 0
ans2 = 0

for (r1, c1), n1 in visited.items():
    for (r2, c2), n2 in visited.items():
        dt = abs(r1 - r2) + abs(c1 - c2)

        if 2 <= dt <= 20:
            saved = n2 - (n1 + dt)

            if saved >= 100:
                if dt == 2:
                    ans1 += 1

                ans2 += 1

print(ans1)
print(ans2)
