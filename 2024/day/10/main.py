from collections import deque
from functools import cache

S = [row.rstrip() for row in open(0)]
G = {(r, c): int(x) for r, row in enumerate(S) for c, x in enumerate(row) if x != "."}
trailheads = {p for p, h in G.items() if h == 0}


# Part 1
def score(p: tuple[int, int]) -> int:
    q = deque([p])
    visited: set[tuple[int, int]] = set([p])
    ans = 0

    while q:
        p = q.popleft()
        r, c = p
        h = G[p]

        if h == 9:
            ans += 1
            continue

        for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            pp = (r + dr, c + dc)

            if pp not in visited and pp in G and G[pp] == h + 1:
                visited.add(pp)
                q.append(pp)

    return ans


print(sum(score(s) for s in trailheads))


# Part 2
@cache
def trailhead_rating(p: tuple[int, int]) -> int:
    r, c = p
    n = G[p]

    if n == 9:
        return 1

    return sum(
        trailhead_rating((r + dr, c + dc))
        for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]
        if (r + dr, c + dc) in G
        if G[(r + dr, c + dc)] == n + 1
    )


print(sum(trailhead_rating(s) for s in trailheads))
