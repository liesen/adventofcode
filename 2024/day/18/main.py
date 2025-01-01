from collections import deque

lines = [ln.rstrip() for ln in open(0)]
bytes = [(int(ln[: ln.index(",")]), int(ln[ln.index(",") + 1 :])) for ln in lines]
xmax = ymax = 70


def bfs(walls: set[tuple[int, int]]) -> int | None:
    q = deque([(0, 0, 0)])
    visited = {(0, 0)}

    while q:
        n, x, y = q.popleft()

        if x == xmax and y == ymax:
            return n

        for x2, y2 in [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]:
            if (
                0 <= x2 <= xmax
                and 0 <= y2 <= ymax
                and (x2, y2) not in visited
                and (x2, y2) not in walls
            ):
                visited.add((x2, y2))
                q.append((n + 1, x2, y2))


                
# Part 1
ans1 = bfs(set(bytes[:1024]))
assert ans1 is not None
print(ans1)


# Part 2
lo = 0
hi = len(bytes) - 1

while lo < hi - 1:
    mid = (lo + hi) // 2

    if bfs(set(bytes[0:mid])) is not None:
        lo = mid
    else:
        hi = mid

x, y = bytes[lo]
print(f"{x},{y}")
