from collections import deque
from functools import cache


with open("input.txt") as f:
    S = [ln.rstrip() for ln in f]

# Strip walls from input
num_rows = len(S) - 2
num_cols = len(S[0]) - 2

DIR = {
    '^': (-1, 0),
    'v': (1, 0),
    '<': (0, -1),
    '>': (0, 1),
}

# Initial blizzard conditions
blizzard = {
    (y - 1, x - 1): DIR[ch]
    for y, ln in enumerate(S)
    for x, ch in enumerate(ln)
    if ch in "^v<>"
}

# Returns where the blizzard initially at @p@ will be at time step @t@
def blizzard_fn(p, dp):
    y, x = p
    dy, dx = dp
    return cache(lambda t: ((y + dy * t) % num_rows, (x + dx * t) % num_cols))

blizzard_fns = [blizzard_fn(p, dp) for p, dp in blizzard.items()]

# Part 1
start = (-1, 0)
end = (num_rows, num_cols - 1)
queue = deque([(0, start)])
seen = set()
maxn = 0

while queue:
    n, p = queue.popleft()

    if n > maxn:
        maxn = n
        print(maxn)

    if p == end:
        print(n)
        break

    # Check if we're in a blizzard
    if any(fn(n) == p for fn in blizzard_fns):
        continue

    if (n, p) in seen:
        continue

    seen.add((n, p))
    y, x = p

    # Wait at current position
    queue.append((n + 1, p))

    for dy, dx in zip([-1, 1, 0, 0], [0, 0, -1, 1]):
        yy, xx = y + dy, x + dx
        pp = (yy, xx)

        if (
            (0 <= yy < num_rows and 0 <= xx < num_cols)
            or pp == end  # Allow reaching the end
        ):
            queue.append((n + 1, pp))

