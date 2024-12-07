from itertools import product

with open("input") as f:
    S = [ln.rstrip() for ln in f]
    numrows = len(S)
    numcols = len(S[0])
    sr, sc = next(
        (r, c) for r, ln in enumerate(S) for c, ch in enumerate(ln) if ch == "^"
    )
    obstacles = {
        (r, c) for r, ln in enumerate(S) for c, ch in enumerate(ln) if ch == "#"
    }


def walk(r: int, c: int, obstacles: set[tuple[int, int]]):
    dr, dc = -1, 0
    seen: set[tuple[int, int]] = {(r, c)}

    while 0 <= r + dr < numrows and 0 <= c + dc < numcols:
        if (r + dr, c + dc) in obstacles:
            match (dr, dc):
                case (-1, 0):
                    dr, dc = 0, 1
                case (1, 0):
                    dr, dc = 0, -1
                case (0, 1):
                    dr, dc = 1, 0
                case (0, -1):
                    dr, dc = -1, 0

            continue

        r += dr
        c += dc
        seen.add((r, c))

    return seen


# Part 1
path1 = walk(sr, sc, obstacles)
print(len(path1))


# Part 2
def loop(r: int, c: int, obstacles: set[tuple[int, int]]):
    dr, dc = -1, 0
    seen2: set[tuple[int, int, int, int]] = {(r, c, dr, dc)}

    while 0 <= r + dr < numrows and 0 <= c + dc < numcols:
        if (r + dr, c + dc) in obstacles:
            match (dr, dc):
                case (-1, 0):
                    dr, dc = 0, 1
                case (1, 0):
                    dr, dc = 0, -1
                case (0, 1):
                    dr, dc = 1, 0
                case (0, -1):
                    dr, dc = -1, 0
                case _:
                    pass

        else:
            r += dr
            c += dc

        if (r, c, dr, dc) in seen2:
            return True

        seen2.add((r, c, dr, dc))

    return False

# Put an obstacle in the guards path to cause them to re-route
ans2 = sum(
    1
    for r, c in path1
    if not ((r, c) in obstacles or (r, c) == (sr, sc)) 
    if loop(sr, sc, obstacles | {(r, c)})
)

print(ans2)
