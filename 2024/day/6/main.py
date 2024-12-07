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
    visited: set[tuple[int, int, int, int]] = {(r, c, dr, dc)}

    while 0 <= r + dr < numrows and 0 <= c + dc < numcols:
        if (r + dr, c + dc) in obstacles:
            dr, dc = dc, -dr
        else:
            r += dr
            c += dc

        if (r, c, dr, dc) in visited:
            return "loop", {(r, c) for r, c, _, _ in visited}

        visited.add((r, c, dr, dc))

    return "escape", {(r, c) for r, c, _, _ in visited}


# Part 1
match walk(sr, sc, obstacles):
    case "escape", path1:
        print(len(path1))

    case _:
        assert False, "loop in part 1"

# Part 2
ans2 = 0

# Put an obstacle in the guards path to cause them to re-route
for p in path1:
    match walk(sr, sc, obstacles | {p}):
        case "loop", _:
            ans2 += 1

        case _:
            pass

print(ans2)
