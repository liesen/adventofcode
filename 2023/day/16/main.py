from collections import defaultdict

G = {
    (r, c): ch for r, ln in enumerate(open("input")) for c, ch in enumerate(ln.rstrip())
}
maxrow = max(r for r, c in G.keys())
maxcol = max(c for r, c in G.keys())


def turn(r, c, dr, dc):
    if not (ch := G.get((r, c))):
        return []

    if ch == ".":
        return [(r, c, dr, dc)]

    if ch == "-":
        if dr == 0:
            return [(r, c, dr, dc)]
        else:
            return [(r, c, 0, -1), (r, c, 0, 1)]

    if ch == "|":
        if dc == 0:
            return [(r, c, dr, dc)]
        else:
            return [(r, c, -1, 0), (r, c, 1, 0)]

    if ch == "/":
        if dc == 0:
            return [(r, c, dc, -dr)]
        elif dr == 0:
            return [(r, c, -dc, dr)]

    if ch == "\\":
        if dc == 0:
            return [(r, c, -dc, dr)]
        elif dr == 0:
            return [(r, c, dc, -dr)]

    raise Exception("unreachable")


# Part 1
def energize(r, c, dr, dc):
    energy = defaultdict(set)
    beams = turn(r, c, dr, dc)

    while beams:
        new_beams = []

        for r, c, dr, dc in beams:
            if (dr, dc) in energy[(r, c)]:
                continue

            energy[(r, c)].add((dr, dc))
            new_beams.extend(turn(r + dr, c + dc, dr, dc))

        beams = new_beams

    return len(energy)

# Part 1
print(energize(0, 0, 0, 1))

# Part 2
print(
    max(
        max(energize(0, c, 1, 0) for c in range(maxcol + 1)),
        max(energize(maxrow, c, -1, 0) for c in range(maxcol + 1)),
        max(energize(r, 0, 0, 1) for r in range(maxrow + 1)),
        max(energize(r, maxcol, 0, -1) for r in range(maxrow + 1))
    )
)

