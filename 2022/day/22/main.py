from collections import defaultdict


G = []
maxcol = 0

with open("input.txt") as f:
    while (ln := next(f)) != "\n":
        lns = ln.rstrip()
        maxcol = max(maxcol, len(lns))
        G.append(lns)

    for i, ln in enumerate(G):
        G[i] = ln.ljust(maxcol, ' ')

    path = next(f).rstrip()

start = next(
    (y, x)
    for y, ln in enumerate(G)
    for x, ch in enumerate(ln)
    if ch == '.'
)

GG = {}

for y, ln in enumerate(G):
    for x, ch in enumerate(G[y]):
        if ch != '.':
            continue

        # Up
        if y == 0 or G[y - 1][x] == ' ':
            # Wrap
            for yy in range(len(G) - 1, 0, -1):
                match G[yy][x]:
                    case '.':
                        GG.setdefault((y, x), {})[(-1, 0)] = (yy, x)
                        break

                    case '#':
                        break
        else:
          if G[y - 1][x] == '.':
                GG.setdefault((y, x), {})[(-1, 0)] = (y - 1, x)

        # Down
        if y == len(G) - 1 or G[y + 1][x] == ' ':
            for yy in range(0, len(G)):
                match G[yy][x]:
                    case '.':
                        GG.setdefault((y, x), {})[(1, 0)] = (yy, x)
                        break

                    case '#':
                        break
        else:
            if G[y + 1][x] == '.':
                GG.setdefault((y, x), {})[(1, 0)] = (y + 1, x)

        # Left
        if x == 0 or G[y][x - 1] == ' ':
            for xx in range(len(G[y]) - 1, -1, -1):
                match G[y][xx]:
                    case '.':
                        GG.setdefault((y, x), {})[(0, -1)] = (y, xx)
                        break

                    case '#':
                        break
        else:
            if G[y][x - 1] == '.':
                GG.setdefault((y, x), {})[(0, -1)] = (y, x - 1)

        # Right
        if x == len(G[y]) - 1 or G[y][x + 1] == ' ':
            for xx in range(0, len(G[y])):
                match G[y][xx]:
                    case '.':
                        GG.setdefault((y, x), {})[(0, 1)] = (y, xx)
                        break

                    case '#':
                        break
        else:
            if G[y][x + 1] == '.':
                GG.setdefault((y, x), {})[(0, 1)] = (y, x + 1)

i = 0
pos = start
facing = (0, 1)

while i < len(path):
    if path[i] == 'R':
        match facing:
            case (0, 1):
                facing = (1, 0)
            case (0, -1):
                facing = (-1, 0)
            case (1, 0):
                facing = (0, -1)
            case (-1, 0):
                facing = (0, 1)

        i += 1
    elif path[i] == 'L':
        match facing:
            case (0, 1):
                facing = (-1, 0)
            case (0, -1):
                facing = (1, 0)
            case (1, 0):
                facing = (0, 1)
            case (-1, 0):
                facing = (0, -1)

        i += 1
    else:
        j = i + 1

        while j < len(path) and path[j].isdigit():
            j += 1

        n = int(path[i:j])
        i = j

        for _ in range(n):
            assert pos in GG

            if facing in GG[pos]:
                pos = GG[pos][facing]

row0, col0 = pos
row, col = row0 + 1, col0 + 1

match facing:
    case (0, 1):
        print(1000 * row + 4 * col + 0)
    case (1, 0):
        print(1000 * row + 4 * col + 1)
    case (0, -1):
        print(1000 * row + 4 * col + 2)
    case (-1, 0):
        print(1000 * row + 4 * col + 3)
