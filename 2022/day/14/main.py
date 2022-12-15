from itertools import chain

def parse_point(s):
    xs, ys = s.split(',', maxsplit=2)
    return (int(xs), int(ys))

rock = set()

with open("input.txt") as f:
    ymax = 0

    for ln in f:
        p, *ps = ln.rstrip().split(" -> ")
        x1, y1 = parse_point(p)
        ymax = max(ymax, y1)

        for p in ps:
            x2, y2 = parse_point(p)

            if x1 == x2:
                rock |= {(x1, y) for y in range(min(y1, y2), max(y1, y2) + 1)}
            elif y1 == y2:
                rock |= {(x, y1) for x in range(min(x1, x2), max(x1, x2) + 1)}

            x1, y1 = x2, y2
            ymax = max(ymax, y2)

# Part 1
def pour1(rock, sand, ymax, p):
    px, py = p

    if p in rock or p in sand:
        yield from ()
    elif py >= ymax:
        yield p
    else:
        yield next(chain(
            pour1(rock, sand, ymax, (px, py + 1)),
            pour1(rock, sand, ymax, (px - 1, py + 1)),
            pour1(rock, sand, ymax, (px + 1, py + 1)),
            iter([p]),
        ))

# Pour while sand does not go into the abyss
sand1 = set()

while (p := next(pour1(rock, sand1, ymax, (500,0)))):
    _, py = p
    
    # Don't count sand in the abyss
    if py >= ymax:
        break

    sand1.add(p)

print(len(sand1))

# Part 2
def pour2(rock, sand, ymax, p):
    px, py = p

    if p in rock or p in sand:
        yield from ()
    elif py >= ymax + 2:
        yield from ()
    else:
        yield next(chain(
            pour2(rock, sand, ymax, (px, py + 1)),
            pour2(rock, sand, ymax, (px - 1, py + 1)),
            pour2(rock, sand, ymax, (px + 1, py + 1)),
            iter([p]),
        ))

sand2 = set()

while (p := next(pour2(rock, sand2, ymax, (500,0)))):
    sand2.add(p)

    if p == (500,0):
        break

print(len(sand2))
