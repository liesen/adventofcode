from math import sqrt, prod


example = False
input = """162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689"""


JunctionBox = tuple[int, int, int]


def parse_line(s: str) -> JunctionBox:
    xs, ys, zs = s.split(",")
    return (int(xs), int(ys), int(zs))


def dist(a: JunctionBox, b: JunctionBox) -> float:
    ax, ay, az = a
    bx, by, bz = b
    return sqrt((ax - bx) ** 2 + (ay - by) ** 2 + (az - bz) ** 2)


# Parse input
lines = input.splitlines()

if not example:
    with open(0) as f:
        lines = [ln.rstrip() for ln in f]

junction_boxes = [parse_line(s) for s in lines]
pairs = sorted(
    [(a, b) for i, a in enumerate(junction_boxes) for b in junction_boxes[i + 1 :]],
    key=lambda ab: dist(*ab),
)


# Union-Find
def find(a: JunctionBox, parents: dict[JunctionBox, JunctionBox]) -> JunctionBox:
    if (b := parents[a]) == a:
        return a

    return find(b, parents)


def union(a: JunctionBox, b: JunctionBox, parents: dict[JunctionBox, JunctionBox]):
    parents[find(a, parents)] = find(b, parents)


# Part 1
parents1 = {a: a for a in junction_boxes}

for a, b in pairs[0 : 10 if example else 1000]:
    union(a, b, parents1)

circuits = {a: [b for b in parents1 if find(b, parents1) == a] for a in parents1}
circuit_sizes = [len(circuit) for circuit in circuits.values()]
print(prod(sorted(circuit_sizes, reverse=True)[0:3]))


# Part 2
parents2 = {a: a for a in junction_boxes}

while len({find(a, parents2) for a in parents2}) > 1:
    a, b = pairs.pop(0)
    union(a, b, parents2)

ax, _, _ = a
bx, _, _ = b
print(ax * bx)
