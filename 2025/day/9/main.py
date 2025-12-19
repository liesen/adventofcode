input = """7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3"""


def parse(s: str) -> tuple[int, int]:
    x, y = s.split(",")
    return int(x), int(y)


def area(a, b):
    ax, ay = a
    bx, by = b
    return (max(ax, bx) - min(ax, bx) + 1) * (max(ay, by) - min(ay, by) + 1)


lines = input.splitlines()

with open(0) as f:
    lines = f.read().splitlines()
    ...

tiles = [parse(line) for line in lines]

# Part 1
print(max(area(a, b) for i, a in enumerate(tiles) for b in tiles[i + 1 :]))

# Part 2
#
# Run `python svg.py < input > input.svg` to visually inspect the
# polygon. It looks like this:
#
#      ████
#    ███████
#   █████████
#  ███████████
#            █
#  ███████████
#   ██████████
#    ████████
#     █████
#
# The right most tile in the upper horizontal edge of the gap is
# the lower right tile in the maximum rectangle in the upper semi-
# circle. And vice versa in the lower semi-circle.
ans2 = 0
ymax = max(y for x, y in tiles)
tiles_mirror_y = [(x, ymax - y) for x, y in tiles]

# Check the original polygon and the polygon flipped horizontally. Then
# we need to find the largest rectangle in the upper semi-circle.
for tiles in [tiles, tiles_mirror_y]:
    xmin = min(x for x, y in tiles)
    xmax = max(x for x, y in tiles)
    ymin = min(y for x, y in tiles)
    ymax = max(y for x, y in tiles)
    xmid = (xmin + xmax) // 2  # Vertical semi-circle mid line
    ymid = (ymin + ymax) // 2  # Horizontal semi-circle mid line
    horizontal_edges = sorted(
        (y1, (min(x1, x2), max(x1, x2)))
        for i, (x1, y1) in enumerate(tiles)
        for x2, y2 in tiles[i + 1 :]
        if y1 == y2
    )

    def dist_to_ymid(h):
        y, _ = h
        return abs(y - ymid)

    # Find the horizontal line just above the middle
    lower_y, (lower_x1, lower_x2) = min(
        (
            (y, (x1, x2))
            for y, (x1, x2) in horizontal_edges
            if y <= ymid and (x2 - x1) >= (xmax - xmin) // 2
        ),
        key=dist_to_ymid,
    )

    # Find a horizontal edge above the right vertex of this edge. The
    # maximum rectangle can not be taller than this edge.
    for upper_y, (upper_x1, upper_x2) in [
        (y, (x1, x2))
        for (y, (x1, x2)) in horizontal_edges
        if y < lower_y and x1 <= lower_x2 <= x2
    ]:
        # Find the tile corresponding to the top left corner of the
        # rectangle
        a = max(
            [
                ((top_left_x, top_left_y), (lower_x2, lower_y))
                for (top_left_x, top_left_y) in tiles
                if upper_y <= top_left_y < lower_y and top_left_x < lower_x2
            ],
            key=lambda a: area(*a),
        )
        top_left, bottom_right = a
        assert top_left in tiles
        assert bottom_right in tiles
        (a1, b1), (a2, b2) = a
        ans2 = max(ans2, area(*a))

print(ans2)
