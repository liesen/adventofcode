with open("input.txt") as f:
    trees = {
        (r, c): int(h)
        for r, hs in enumerate(f)
        for c, h in enumerate(hs.rstrip())
    }
    num_rows = max(r for r, c in trees) + 1
    num_cols = max(c for r, c in trees) + 1

# Part 1
visible = {
    (r, c)
    for (r, c), h in trees.items()
    if any([
        all(trees[(rr, c)] < h for rr in range(0, r)), # top
        all(trees[(rr, c)] < h for rr in range(r + 1, num_rows)),  # bottom
        all(trees[(r, cc)] < h for cc in range(0, c)),  # left
        all(trees[(r, cc)] < h for cc in range(c + 1, num_cols))  # right
    ])
}

print(len(visible))

# Part 2
from math import prod


def takewhile_inclusive(p, it):
    for x in it:
        if not p(x):
            yield x
            break

        yield x


def viewing_distance(h, it):
    return sum(1 for _ in takewhile_inclusive(lambda x: x < h, it))


scenic_scores = {
    (r, c): prod([
        viewing_distance(h, (trees[(rr, c)] for rr in range(r - 1, -1, -1))), # up
        viewing_distance(h, (trees[(rr, c)] for rr in range(r + 1, num_rows))),  # down
        viewing_distance(h, (trees[(r, cc)] for cc in range(c - 1, -1, -1))),  # left
        viewing_distance(h, (trees[(r, cc)] for cc in range(c + 1, num_cols)))  # right
    ])
    for (r, c), h in trees.items()
}

print(max(scenic_scores.values()))