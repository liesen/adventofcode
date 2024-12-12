from collections import deque


S = [ln.rstrip() for ln in open(0)]

# Find regions
unvisited = {((r, c), label) for r, row in enumerate(S) for c, label in enumerate(row)}
regions: list[tuple[str, set[tuple[int, int]]]] = []

while unvisited:
    p, label = unvisited.pop()

    # Bfs to find all plots in the region
    q = deque([p])
    region = set([p])

    while q:
        r, c = q.pop()

        for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            if ((r + dr, c + dc), label) not in unvisited:
                continue

            unvisited.remove(((r + dr, c + dc), label))
            region.add((r + dr, c + dc))
            q.appendleft((r + dr, c + dc))

    regions.append((label, region))

ans1 = 0
ans2 = 0

for label, region in regions:
    area = len(region)

    # If a plot has no neighbor to one of its sides, then it's
    # part of the perimeter (in the open direction)
    perimeter = sum(
        (r + dr, c + dc) not in region
        for r, c in region
        for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]
    )
    ans1 += area * perimeter

    # Number of corners = number of sides
    sides = (
        #  .
        # .X
        sum(
            1
            for r, c in region
            if (r, c - 1) not in region and (r - 1, c) not in region
        )
        # xX
        # .x
        + sum(
            1
            for r, c in region
            if (r, c - 1) in region
            and (r + 1, c) in region
            and (r + 1, c - 1) not in region
        )
        # .
        # X.
        + sum(
            1
            for r, c in region
            if (r - 1, c) not in region and (r, c + 1) not in region
        )
        # .x
        # xX
        + sum(
            1
            for r, c in region
            if (r - 1, c) in region
            and (r, c - 1) in region
            and (r - 1, c - 1) not in region
        )
        # .X
        #  .
        + sum(
            1
            for r, c in region
            if (r + 1, c) not in region and (r, c - 1) not in region
        )
        # Xx
        # x.
        + sum(
            1
            for r, c in region
            if (r, c + 1) in region
            and (r + 1, c) in region
            and (r + 1, c + 1) not in region
        )
        # X.
        # .
        + sum(
            1
            for r, c in region
            if (r, c + 1) not in region and (r + 1, c) not in region
        )
        # x.
        # Xx
        + sum(
            1
            for r, c in region
            if (r - 1, c) in region
            and (r, c + 1) in region
            and (r - 1, c + 1) not in region
        )
    )
    ans2 += area * sides

print(ans1)
print(ans2)
