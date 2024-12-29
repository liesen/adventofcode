import heapq

elems = {
    ((r, c), ch) for r, row in enumerate(open(0)) for c, ch in enumerate(row.rstrip())
}
walls = {p for p, ch in elems if ch == "#"}
Sr, Sc = next(p for p, ch in elems if ch == "S")
E = next(p for p, ch in elems if ch == "E")


V = tuple[int, int, int, int]

start: V = (Sr, Sc, 0, 1)
q = [(0, start)]
heapq.heapify(q)
dist: dict[V, int] = {start: 0}
prev: dict[V, set[V]] = {}

while q:
    dist_u, u = heapq.heappop(q)
    r, c, dr, dc = u

    if (r, c) == E:
        # Part 1
        print(dist_u)

        # Part 2
        best_tiles: set[tuple[int, int]] = set()
        qq = {u}

        while qq:
            best_tiles |= {(r, c) for r, c, dr, dc in qq}
            qq = {v for u in qq for v in prev.get(u, set())}

        print(len(best_tiles))
        break

    for dist_uv, v in (
        [(1, (r + dr, c + dc, dr, dc))]
        + (dr == 0) * [(1001, (r - 1, c, -1, 0)), (1001, (r + 1, c, 1, 0))]
        + (dc == 0) * [(1001, (r, c - 1, 0, -1)), (1001, (r, c + 1, 0, 1))]
    ):
        r2, c2, _, _ = v

        if (r2, c2) in walls:
            continue

        alt = dist_u + dist_uv

        if (dist_v := dist.get(v)) is None or alt < dist_v:
            # Found a shorter path to v
            dist[v] = alt
            prev[v] = {u}
            heapq.heappush(q, (alt, v))
        elif alt == dist_v:
            # Found a new path to v (via u) with the same dist
            prev[v].add(u)
