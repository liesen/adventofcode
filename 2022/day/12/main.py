import heapq


with open("input.txt") as f:
    G = {
        (r, c): x
        for r, ln in enumerate(f)
        for c, x in enumerate(ln.rstrip())
    }

    S = next(p for p, x in G.items() if x == 'S')
    G[S] = 'a'
    E = next(p for p, x in G.items() if x == 'E')
    G[E] = 'z'

    G = {p: ord(x) - ord('a') for p, x in G.items()}

# Part 1
queue = [(0, S)]
seen = set()

while queue:
    n, p = heapq.heappop(queue)

    if p == E:
        print(n)
        break

    if p in seen:
        continue

    seen.add(p)

    for dr, dc in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
        r, c = p
        q = (r + dr, c + dc)

        if q in G and G[q] - G[p] <= 1:
            heapq.heappush(queue, (n + 1, q))


# Part 2
queue = [(0, s) for s in G if G[s] == 0]
seen = set()

while queue:
    n, p = heapq.heappop(queue)

    if p == E:
        print(n)
        break

    if p in seen:
        continue

    seen.add(p)

    for dr, dc in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
        r, c = p
        q = (r + dr, c + dc)

        if q in G and G[q] - G[p] <= 1:
            heapq.heappush(queue, (n + 1, q))