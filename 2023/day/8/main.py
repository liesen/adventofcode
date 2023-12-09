from math import lcm


G = {} 

with open("input") as f:
    path, body = f.read().strip().split("\n\n")

    for ln in body.splitlines():
        src, dsts = ln.split(" = ")
        l, r = dsts[1:-1].split(", ")
        G[src] = (l, r)


def step(p, n):
    l, r = G[p]

    if path[n % len(path)] == 'L':
        return l
    elif path[n % len(path)] == 'R':
        return r

    raise Exception("bad step")


# Shortest path from all A or Z nodes to Z nodes
S = {}

for p in G:
    if p[-1] not in "AZ":
        continue

    seen = set()
    p0 = p
    n = 0

    while ((m := n % len(path)) or True) and (m, p) not in seen:
        if p[-1] == "Z":
            S.setdefault((m, p0), {})[p] = n

        seen.add((m, p))
        p = step(p, n)
        n += 1

# Part 1
print(S[(0, "AAA")]["ZZZ"])

# Part 2
# By inspection we find that all paths leads to a Z node where it
# stays in a cycle
cycles = []

for n, p in S:
    # Make sure its a unique terminal node
    assert len(S[(n, p)]) == 1
    q, m = next(iter(S[(n, p)].items()))
    assert q[-1] == "Z"

    if p[-1] == "A":
        cycles.append(m)

print(lcm(*cycles))
