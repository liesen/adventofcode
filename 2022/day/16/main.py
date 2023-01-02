from collections import defaultdict, deque
from functools import cache


AA = 0
valves = []

with open(0) as f:
    for ix, ln in enumerate(f):
        i = len("Valve ")
        j = ln.index(" ", i)
        valve = ln[i:j]
        i = j + len(" has flow rate=")
        j = ln.index(";", i)
        flow_rate = int(ln[i:j])

        if ln.startswith("; tunnels lead to valves ", j):
            i = j + len("; tunnels lead to valves ")
            neighbors = ln[i:].rstrip().split(", ")
        elif ln.startswith("; tunnel leads to valve ", j):
            i = j + len("; tunnel leads to valve ")
            neighbors = [ln[i:].rstrip()]
        else:
            raise Exception("bad input")

        valves.append((1 << ix, valve, flow_rate, neighbors))

        if valve == "AA":
            AA = 1 << ix


assert AA, "no AA in input"

# Construct adjacency graph. Convert valve "names" to bitmasks.
G = {
    key: {
        (1, neighbor_mask)
        for neighbor_mask, neighbor, _, _ in valves
        if neighbor in neighbors
    }
    for key, valve, flow_rate, neighbors in valves
}
flow_rates = {mask: flow_rate for mask, valve, flow_rate, neighbors in valves}

# Compress graph, removing edges between valves with no flow rate
G2 = defaultdict(dict)

for src, tunnels in G.items():
    queue = deque(tunnels)
    seen = set([src])

    while queue:
        cost, v = queue.popleft()

        if v in seen:
            continue

        seen.add(v)

        if flow_rates[v]:
            G2[src][v] = cost

        for step_cost, w in G[v]:
            queue.append((cost + step_cost, w))

# All valves left to open
queue = sum(valve for valve, flow_rate in flow_rates.items() if flow_rate)

# Part 1
@cache
def part1(t, pos, queue):
    return max(
        (
            (
                flow_rates[new_pos] * (t - dt - 1)
                + part1(t - dt - 1, new_pos, queue - new_pos)
            )
            for new_pos in G2
            if new_pos & queue
            for dt in [G2[pos][new_pos]]
            if dt < t
        ),
        default=0,
    )

print(part1(30, AA, queue))

# Part 2
def part2(t, pos, queue):
    return max(
        part1(26, AA, queue), # Elephant opens remaining valves
        max(
            (
                (
                    flow_rates[new_pos] * (t - dt - 1)
                    + part2(t - dt - 1, new_pos, queue - new_pos)
                )
                for new_pos in G2
                if new_pos & queue
                for dt in [G2[pos][new_pos]]
                if dt < t  # Must have enough time
            ),
            default=0,
        ),
    )

print(part2(26, AA, queue))

