from functools import cache


AA = 0
valves = []

with open("input.txt") as f:
    for ix, ln in enumerate(f):
        i = len("Valve ")
        j = ln.index(" ", i)
        valve = ln[i:j]
        i = j + len(" has flow rate=")
        j = ln.index(";", i)
        flow_rate = int(ln[i:j])

        if ln.startswith("; tunnels lead to valves ", j):
            i = j + len("; tunnels lead to valves ")
            neighbors = ln[i:].rstrip().split(', ')
        elif ln.startswith("; tunnel leads to valve ", j):
            i = j + len("; tunnel leads to valve ")
            neighbors = [ln[i:].rstrip()]
        else:
            raise Exception("bad input")

        valves.append((1 << ix, valve, flow_rate, neighbors))

        if valve == "AA":
            AA = 1 << ix


# Construct adjacency graph. Convert valve "names" to bitmasks.
G = {
    key: {
        neighbor_mask
        for neighbor_mask, neighbor, _, _ in valves
        if neighbor in neighbors
    }
    for key, valve, flow_rate, neighbors in valves
}
flow_rates = {mask: flow_rate for mask, valve, flow_rate, neighbors in valves}
ALL = sum(G)


# Part 1

# Solve using dynamic programming
memo = {}

def dp1(t, valve, opened, pressure):
    if t >= 30:
        return 0

    if opened == ALL:
        return pressure * (30 - t)

    memo_key = (t, valve, opened)

    if memo_key in memo:
        return memo[memo_key]

    # Move to another valve
    res = max(
        pressure + dp1(t + 1, v, opened, pressure)
        for v in G[valve]
    )

    # Open valve unless opened. Skip valves that does not have any flow rate.
    if flow_rates[valve] and not (opened & valve):
        res = max(
            res,
            pressure + dp1(t + 1, valve, opened | valve, pressure + flow_rates[valve])
        )

    memo[memo_key] = res
    return res

print(dp1(0, AA, 0, 0))
