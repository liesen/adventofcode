from itertools import product

with open("input.txt") as f:
    cubes = set()

    for ln in f:
        x, y, z = map(int, ln.split(","))
        cubes.add((x, y, z))

def adj(p):
    return {
        tuple(map(sum, zip(p, d)))
        for d in product(*[[-1, 0, 1]] * len(p))
        if sum(map(abs, d)) == 1
    }

# Part 1
print(sum(6 - sum(q in cubes for q in adj(p)) for p in cubes))
