from itertools import product

with open(0) as f:
    paper_rolls = {
        (r, c) for r, ln in enumerate(f) for c, x in enumerate(ln.rstrip()) if x == "@"
    }

def neighbors(p):
    r, c = p
    return (
        (r + dr, c + dc)
        for dr, dc in product([-1, 0, 1], repeat=2)
        if not (dr == 0 and dc == 0)
    )

def count_neighbors(paper_rolls, p):
    return sum(1 for q in neighbors(p) if q in paper_rolls)

def accessible(paper_rolls):
    return {p for p in paper_rolls if count_neighbors(paper_rolls, p) < 4}

part1 = True
ans1 = 0
ans2 = 0

while a := accessible(paper_rolls):
    if part1:
        ans1 = len(a)
        part1 = False

    ans2 += len(a)
    paper_rolls -= a

print(ans1)
print(ans2)
