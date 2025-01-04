import re
import z3

S = open(0).read()

ans1 = 0
ans2 = 0

for i, s in enumerate(S.split("\n\n")):
    lines = s.splitlines()
    assert (ma := re.match(r"Button A: X\+(\d+), Y\+(\d+)", lines[0])) is not None
    axs, ays = ma.groups()
    ax = int(axs)
    ay = int(ays)
    assert (mb := re.match(r"Button B: X\+(\d+), Y\+(\d+)", lines[1])) is not None
    bxs, bys = mb.groups()
    bx = int(bxs)
    by = int(bys)
    assert (mp := re.match(r"Prize: X=(\d+), Y=(\d+)", lines[2])) is not None
    pxs, pys = mp.groups()
    px = int(pxs)
    py = int(pys)

    opt = z3.Optimize()
    na = z3.Int("na")
    nb = z3.Int("nb")
    cost = z3.Int("cost")
    opt.add(cost == na * 3 + nb)

    # Part 1
    opt.push()
    opt.add(na * ax + nb * bx == px)
    opt.add(na * ay + nb * by == py)
    opt.add(na <= 100, nb <= 100)

    if opt.check() == z3.sat:
        ans1 += opt.model()[cost].as_long()

    opt.pop()
    
    # Part 2
    opt.add(na * ax + nb * bx == (px + 10000000000000))
    opt.add(na * ay + nb * by == (py + 10000000000000))

    if opt.check() == z3.sat:
        ans2 += opt.model()[cost].as_long()

print(ans1)
print(ans2)
