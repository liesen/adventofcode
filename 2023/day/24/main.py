from typing import Tuple
import fileinput
from dataclasses import dataclass


@dataclass(frozen=True)
class Hailstone:
    pos: Tuple[int, int, int]
    vel: Tuple[int, int, int]


hailstones = []

with fileinput.input("input") as f:
    for ln in f:
        poss, vels = ln.rstrip().split(" @ ", maxsplit=2)
        pos = tuple([int(x.strip()) for x in poss.split(",")])
        vel = tuple([int(x.strip()) for x in vels.split(",")])
        hailstones.append(Hailstone(pos, vel))

# Part 1
ans1 = 0

for i, a in enumerate(hailstones):
    for j, b in enumerate(hailstones):
        if i >= j:
            continue

        x1, y1, _z1 = a.pos
        x2 = x1 + a.vel[0]
        y2 = y1 + a.vel[1]

        x3, y3, _z3 = b.pos
        x4 = x3 + b.vel[0]
        y4 = y3 + b.vel[1]

        if (x1 - x2) * (y3 - y4) == (y1 - y2) * (x3 - x4):
            continue

        if (x1 - x2) * (y3 - y4) == (y1 - y2) * (x3 - x4):
            continue

        t = ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / (
            (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
        )
        u = -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / (
            (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
        )

        if t <= 0 or u <= 0:
            continue

        def in_range(n):
            return 200_000_000_000_000 <= n <= 400_000_000_000_000

        if all(
            map(
                in_range,
                [
                    x1 + a.vel[0] * t,
                    y1 + a.vel[1] * t,
                    x3 + b.vel[0] * u,
                    y3 + b.vel[1] * u,
                ],
            ),
        ):
            ans1 += 1

print(ans1)

# Part 2
import z3

px, py, pz = z3.Ints("px py pz")
vx, vy, vz = z3.Ints("vx vy vz")
ts = [z3.Int(f"t{i}") for i in range(len(hailstones))]
s = z3.Solver()

for t, h in zip(ts, hailstones):
    hpx, hpy, hpz = h.pos
    hvx, hvy, hvz = h.vel

    s.add(t >= 0)
    s.add(hpx + hvx * t == px + vx * t)
    s.add(hpy + hvy * t == py + vy * t)
    s.add(hpz + hvz * t == pz + vz * t)

assert s.check() == z3.sat
m = s.model()

# for i, (t, h) in enumerate(zip(ts, hailstones)):
#     print(f"Hailstone: {h}")
#     print(f"Collision time: {m[t]}")
print(sum([m[p].as_long() for p in [px, py, pz]]))