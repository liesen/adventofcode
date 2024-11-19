import fileinput
from dataclasses import dataclass


@dataclass(frozen=True)
class Hailstone:
    pos: tuple[int, int, int]
    vel: tuple[int, int, int]


hailstones: list[Hailstone] = []

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

        px1, py1, pz1 = a.pos
        vx1, vy1, vz1 = a.vel
        px2 = px1 + vx1
        py2 = py1 + vy1

        px3, py3, pz3 = b.pos
        vx3, vy3, vz3 = b.vel
        px4 = px3 + vx3
        py4 = py3 + vy3

        denominator = (px1 - px2) * (py3 - py4) - (py1 - py2) * (px3 - px4)

        if denominator == 0:
            continue

        t = ((px1 - px3) * (py3 - py4) - (py1 - py3) * (px3 - px4)) / denominator
        u = -((px1 - px2) * (py1 - py3) - (py1 - py2) * (px1 - px3)) / denominator

        if t <= 0 or u <= 0:
            continue

        def in_range(n: float) -> bool:
            return 200_000_000_000_000 <= n <= 400_000_000_000_000

        if all(
            map(
                in_range,
                [
                    px1 + a.vel[0] * t,
                    py1 + a.vel[1] * t,
                    px3 + b.vel[0] * u,
                    py3 + b.vel[1] * u,
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
