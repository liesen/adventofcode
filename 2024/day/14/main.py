import math
import re

xmax = 101
ymax = 103

Robot = tuple[int, int, int, int]
robots: list[Robot] = []

with open("input") as f:
    for ln in f:
        px, py, vx, vy = map(int, re.findall(r"-?\d+", ln))
        robots.append((px, py, vx, vy))

quadrants = {(0, 0): 0, (0, 1): 0, (1, 0): 0, (1, 1): 0}

for px, py, vx, vy in robots:
    px2 = (px + vx * 100) % xmax
    py2 = (py + vy * 100) % ymax

    if not (px2 == xmax // 2 or py2 == ymax // 2):
        quadrants[(px2 < xmax // 2, py2 < ymax // 2)] += 1

ans1 = math.prod(quadrants.values())
print(ans1)

# Part 2
def draw(t: int):
    rs = {((px + vx * t) % xmax, (py + vy * t) % ymax) for px, py, vx, vy in robots}

    for y in range(ymax):
        for x in range(xmax):
            ch = "|" if x == xmax // 2 else "."
            print(ch if (x, y) not in rs else "#", end="")

        print()

frames: dict[int, list[tuple[int, int]]] = {}

for i, (px, py, vx, vy) in enumerate(robots):
    seen: set[tuple[int, int]] = set()
    t = 0
    px0 = px
    py0 = py

    while (px, py) not in seen:
        seen.add((px, py))
        frames.setdefault(t, []).append((px, py))
        t += 1
        px = (px0 + vx * t) % xmax
        py = (py0 + vy * t) % ymax

    assert (px0, py0) == ((px0 + vx * t) % xmax, (py0 + vy * t) % ymax)

def sumdist(t: int) -> int:
    # sum manhattan distance to center for all robots
    return sum(abs(px - xmax // 2) + abs(py - ymax // 2) for px, py in frames[t])

ans2 = min(frames, key=sumdist)
print(ans2)
