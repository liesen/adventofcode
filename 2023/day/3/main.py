import math
import fileinput
import re
from dataclasses import dataclass

with fileinput.input("input") as f:
    S = [ln.rstrip() for ln in f]

symbols = {x for ln in S for x in ln if not (x.isdigit() or x == ".")}


@dataclass(frozen=True)
class Num:
    row: int
    col: int
    len: int
    num: int


N = {
    Num(row, m.start(), m.end() - m.start(), int(m.group(0)))
    for row, s in enumerate(S)
    for m in re.finditer("\d+", s)
}


def adjacent(row, col, len):
    for c in range(col - 1, col + len + 1):
        yield (row - 1, c)  # Above
        yield (row + 1, c)  # Below

    # Same row
    yield (row, col - 1)
    yield (row, col + len)


def in_range(row, col):
    return row >= 0 and row < len(S) and col >= 0 and col < len(S[row])


# Part 1
# Filter out numbers with an adjacent symbol
part_numbers = {
    n
    for n in N
    if any(
        S[r][c] in symbols for (r, c) in adjacent(n.row, n.col, n.len) if in_range(r, c)
    )
}
print(sum(n.num for n in part_numbers))

# Part 2
E = {}  # Neighbor numbers for each gear

for n in part_numbers:
    for r, c in adjacent(n.row, n.col, n.len):
        if in_range(r, c) and S[r][c] == "*":  # Gear at (r, c)
            E.setdefault((r, c), set()).add(n)

ans2 = 0

for nums in E.values():
    if len(nums) == 2:
        ans2 += math.prod(n.num for n in nums)

print(ans2)
