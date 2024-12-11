from math import log10
from functools import cache


@cache
def count(d: int, n: int) -> int:
    if d == 0:
        return 1

    if n == 0:
        return count(d - 1, 1)

    num_digits = int(log10(n) + 1)

    if num_digits % 2 == 0:
        l, r = divmod(n, pow(10, num_digits // 2))
        return count(d - 1, l) + count(d - 1, r)

    return count(d - 1, n * 2024)


assert sum(count(6, n) for n in [125, 17]) == 22
assert sum(count(25, n) for n in [125, 17]) == 55312

stones = [int(n) for n in open(0).read().split()]

# Part 1
print(sum(count(25, n) for n in stones))

# Part 2
print(sum(count(75, n) for n in stones))
