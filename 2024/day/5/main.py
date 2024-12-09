from itertools import pairwise
from functools import cmp_to_key

with open("input") as f:
    S = [ln.rstrip() for ln in f]

divider = S.index("")
rules = set(S[:divider])


def cmp(p1: str, p2: str):
    if f"{p1}|{p2}" in rules:
        return -1

    if f"{p2}|{p1}" in rules:
        return 1

    raise Exception("unreachable")


ans1 = 0
ans2 = 0

for pgstr in S[divider + 1 :]:
    pgnos = pgstr.split(",")
    mid = len(pgnos) // 2

    if all(f"{p1}|{p2}" in rules for p1, p2 in pairwise(pgnos)):
        ans1 += int(pgnos[mid])
    else:
        ordered_pgnos = sorted(pgnos, key=cmp_to_key(cmp))
        ans2 += int(ordered_pgnos[mid])

print(ans1)
print(ans2)
