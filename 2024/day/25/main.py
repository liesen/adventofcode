S = open(0).read()
locks: list[list[int]] = []
keys: list[list[int]] = []

for s in S.split("\n\n"):
    rows = s.splitlines()
    cols = ["".join(x) for x in zip(*rows)]

    if all(x == "#" for x in rows[0]):
        # Lock
        pins = [x.index(".") - 1 for x in cols]
        locks.append(pins)
    else:
        # Key
        assert all(x == "." for x in rows[0])
        pins = [len(x) - x.index("#") - 1 for x in cols]
        keys.append(pins)

ans1 = 0

for lock in locks:
    for key in keys:
        ans1 += all(l + k <= 5 for l, k in zip(lock, key))

print(ans1)
