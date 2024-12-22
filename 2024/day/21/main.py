from collections import deque
from functools import cache
from itertools import pairwise

codes = [ln.rstrip() for ln in open(0)]

# Numpad:
#
# +---+---+---+
# | 7 | 8 | 9 |
# +---+---+---+
# | 4 | 5 | 6 |
# +---+---+---+
# | 1 | 2 | 3 |
# +---+---+---+
#     | 0 | A |
#     +---+---+
numpad = {
    "0": dict(zip("^>", "2A")),
    "1": dict(zip("^>", "42")),
    "2": dict(zip("^<v>", "5103")),
    "3": dict(zip("^<v", "62A")),
    "4": dict(zip("^v>", "715")),
    "5": dict(zip("^<v>", "8426")),
    "6": dict(zip("^<v", "953")),
    "7": dict(zip("v>", "48")),
    "8": dict(zip("<v>", "759")),
    "9": dict(zip("<v", "86")),
    "A": dict(zip("^<", "30")),
}

# Directional keypad:
#
#     +---+---+
#     | ^ | A |
# +---+---+---+
# | < | v | > |
# +---+---+---+
keypad = {
    "^": dict(zip("v>", "vA")),
    "<": dict(zip(">", "v")),
    "v": dict(zip("^<>", "^<>")),
    ">": dict(zip("^<", "Av")),
    "A": dict(zip("<v", "^>")),
}


@cache
def bfs_keypad(src: str, dst: str) -> set[str]:
    shortest_sequence = None
    q = deque([(0, src, "")])
    seen = set([(src, "")])
    paths: set[str] = set()

    while q:
        n, ch, path = q.popleft()

        if ch == dst:
            if shortest_sequence is None:
                shortest_sequence = n
            paths.add(path)
            continue

        for d in "^<v>":
            for ch2 in keypad[ch].get(d, ""):
                if (ch2, path + d) not in seen and (
                    shortest_sequence is None or n + 1 < shortest_sequence
                ):
                    q.append((n + 1, ch2, path + d))

    return paths


@cache
def bfs_numpad(src: str, dst: str) -> set[str]:
    shortest_sequence = None
    q = deque([(0, src, "")])
    seen = set([(src, "")])
    paths: set[str] = set()

    while q:
        n, ch, path = q.popleft()

        if ch == dst:
            if shortest_sequence is None:
                shortest_sequence = n
            paths.add(path)
            continue

        for d in "^<v>":
            for ch2 in numpad[ch].get(d, ""):
                if (ch2, path + d) not in seen and (
                    shortest_sequence is None or n + 1 < shortest_sequence
                ):
                    q.append((n + 1, ch2, path + d))

    return paths


ans1 = 0
num_robots = 2

for code in codes:
    S = {""}

    for x0, x1 in pairwise("A" + code):
        S = {s + t + "A" for s in S for t in bfs_numpad(x0, x1)}

    assert len({len(s) for s in S}) == 1
    assert code != "029A" or (S == {"<A^A>^^AvvvA", "<A^A^>^AvvvA", "<A^A^^>AvvvA"})

    for irobot in range(num_robots):
        R: set[str] = set()

        for s in S:
            r1 = {""}

            for x0, x1 in pairwise("A" + s):
                r1 = {r + t + "A" for r in r1 for t in bfs_keypad(x0, x1)}

            R |= r1

        n = min(len(r) for r in R)
        S = {r for r in R if len(r) == n}

        if code == "029A":
            if irobot == 0:
                assert "v<<A>>^A<A>AvA<^AA>A<vAAA>^A" in S
            if irobot == 1:
                assert (
                    "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A"
                    in S
                )

    n = min(len(r) for r in S)
    numpart = int("".join(d for d in code if d.isdigit()))
    ans1 += numpart * n

print(ans1)
