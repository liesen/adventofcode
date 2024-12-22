from collections.abc import Generator
from itertools import pairwise
from functools import cache
from collections import deque

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
print(numpad)

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
print(keypad)


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


@cache
def dfs_numpad(src: str, dst: str) -> set[str]:
    path: deque[str] = deque()
    seen = set([src])

    def go(src: str) -> Generator[str]:
        if src == dst:
            yield "".join(path)
            return

        for d, x in numpad[src].items():
            if x in seen:
                continue

            seen.add(x)
            path.append(d)
            yield from go(x)
            _ = path.pop()
            seen.remove(x)

    return set(go(src))


if False:
    print(numpad["0"])
    # assert dfs_numpad("0", "2") == {"^"}
    print(bfs_numpad("0", "2"))
    print(bfs_numpad("1", "5"))


    def punch_numpad(s, start="A"):
        ways = {""}

        for src, dst in pairwise(start + s):
            ways = {ss + sss + "A" for ss in ways for sss in bfs_numpad(src, dst)}

        return ways    


    print(punch_numpad("029A", "A"))
    assert punch_numpad("029A") == {"<A^A>^^AvvvA", "<A^A^>^AvvvA", "<A^A^^>AvvvA"}

    def punch_keypad(s, start="A"):
        ways = {""}

        for src, dst in pairwise(start + s):
            ways = {ss + sss + "A" for ss in ways for sss in bfs_keypad(src, dst)}

        return ways    

    for kp in {"<A^A>^^AvvvA", "<A^A^>^AvvvA", "<A^A^^>AvvvA"}:
        print(kp, punch_keypad(kp, "A"), "v<<A>>^A<A>AvA<^AA>A<vAAA>^A" in punch_keypad(kp))
        
codes = """029A
980A
179A
456A
379A""".splitlines()

codes = """129A
540A
789A
596A
582A""".splitlines()

code = "029A"

ans1 = 0

for code in codes:
    S = {""}

    for x0, x1 in pairwise("A" + code):
        S = {s + t + "A" for s in S for t in bfs_numpad(x0, x1)}
    
    assert len({len(s) for s in S}) == 1

    # print(S)
    # assert S == {"<A^A>^^AvvvA", "<A^A^>^AvvvA", "<A^A^^>AvvvA"}

    # Robot 1
    R1: set[str] = set()

    for s in S:
        r1 = {""}

        for x0, x1 in pairwise("A" + s):
            r1 = {r + t + "A" for r in r1 for t in bfs_keypad(x0, x1)}

        R1 |= r1
        
    n = min(len(r) for r in R1)
    R1 = {r for r in R1 if len(r) == n}

    # print(R1)
    # assert "v<<A>>^A<A>AvA<^AA>A<vAAA>^A" in R1

    # Robot 2
    R2: set[str] = set()

    for s in R1:
        r2 = {""}

        for x0, x1 in pairwise("A" + s):
            r2 = {r + t + "A" for r in r2 for t in bfs_keypad(x0, x1)}

        R2 |= r2
        

    n = min(len(r) for r in R2)
    R2 = {r for r in R2 if len(r) == n}
    # assert "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A" in R2
    assert len({len(x) for x in R2}) == 1
    numpart = int("".join(d for d in code if d.isdigit()))

    print(code, numpart, n, numpart * n)
    ans1 += numpart * n
    
print(ans1)