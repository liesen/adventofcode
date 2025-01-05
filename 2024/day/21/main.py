from collections import deque
from functools import cache
from itertools import pairwise, product

codes = [ln.rstrip() for ln in open(0)]

# Numeric keypad:
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
dirpad = {
    "^": dict(zip("v>", "vA")),
    "<": dict(zip(">", "v")),
    "v": dict(zip("^<>", "^<>")),
    ">": dict(zip("^<", "Av")),
    "A": dict(zip("<v", "^>")),
}


def bfs_pad(src: str, dst: str, pad: dict[str, dict[str, str]]) -> set[str]:
    """Find all shortest sequences to go from src to dst on a keypad"""
    assert len(src) == 1
    assert len(dst) == 1

    if src == dst:
        return {"A"}

    shortest_sequence = None
    q = deque([(0, src, "")])
    paths: set[str] = set()

    while q:
        n, ch, path = q.popleft()

        if ch == dst:
            shortest_sequence = n
            paths.add(path + "A")
            continue

        for d in "^<v>":
            for ch2 in pad[ch].get(d, ""):
                if shortest_sequence is None or n + 1 < shortest_sequence:
                    q.append((n + 1, ch2, path + d))

    return paths


def bfs_numpad(src: str, dst: str) -> set[str]:
    return bfs_pad(src, dst, numpad)


def bfs_dirpad(src: str, dst: str) -> set[str]:
    return bfs_pad(src, dst, dirpad)


@cache
def count_dirpad_presses(robot_index: int, src: str, dst: str) -> int:
    if robot_index == 1:
        # Last robot before the numeric keypad! Return the (fewest)
        # number of dirpad presses.
        return len(next(iter(bfs_dirpad(src, dst))))

    return min(
        sum(
            count_dirpad_presses(a, b, robot_index - 1)
            for a, b in pairwise("A" + seq)
        )
        for seq in bfs_dirpad(src, dst)
    )


def numpad_seq(seq: str) -> list[str]:
    """Find all shortest sequences for a given code/sequence on a numeric keypad"""
    paths = [
        "".join(subpath)
        for subpath in product(*[bfs_numpad(src, dst) for src, dst in pairwise(seq)])
    ]
    min_len = min(len(path) for path in paths)
    return [path for path in paths if len(path) == min_len]


assert set(numpad_seq("A029A")) == {"<A^A>^^AvvvA", "<A^A^>^AvvvA", "<A^A^^>AvvvA"}


# Part 1 & 2
def get_numeric_part_of_code(code: str) -> int:
    return int(code.rstrip("A"))


for part, num_chained_dirpads in [(1, 2), (2, 25)]:
    print(
        sum(
            get_numeric_part_of_code(code)
            * min(
                sum(
                    count_dirpad_presses(num_chained_dirpads, src, dst)
                    for src, dst in pairwise("A" + dirpad_input)
                )
                for dirpad_input in numpad_seq("A" + code)
            )
            for code in codes
        )
    )
