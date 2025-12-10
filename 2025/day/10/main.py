# /// script
# dependencies = [
#   "z3-solver"
# ]
# ///
import heapq
from dataclasses import dataclass

import z3

input = """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"""
lines = input.splitlines()


with open(0) as f:
    lines = [ln.rstrip() for ln in f]


@dataclass(frozen=True)
class Manual:
    light_diagram: list[bool]
    buttons: list[list[int]]
    joltage_requirements: list[int]


def parse_manual(s: str) -> Manual:
    light_diagram, *rest = s.split(" ")
    light_diagram = [x == "#" for x in light_diagram[1:-1]]
    buttons = [[int(x) for x in button[1:-1].split(",")] for button in rest[:-1]]
    joltage_requirements = [int(x) for x in rest[-1][1:-1].split(",")]
    return Manual(light_diagram, buttons, joltage_requirements)


def configure_indicator_lights(manual: Manual) -> int:
    # Convert to bitmask representation
    target_mask = sum(1 << i for i, b in enumerate(manual.light_diagram) if b)
    button_masks = [sum(1 << i for i in button) for button in manual.buttons]

    # Bfs
    s0 = 0
    q = [(0, s0)]
    heapq.heapify(q)
    seen = {s0}

    while q:
        n, s = heapq.heappop(q)

        if s == target_mask:
            return n

        for button in button_masks:
            t = s ^ button

            if t not in seen:
                seen.add(t)
                heapq.heappush(q, (n + 1, t))

    raise Exception("no solution")


def find_joltage_levels(manual: Manual) -> int:
    solver = z3.Optimize()
    joltage_levels = z3.IntVector("j", len(manual.joltage_requirements))
    button_presses = z3.IntVector("b", len(manual.buttons))

    # Must press each button non-negative times
    for num_presses in button_presses:
        solver.add(num_presses >= 0)

    for ji, (joltage_level, joltage_requirement) in enumerate(
        zip(joltage_levels, manual.joltage_requirements)
    ):
        # Must meet the joltage requirement
        solver.add(joltage_level == joltage_requirement)

        # Find buttons that affect this joltage level
        solver.add(
            joltage_level
            == sum(
                button_presses[bi]
                for bi, button in enumerate(manual.buttons)
                if ji in button
            )
        )

    total_button_presses = z3.Int("ans")
    solver.add(total_button_presses == z3.Sum(button_presses))
    solver.minimize(total_button_presses)
    assert solver.check() == z3.sat
    return solver.model()[total_button_presses].as_long()


ans1 = 0
ans2 = 0

for ln in lines:
    manual = parse_manual(ln)
    ans1 += configure_indicator_lights(manual)
    ans2 += find_joltage_levels(manual)


print(ans1)
print(ans2)
