from collections import deque
import re


with open("input.txt") as f:
    lines = [ln.rstrip('\n') for ln in f.readlines()]

maxcol = 9

# Split input into stack lines and rearrangement procedure lines
div = lines.index("")
assert 0 <= div < len(lines)
assert lines[div - 1] == " 1   2   3   4   5   6   7   8   9 "
stack_lines = lines[0:div - 1]
move_lines = lines[div + 1:]

# Transpose stack rows to get stacks
stack_rows = [[ln[i * 3 + i:(i + 1) * 3 + i] for i in range(maxcol)] for ln in stack_lines]
stacks1 = [deque(crate[1] for crate in stack if crate.strip()) for stack in map(list, zip(*stack_rows))]

# Clone stacks for part 2
stacks2 = [deque(stack) for stack in stacks1]

for move in move_lines:
    assert (m := re.match("move (\d+) from (\d+) to (\d+)", move))
    n, src, dst = map(int, m.groups())
    tmp = deque()
    
    for _ in range(n):
        stacks1[dst - 1].appendleft(stacks1[src - 1].popleft())
        tmp.appendleft(stacks2[src - 1].popleft())

    for x in tmp:
        stacks2[dst - 1].appendleft(x)

# Part 1
print("".join(stack[0] for stack in stacks1))

# Part 2
print("".join(stack[0] for stack in stacks2))