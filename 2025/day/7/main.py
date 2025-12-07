from functools import cache


input = """.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
..............."""

# Read lines, strip "void" lines
def void(line):
    return all(c == "." for c in line)

lines = [line for line in input.splitlines() if not void(line)]

with open(0) as f:
    lines = [line.rstrip("\n") for line in f if not void(line.rstrip("\n"))]

S = next(i for i, c in enumerate(lines[0]) if c == "S")

# Parse each line to find splitters
splitters_per_line = [{i for i, c in enumerate(line) if c == "^"} for line in lines[1:]]

# Part 1
beams = {S}
num_splits = 0

for splitters in splitters_per_line:
    splitting_beams = beams & splitters
    passthrough_beams = beams - splitters
    split_beams = {x + dx for x in splitting_beams for dx in [-1, 1]}
    beams = passthrough_beams | split_beams
    num_splits += len(splitting_beams)

print(num_splits)

# Part 2
@cache
def dfs(line_no, beam_x):
    if line_no >= len(lines):
        return 0
    
    if line_no == len(lines) - 1:
        return 1

    if beam_x in splitters_per_line[line_no]:
        return dfs(line_no + 1, beam_x - 1) + dfs(line_no + 1, beam_x + 1)
    else:
        return dfs(line_no + 1, beam_x)

print(dfs(0, S))