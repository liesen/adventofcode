from intcode import Intcode
import numpy as np

with open('input.txt') as fp:
    prog = Intcode([int(x) for ln in fp for x in ln.split(',')])

prog.run()
s = ''.join(map(chr, prog.outputs))
# print(s)

width = prog.outputs.index(10) + 1
height = len(prog.outputs) // width

# Part 1

# horizontal lines
hh = []

for row in range(height):
    i = 0

    while i < width:
        j = i
        
        while j < width and s[row * width + j] == '#':
            j += 1

        if j > i + 1:
            hh.append((row, (i, j - 1)))

        i = j + 1

# vertical lines
vv = []

for col in range(width):
    i = 0

    while i < height:
        j = i
        
        while j < height and s[j * width + col] == '#':
            j += 1

        if j > i + 1:
            vv.append((col, (i, j)))

        i = j + 1

ans = 0

for hy, (vx0, vx1) in hh:
    for vx, (hy0, hy1) in vv:
        if hy0 < hy < hy1 and vx0 < vx < vx1:
            ans += vx * hy

print(ans)
