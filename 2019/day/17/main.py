from collections import deque
from itertools import chain
from intcode import Intcode
import numpy as np


with open('input.txt') as fp:
    prog = Intcode([int(x) for ln in fp for x in ln.split(',')])

prog.run()
s = ''.join(map(chr, prog.outputs))
# print(s)

width = prog.outputs.index(10) + 1
height = len(prog.outputs) // width

WALL = '#'
ROBOT = '^'  # By inspection

for y in range(height):
    for x in range(width):
        if s[y * width + x] == ROBOT:
            robot_pos = x, y

# Part 1
# horizontal lines
hhh = {}

for y in range(height):
    i = 0

    while i < width:
        j = i
        
        while j < width and s[y * width + j] in [WALL, ROBOT]:
            j += 1

        if j > i + 1:
            hhh.setdefault(y, []).append((i, j - 1))

        i = j + 1

# vertical lines
vvv = {}

for x in range(width):
    i = 0

    while i < height:
        j = i
        
        while j < height and s[j * width + x] in [WALL, ROBOT]:
            j += 1

        if j > i + 1:
            vvv.setdefault(x, []).append((i, j - 1))

        i = j + 1

ans =  0

for y, xs in hhh.items():  # each row
    for x0, x1 in xs:  # each horizontal line on row
        for x, ys in vvv.items():  # each col
            if not x0 < x < x1:  # col inside horizontal line
                continue
            
            for y0, y1 in ys:  # each vertical line on col
                if y0 < y < y1:  # row inside vertical line 
                    ans += x * y

print(ans)

# Part 2
rx, ry = robot_pos
dx, dy = 0, -1  # Robot direction
commands = []

while (dx == 0 and ry in hhh) or (dy == 0 and rx in vvv):
    if dx == 0:  # moving horizontally
        for x0, x1 in hhh[ry]:
            if rx == x0:
                if dy == 1:
                    commands.append(f'L{x0 - x1}')
                else:
                    commands.append(f'R{x1 - x0}')

                dx, dy = 1, 0
                rx = x1
            elif rx == x1:
                if dy == 1:
                    commands.append(f'R{x1 - x0}')
                else:
                    commands.append(f'L{x1 - x0}')

                dx, dy = -1, 0
                rx = x0
    else:  # moving vertically
        for y0, y1 in vvv[rx]:
            if ry == y0:
                if dx == 1:
                    commands.append(f'R{y1 - y0}')
                else:
                    commands.append(f'L{y1 - y0}')

                dx, dy = 0, 1
                ry = y1
            elif ry == y1:
                if dx == 1:
                    commands.append(f'L{y1 - y0}')
                else:
                    commands.append(f'R{y1 - y0}')
                    
                dx, dy = 0, -1
                ry = y0

# This was done by hand
funs = {
    'A': ['R12', 'R4', 'R10', 'R12'],
    'B': ['R6', 'L8', 'R10'],
    'C': ['L8', 'R4', 'R4', 'R6']
}

main_fun = 'ABACABCABC'
assert commands == list(chain(*[funs[f] for f in main_fun]))

prog.reset()
assert prog.mem[0] == 1
prog.mem[0] = 2
prog.run([ord(z) for z in ','.join(main_fun) + chr(10)])
assert prog.need_input
prog.run([ord(z) for z in ','.join(f'{x[0]},{x[1:]}' for x in funs['A']) + chr(10)])
assert prog.need_input
prog.run([ord(z) for z in ','.join(f'{x[0]},{x[1:]}' for x in funs['B']) + chr(10)])
assert prog.need_input
prog.run([ord(z) for z in ','.join(f'{x[0]},{x[1:]}' for x in funs['C']) + chr(10)])
assert prog.need_input
prog.run([ord('n'), 10])
assert prog.halt
score = prog.outputs.pop()

# s = ''.join(map(chr, prog.outputs))
# print(s)

print(score)