from intcode import Intcode
from collections import deque
import curses
from save import save

with open('input.txt') as fp:
    code = [int(x) for ln in fp for x in ln.split(',')]

prog = Intcode(code)
prog.run()

field = {}

while len(prog.outputs) > 0:
    x, y, z = prog.outputs.popleft(), prog.outputs.popleft(), prog.outputs.popleft()
    field[(x, y)] = z

# Part 1
print(len([z for k, z in field.items() if z == 2]))

# Part 2
max_x = max([x for x, y in field.keys()])
max_y = max([y for x, y in field.keys()])

tiles = {0: ' ', 1: 'W', 2: '#', 3: '-', 4: 'o'}

screen = curses.initscr()
curses.cbreak()
screen.keypad(True)
smaxcol = pad_x = max_x + 1
smaxrow = pad_y = 1 + max_y + 2
pad = curses.newpad(pad_y, pad_x)
screen.refresh()

field = {}

prog.reset()
prog.mem[0] = 2

block_count = 0
score = 0
inputs = deque(save)
game_inputs = deque()

while not prog.halt:
    prog.run()

    block_count = 0

    while len(prog.outputs) > 0:
        x, y, z = prog.outputs.popleft(), prog.outputs.popleft(), prog.outputs.popleft()

        if x == -1 and y == 0:
            score = z
        else:
            field[(x, y)] = z

            if z == 2:
                block_count += 1

            pad.addch(y + 1, x, tiles[z])

    pad.addstr(0, 0, f'SCORE: {score}, BLOCK_COUNT: {block_count}')
    pad.refresh(0, 0, 0, 0, pad_y, pad_x)

    # Handle input
    if prog.need_input:
        prog.inputs.clear()

        if len(inputs) > 0:  # exhaust saved game
            x = inputs.popleft()
        else:
            x = screen.getch()
            save.append(x)

        if x == 10:  # enter
            break
        elif x == 258:  # d arrow
            prog.inputs.append(0)
            game_inputs.append(0)
        elif x == 260:  # l arrow
            prog.inputs.append(-1)
            game_inputs.append(-1)
        elif x == 261:  # r arrow
            prog.inputs.append(1)
            game_inputs.append(1)

assert prog.halt

pad.addstr(max_y + 2, 0, f'SAVE? ')
pad.refresh(0, 0, 0, 0, pad_y, pad_x)

if screen.getch(max_y + 1, len('SAVE? ')) == ord('y'):
    with open('save.py', 'w') as fp:
        fp.write('''from collections import deque

save = deque(''')
        fp.write(str(list(save)))
        fp.write(')')
