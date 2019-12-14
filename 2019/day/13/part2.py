from collections import deque
import curses
from intcode import Intcode
from save import save

with open('input.txt') as fp:
    prog = Intcode([int(x) for ln in fp for x in ln.split(',')])

# From part 1
smaxrow = 22
smaxcol = 43

# Set memory addr 0 to 2 to play for free
prog.mem[0] = 2

tilemap = {0: ' ', 1: chr(0x2588), 2: chr(0x2591), 3: chr(0x2594), 4: chr(0x25EF)}

def main(screen):
    global prog, smaxrow, smaxcol, tilemap

    screen.refresh()
    score = 0

    inputs = deque(save)
    game_inputs = deque()

    while not prog.halt:
        prog.run()

        while len(prog.outputs) > 0:
            x, y, z = prog.outputs.popleft(), prog.outputs.popleft(), prog.outputs.popleft()

            if x == -1 and y == 0:
                score = z
            else:
                screen.addch(y, x, tilemap[z])

        screen.addstr(smaxrow + 1, 0, f'SCORE: {score}')
        screen.refresh()

        # Handle input
        if prog.need_input:
            prog.inputs.clear()

            if len(inputs) > 0:  # exhaust saved game
                x = inputs.popleft()
            else:
                x = screen.getch()
                save.append(x)

            if x == curses.KEY_ENTER:
                return
            elif x == curses.KEY_DOWN:
                prog.inputs.append(0)
                game_inputs.append(0)
            elif x == curses.KEY_LEFT:
                prog.inputs.append(-1)
                game_inputs.append(-1)
            elif x == curses.KEY_RIGHT:
                prog.inputs.append(1)
                game_inputs.append(1)

    assert prog.halt

    screen.addstr(smaxrow + 2, 0, f'SAVE? ')
    screen.refresh()

    if screen.getch(smaxrow + 2, len('SAVE? ')) == ord('y'):
        with open('save.py', 'w') as fp:
            fp.write('''from collections import deque

save = deque(''')
            fp.write(str(list(save)))
            fp.write(')')
    
curses.wrapper(main)