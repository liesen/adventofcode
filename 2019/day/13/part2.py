from collections import deque
import curses
from intcode import Intcode

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

    paddle_x = 0
    ball_x = 0

    game_inputs = deque()

    while not prog.halt:
        prog.run()

        while len(prog.outputs) > 0:
            x, y, z = prog.outputs.popleft(), prog.outputs.popleft(), prog.outputs.popleft()

            if x == -1 and y == 0:
                score = z
            else:
                if z == 3:
                    paddle_x = x

                if z == 4:
                    ball_x = x
                
                screen.addch(y, x, tilemap[z])

        screen.addstr(smaxrow + 1, 0, f'SCORE: {score}')
        screen.refresh()

        # Handle input
        if prog.need_input:
            prog.inputs.clear()

            # Match paddle position with the ball
            if paddle_x < ball_x:
                prog.inputs.append(1)
            elif paddle_x > ball_x:
                prog.inputs.append(-1)
            else:
                prog.inputs.append(0)

    assert prog.halt
    screen.refresh()
    screen.getch()
    
curses.wrapper(main)