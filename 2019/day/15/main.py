from intcode import Intcode
import curses
import numpy as np
from collections import deque

with open('input.txt') as fp:
    prog = Intcode([int(x) for ln in fp for x in ln.split(',')])

D = {
    1: np.array([0, -1]),
    2: np.array([0, 1]),
    3: np.array([-1, 0]),
    4: np.array([1, 0])
}

R = {1: 2, 2: 1, 3: 4, 4: 3}
minrow = 0
mincol = 0
maxrow = 0
maxcol = 0

def search(prog, pos, field, screen):
    global minrow, mincol, maxrow, maxcol

    x, y = pos

    if not screen:
        print(f'> {x},{y}, HALT: {prog.halt}, INPUT: {prog.need_input}, VISITED: {(x, y) in field}, #VISITED: {len(field)}')

    if prog.halt:
        return prog

    # if (x, y) in field:
    #     return

    # if x < 0 or y < 0:
    #    return

    # print(f'{x},{y}')

    if screen:
        screen.addstr(0, 0, f'{x},{y}')
        screen.addch(y + 1, x, 'D')
        screen.refresh()

    minrow = min(minrow, y)
    maxrow = max(maxrow, y)
    mincol = min(mincol, x)
    maxcol = max(maxcol, x)

    # key = screen.getch()

    #    if key == curses.KEY_UP:
    #        command = 1
    #    elif key == curses.KEY_DOWN:
    #        command = 2
    #    elif key == curses.KEY_LEFT:
    #        command = 3
    #    elif key == curses.KEY_RIGHT:
    #        command = 4

    for command in range(1, 5):
        if screen: screen.getch()
        d = D[command]
        x1, y1 = pos1 = pos + d

        if (x1, y1) in field:
            continue

        prog.inputs.append(command)
        prog.run()
        status = prog.outputs.pop()
        field[(x1, y1)] = status
        # print('new pos', pos1, 'status', status)

        if status == 0:
            if screen: screen.addch(y1 + 1, x1, '#')
            if not screen: print(f'{x1},{y1}: #')
        elif status == 1:
            if screen: screen.addch(y1 + 1, x1, '.')
            if not screen: print(f'{x1},{y1}: .')
            # print('RECURSE MF!')
            search(prog, pos1, field, screen)
            # input('BACK')
            prog.inputs.append(R[command])
            prog.run()
        elif status == 2:
            if screen: screen.addch(y1 + 1, x1, 'X')
            if not screen: print(f'{x1},{y1}: X')
            search(prog, pos1, field, screen)
            prog.inputs.append(R[command])
            prog.run()
        
        if screen: screen.refresh()



def main(screen):
    global prog
    print(screen)
    prog.run()
    print(prog.outputs)
    screen.refresh()
    
    field = {}
    search(prog, np.array([0, 0]), field, screen)
    screen.getkey()


# curses.wrapper(main)

field = {}
search(prog, np.array([0, 0]), field, None)
print(field)
print((mincol, minrow), (maxcol, maxrow))

tilemap = {
    -1: ' ',
    0: chr(0x2588),
    1: '.',
    2: 'X'
}

O, = [p for p, z in field.items() if z == 2]

print('\n'.join([''.join([tilemap[field.get((x, y), -1)] for x in range(mincol, maxcol + 1)]) for y in range(minrow, maxrow + 1)]))

def neighbors(pos, field):
    for d in D.values():
        x1, y1 = pos + d

        if field.get((x1, y1), -1) > 0:
            yield (x1, y1)

# BFS = shortest
q = deque([])
q.append(((0, 0), deque([])))
visited = set()

sp = {}

while q:
    pos, path = q.popleft()
    x, y = pos
    sp[pos] = len(path)

    if pos in visited:
        continue

    visited.add(pos)

    if field.get((x, y), -1) == 2:
        print(path)
        print(len(path))
        break

    for n in neighbors(pos, field):
        q.append((n, path + deque([pos])))

print(O)