import numpy as np
from intcode import Intcode

with open('input.txt') as fp:
    code = [int(x) for ln in fp for x in ln.split(',')]

# Part 1
prog = Intcode(code)
p, a = np.array([0, 0]), np.array([0, -1])
panels = {}

while not prog.halt:
    color = panels.get(tuple(p), 0)
    prog.inputs.append(color)
    prog.run()
    assert len(prog.outputs) == 2

    new_color = prog.outputs.popleft()
    panels[tuple(p)] = new_color

    turn = prog.outputs.popleft()
    
    if turn == 0:  # left
        a = np.matmul(a, np.array([[0, -1], [1, 0]]))
    elif turn == 1:  # right
        a = np.matmul(a, np.array([[0, 1], [-1, 0]]))
    else:
        raise Exception(f'illegal turn: {turn}')

    # move
    p += a

print(len(panels))