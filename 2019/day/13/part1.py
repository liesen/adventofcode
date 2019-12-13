from intcode import Intcode

with open('input.txt') as fp:
    prog = Intcode([int(x) for ln in fp for x in ln.split(',')])

prog.run()
num_blocks = 0

while len(prog.outputs) > 0:
    x, y, z = prog.outputs.popleft(), prog.outputs.popleft(), prog.outputs.popleft()
    if z == 2:
        num_blocks += 1

# Part 1
print(num_blocks)