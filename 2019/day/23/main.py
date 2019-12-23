from intcode import Intcode
from collections import deque


with open('input.txt') as fp:
    code = [int(x) for x in fp.read().split(',')]


q = deque()
nics = {}

for i in range(50):
    prog = Intcode(code)
    nics[i] = prog
    prog.run([i, -1])

    while prog.outputs:
        q.append([prog.outputs.popleft(),
                  prog.outputs.popleft(),
                  prog.outputs.popleft()])

# Part 1
while q and not q[0][0] == 255:
    cmd = q.popleft()
    dst = cmd[0]
    prog = nics[dst]
    payload = cmd[1:]
    prog.run(payload)

    while prog.outputs:
        q.append([prog.outputs.popleft(),
                  prog.outputs.popleft(),
                  prog.outputs.popleft()])

print(q.popleft()[2])

# Part 2