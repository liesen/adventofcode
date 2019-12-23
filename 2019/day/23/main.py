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
done = False

while not done and q:
    cmd = q.popleft()
    dst = cmd[0]
    prog = nics[dst]
    payload = cmd[1:]
    prog.run(payload)

    while prog.outputs:
        if prog.outputs[0] == 255:
            done = True
            ans = prog.outputs[2]
            break

        q.append([prog.outputs.popleft(),
                  prog.outputs.popleft(),
                  prog.outputs.popleft()])

print(ans)