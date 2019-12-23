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
while q:
    cmd = q.popleft()
    dst = cmd[0]

    if dst == 255:
        nat_packet = cmd
        ans = cmd[2]
        continue

    prog = nics[dst]
    payload = cmd[1:]
    prog.run(payload)

    while prog.outputs:
        q.append([prog.outputs.popleft(),
                  prog.outputs.popleft(),
                  prog.outputs.popleft()])

print(ans)

# Part 2
ans = None
nat_ys = set()

while True:
    if not q:
        nat_x, nat_y = nat_packet[1:]

        if nat_y in nat_ys:
            ans = nat_y
            break
        
        nat_ys.add(nat_y)
        q.append([0, nat_x, nat_y])

    while q:
        cmd = q.popleft()
        dst = cmd[0]

        if dst == 255:
            nat_packet = cmd
            continue

        prog = nics[dst]
        payload = cmd[1:]
        prog.run(payload)

        while prog.outputs:
            q.append([prog.outputs.popleft(),
                    prog.outputs.popleft(),
                    prog.outputs.popleft()])

print(ans)