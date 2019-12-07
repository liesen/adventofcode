from collections import deque, namedtuple
from itertools import product

with open('input.txt') as fp:
    prog = [int(i) for line in fp for i in line.split(',')]

# prog = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
# prog = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]

# Part 1
max_thruster_signal = 0

def run(prog, phase_settings):
    input_signal = 0

    for phase_setting in phase_settings:
        pc = 0
        halt = False
        mem = list(prog)
        inp = iter([phase_setting, input_signal])
        outp = deque()

        def param(i):
            mode = (mem[pc] // (10 * 10**i)) % 10

            if mode == 0:
                return mem[mem[pc + i]]
            elif mode == 1:
                return mem[pc + i]

            raise Exception("bad parameter mode")

        while not halt:
            opcode = mem[pc] % 100
            # print(f'pc={pc}, opcode={opcode}, mem={mem}')

            if opcode == 1:
                mem[mem[pc + 3]] = param(1) + param(2)
                pc += 4
            elif opcode == 2:
                mem[mem[pc + 3]] = param(1) * param(2)
                pc += 4
            elif opcode == 3:
                mem[mem[pc + 1]] = next(inp)
                pc += 2
            elif opcode == 4:
                outp.append(param(1))
                pc += 2
            elif opcode == 5:
                if param(1) == 0:
                    pc += 3
                else:
                    pc = param(2)
            elif opcode == 6:
                if param(1) == 0:
                    pc = param(2)
                else:
                    pc += 3
            elif opcode == 7:
                mem[mem[pc + 3]] = 1 if param(1) < param(2) else 0
                pc += 4
            elif opcode == 8:
                mem[mem[pc + 3]] = 1 if param(1) == param(2) else 0
                pc += 4
            elif opcode == 99:
                halt = True
            else:
                raise Exception(f'unknown opcode: {opcode} at pc = {pc}')

        input_signal = output_signal = outp.pop()

    return output_signal


max_thruster_signal = 0

for a in range(5):
    for b in range(5):
        if a == b:
            continue

        for c in range(5):
            if a == c or b == c:
                continue
        
            for d in range(5):
                if a == d or b == d or c == d:
                    continue
                
                for e in range(5):
                    if a == e or b == e or c == e or d == e:
                        continue

                    thruster_signal = run(prog, [a, b, c, d, e])

                    if thruster_signal > max_thruster_signal:
                        max_thruster_signal = thruster_signal

print(max_thruster_signal)