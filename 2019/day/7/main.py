from collections import deque, namedtuple
from itertools import permutations

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


# Part 1
max_thruster_signal = 0

for phases in permutations(range(5)):
    thruster_signal = run(prog, phases)

    if thruster_signal > max_thruster_signal:
        max_thruster_signal = thruster_signal

print(max_thruster_signal)

# Part 2
prog = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]