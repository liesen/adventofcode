from collections import deque
import fileinput
from dataclasses import dataclass

@dataclass
class Op:
    name: str
    arg1: int

program = []

for line in fileinput.input('input.txt'):
    opname, arg = line.strip().split()
    program.append(Op(opname, int(arg)))

# Part 1
ip = 0
seen = set()
acc = 0

while True:
    if ip in seen:
        print(acc)
        break

    op = program[ip]
    seen.add(ip)
    
    if op.name == 'acc':
        acc += op.arg1
        ip += 1
    elif op.name == 'jmp':
        ip += op.arg1
    elif op.name == 'nop':
        ip += 1

# Part 2
halt = False

while not halt:
    for i, op in enumerate(program):
        tmp = op

        if op.name == 'jmp':
            program[i] = Op('nop', op.arg1)
        elif op.name == 'nop':
            program[i] = Op('nop', op.arg1)

        halt = False
        ip = 0
        seen = set()
        acc = 0

        while True:
            if ip == len(program):
                halt = True
                break

            if ip in seen:
                break

            op = program[ip]
            seen.add(ip)
            
            if op.name == 'acc':
                acc += op.arg1
                ip += 1
            elif op.name == 'jmp':
                ip += op.arg1
            elif op.name == 'nop':
                ip += 1

        program[i] = tmp

        if halt:
            print(acc)
            break