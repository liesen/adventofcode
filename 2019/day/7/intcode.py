from collections import deque


class Intcode:
    def __init__(self, prog, inputs=[]):
        self.prog = prog
        self.pc = 0
        self.mem = list(prog)
        self.halt = False
        self.inputs = deque(inputs)
        self.outputs = deque()

    def reset(self):
        self.pc = 0
        self.halt = False
        self.mem = list(self.prog)

    def run(self, inputs=[]):
        self.inputs.extend(inputs)

        while not (self.halt or self.need_input):
            self.step()

    def step(self):
        def param(i):
            mode = (self.mem[self.pc] // (10 * 10**i)) % 10

            if mode == 0:
                return self.mem[self.mem[self.pc + i]]
            elif mode == 1:
                return self.mem[self.pc + i]

            raise Exception("bad parameter mode")

        if not self.halt:
            opcode = self.mem[self.pc] % 100

            if opcode == 1:
                self.mem[self.mem[self.pc + 3]] = param(1) + param(2)
                self.pc += 4
            elif opcode == 2:
                self.mem[self.mem[self.pc + 3]] = param(1) * param(2)
                self.pc += 4
            elif opcode == 3:
                if len(self.inputs) > 0:
                    self.mem[self.mem[self.pc + 1]] = self.inputs.popleft()
                    self.pc += 2
            elif opcode == 4:
                self.outputs.append(param(1))
                self.pc += 2
            elif opcode == 5:
                if param(1) == 0:
                    self.pc += 3
                else:
                    self.pc = param(2)
            elif opcode == 6:
                if param(1) == 0:
                    self.pc = param(2)
                else:
                    self.pc += 3
            elif opcode == 7:
                self.mem[self.mem[self.pc + 3]] = 1 if param(1) < param(2) else 0
                self.pc += 4
            elif opcode == 8:
                self.mem[self.mem[self.pc + 3]] = 1 if param(1) == param(2) else 0
                self.pc += 4
            elif opcode == 99:
                self.halt = True
            else:
                raise Exception(f"unknown opcode: {opcode} at pc = {self.pc}")

    @property
    def need_input(self):
        return self.mem[self.pc] % 100 == 3 and len(self.inputs) == 0
