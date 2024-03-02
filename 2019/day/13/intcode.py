from collections import defaultdict, deque


class Intcode:
    def __init__(self, prog, inputs=[]):
        self.prog = prog
        self.pc = 0
        self.mem = list(prog)
        self.halt = False
        self.inputs = deque(inputs)
        self.outputs = deque()
        self.relative_base = 0
        self.xmem = defaultdict(int)

    def reset(self):
        self.pc = 0
        self.halt = False
        self.mem = list(self.prog)

    def run(self, inputs=[]):
        self.inputs.extend(inputs)

        while not (self.halt or self.need_input):
            self.step()

    def read(self, addr):
        if addr < 0:
            raise Exception("invalid memory access to negative address")

        if addr < len(self.mem):
            return self.mem[addr]
        else:
            return self.xmem[addr]

    def write(self, addr, value):
        if addr < len(self.mem):
            self.mem[addr] = value
        else:
            self.xmem[addr] = value

    def step(self):
        if self.halt:
            raise Exception("halted")

        if self.need_input:
            raise Exception("need input")

        def param(i):
            mode = (self.mem[self.pc] // (10 * 10**i)) % 10

            if mode == 0:  # position mode
                return self.mem[self.pc + i]
            elif mode == 1:  # immediate mode
                return self.pc + i
            elif mode == 2:  # relative mode
                return self.mem[self.pc + i] + self.relative_base

            raise Exception("bad parameter mode")

        def read_param(i):
            return self.read(param(i))

        opcode = self.mem[self.pc] % 100

        if opcode == 1:
            self.write(param(3), read_param(1) + read_param(2))
            self.pc += 4
        elif opcode == 2:
            self.write(param(3), read_param(1) * read_param(2))
            self.pc += 4
        elif opcode == 3:
            if len(self.inputs) > 0:
                self.write(param(1), self.inputs.popleft())
                self.pc += 2
        elif opcode == 4:
            self.outputs.append(read_param(1))
            self.pc += 2
        elif opcode == 5:
            if read_param(1) == 0:
                self.pc += 3
            else:
                self.pc = read_param(2)
        elif opcode == 6:
            if read_param(1) == 0:
                self.pc = read_param(2)
            else:
                self.pc += 3
        elif opcode == 7:
            self.write(param(3), 1 if read_param(1) < read_param(2) else 0)
            self.pc += 4
        elif opcode == 8:
            self.write(param(3), 1 if read_param(1) == read_param(2) else 0)
            self.pc += 4
        elif opcode == 9:
            self.relative_base += read_param(1)
            self.pc += 2
        elif opcode == 99:
            self.halt = True
        else:
            raise Exception(f"unknown opcode: {opcode} at pc = {self.pc}")

    @property
    def need_input(self):
        return self.mem[self.pc] % 100 == 3 and len(self.inputs) == 0

    def copy(self):
        other = Intcode(self.prog)
        other.pc = self.pc
        other.mem = list(self.mem)
        other.halt = self.halt
        other.inputs = deque(self.inputs)
        other.outputs = deque(self.outputs)
        other.relative_base = self.relative_base
        other.xmem = self.xmem.copy()
        return other
