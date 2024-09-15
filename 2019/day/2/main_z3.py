# from typing import Self
import z3
from dataclasses import dataclass


@dataclass(frozen=True)
class E:
    lit: int
    exp: z3.ArithRef

    # def __add__(self, that: Self):
    def __add__(self, that):
        return E(self.lit + that.lit, self.exp + that.exp)

    # def __mul__(self, that: Self):
    def __mul__(self, that):
        return E(self.lit * that.lit, self.exp * that.exp)


s = z3.Solver()

verb = z3.Int("verb")
s.add(verb >= 0, verb < 99)
noun = z3.Int("noun")
s.add(noun >= 0, noun < 99)

with open("input.txt") as f:
    prog = [int(x) for x in f.read().rstrip().split(",")]

    # Restore the gravity assist program
    prog[1] = 12
    prog[2] = 2
    exprs = z3.IntVector("x", len(prog))
    exprs[1] = noun
    exprs[2] = verb

    for i, n in enumerate(prog):
        if i != 1 and i != 2:
            s.add(exprs[i] == n)

pc = 0
op = 0

while True:
    op = prog[pc]

    if op == 99:
        break
    elif op == 1:
        exprs[prog[pc + 3]] = exprs[prog[pc + 1]] + exprs[prog[pc + 2]]
        prog[prog[pc + 3]] = prog[prog[pc + 1]] + prog[prog[pc + 2]]
        pc += 4
    elif op == 2:
        exprs[prog[pc + 3]] = exprs[prog[pc + 1]] * exprs[prog[pc + 2]]
        prog[prog[pc + 3]] = prog[prog[pc + 1]] * prog[prog[pc + 2]]
        pc += 4
    else:
        assert False, "error"


# Part 1
ans1 = prog[0]
print(ans1)

# Part 2
expr = exprs[0]
s.add(expr == 19690720)
ans2 = z3.Int("ans2")
s.add(ans2 == (100 * noun) + verb)
assert s.check() == z3.sat
print(s.model()[ans2])
