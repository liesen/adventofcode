from typing import Callable
import z3

# Program: 2,4,1,2,7,5,0,3,4,7,1,7,5,5,3,0
prog = [2, 4, 1, 2, 7, 5, 0, 3, 4, 7, 1, 7, 5, 5, 3, 0]
a = 30878003
b = 0
c = 0

# do:
# bst 4 = b = a % 8
# bxl 2 = b = b ^ 2
# cdv 5 = c = a / 2^b
# adv 3 : a = a / 2^3
# bxc 7 : b = b ^ c
# bxl 7 : b = b ^ 7
# out 5 : out(c % 8)
# jnz 0 : while a != 0


def run(prog: list[int], a: int, b: int, c: int):
    ip = 0
    out: list[int] = []

    def combo(x: int):
        return [0, 1, 2, 3, a, b, c][x]

    while ip < len(prog):
        match prog[ip], prog[ip + 1]:
            case 0, x:
                a = a // (1 << combo(x))
                ip += 2
            case 1, x:
                b = b ^ x
                ip += 2
            case 2, x:
                b = combo(x) % 8
                ip += 2
            case 3, x if a == 0:
                ip += 2
            case 3, x:
                ip = x
            case 4, x:
                b = b ^ c
                ip += 2
            case 5, x:
                out.append(combo(x) % 8)
                ip += 2
            case 6, x:
                b = a // (1 << combo(x))
                ip += 2
            case 7, x:
                c = a // (1 << combo(x))
                ip += 2
            case _:
                raise Exception("unreachable")

    return out


# Part 1
print(",".join(str(x) for x in run(prog, a, b, c)))

# Part 2
bv = 64  # Found by trial and error
a = z3.BitVec("a", bv)
b = 0
c = 0


def combo(x):
    return [0, 1, 2, 3, a, b, c][x]


# Gather the expressions for the first len(prog) outputs
outs = []

for i in range(len(prog)):
    b = combo(4) & 0b111
    b = b ^ 2
    c = a / (1 << combo(5))
    a = a / 8
    b = b ^ c
    b = b ^ 7
    outs.append(combo(5) & 0b111)

s = z3.Optimize()

for out, expected in zip(outs, prog):
    s.add(out == expected)

s.add(z3.BV2Int(a) < 190384113355697)  # Rist result was too high
s.minimize(z3.BV2Int(a))
assert s.check() == z3.sat
m = s.model()
print(m)

run(prog, 190384113204239, 0, 0) == prog
