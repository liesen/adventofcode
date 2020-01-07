from intcode import Intcode
from itertools import chain


with open('input.txt') as fp:
    image = [int(x) for x in fp.read().split(',')]

# Part 1
# Idea: the jump is four tiles wide, so if there's
# ground four tiles away (D) and not ground anywhere
# in between (~A | ~B | ~C), then jump
scriptcode = '''NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
WALK
'''

prog = Intcode(image)
prog.runs(scriptcode)

for z in prog.outputs:
    if z > 256:
        ans = z
        print(ans)
        break

    print(chr(z), end='')

# Part 2
# ABCDEFGHI
#    ^   ^
scriptcode = '''NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
NOT I T
NOT T T
OR F T
AND E T
OR H T
AND T J
RUN
'''

prog = Intcode(image)
prog.runs(scriptcode)

for z in prog.outputs:
    if z > 256:
        ans = z
        print(ans)
        break

    print(chr(z), end='')