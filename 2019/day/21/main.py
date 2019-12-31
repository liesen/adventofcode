from intcode import Intcode
from itertools import chain


with open('input.txt') as fp:
    prog = Intcode([int(x) for x in fp.read().split(',')])

# Idea: the jump is four tiles wide, so if there's
# ground four tiles away (D) and not ground anywhere
# in between (~A | ~B | ~C), then jump
scriptcode = '''
NOT A J
NOT B T
OR T J
NOT C T
OR T J
AND D J
WALK
'''

def compile(sc):
    return list(chain(*[list(map(ord, s)) + [10] for s in sc.splitlines() if s]))

prog.run()
prog.run(compile(scriptcode))

for z in prog.outputs:
    if z > 256:
        ans = z
        print(ans)
        break

    print(chr(z), end='')