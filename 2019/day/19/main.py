from intcode import Intcode


with open('input.txt') as fp:
    prog = Intcode([int(x) for ln in fp for x in ln.split(',')])

# Part 1
ans = 0

for y in range(50):
    for x in range(50):
        prog.reset()
        prog.run([y, x])
        z = prog.outputs.pop()
        ans += z

print(ans)

# Part 2
def test(x, y):
    global prog
    prog.reset()
    prog.run([x, y])
    return prog.outputs.pop()

y0 = 10
x0 = 0

# Find start of beam
while test(x0, y0) == 0:
    x0 += 1

# Find end of beam
x1 = x0

while test(x1, y0) == 1:
    x1 += 1

# Size of the largest possible square that fits inside the beam
s = 0

while s < 100:
    y0 += 1
    
    while test(x1, y0) == 1:
        x1 += 1

    # Try to increase the square size
    while test(x1 - s - 1, y0 + s) == 1:
        s += 1

print((x1 - 100) * 10000 + y0)
