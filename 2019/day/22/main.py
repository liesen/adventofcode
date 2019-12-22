with open('input.txt') as fp:
    S = fp.read()

# Part 1
N = 10007
x = 2019

for ln in enumerate(S.splitlines()):
    if ln == 'deal into new stack':
        x = N - x - 1
    elif ln.startswith('deal with increment'):
        i = int(ln[len('deal with increment '):])
        x = (i * x) % N
    elif ln.startswith('cut'):
        i = int(ln[len('cut '):])
        x = (N + x - i) % N

print(x)