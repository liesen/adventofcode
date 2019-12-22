with open('input.txt') as fp:
    S = fp.read()

# Part 1
N = 10007
x = 2019
y = 1498

for ln in S.splitlines():
    if ln == 'deal into new stack':
        x = N - x - 1
        y = N - (N - y - 1)
    elif ln.startswith('deal with increment'):
        i = int(ln[len('deal with increment '):])
        x = (i * x) % N
        y = N - ((i * y) % N)
    elif ln.startswith('cut'):
        i = int(ln[len('cut '):])
        x = (N + x - i) % N
        y = N - ((N + y - i) % N)

print(x)
print(y)