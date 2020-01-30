with open('input.txt') as fp:
    S = fp.read()

# Part 1
N = 10007
x = 2019

for ln in S.splitlines():
    if ln == 'deal into new stack':
        x = N - x - 1
    elif ln.startswith('deal with increment'):
        i = int(ln[len('deal with increment '):])
        x = (i * x) % N
    elif ln.startswith('cut'):
        i = int(ln[len('cut '):])
        x = (N + x - i) % N

print(x)

# Part 1 as a linear congruential generator: https://en.wikipedia.org/wiki/Linear_congruential_generator
a, b = 1, 0  # Identity

# All the steps can be written as x_(i + 1) = a * x_i + b mod N
for ln in S.splitlines():
    if ln == 'deal into new stack':
        c = -1
        d = -1
    elif ln.startswith('deal with increment'):
        c = int(ln[len('deal with increment '):])
        d = 0
    elif ln.startswith('cut'):
        c = 1
        d = -int(ln[len('cut '):])

    # Combine the two LCRs
    a = (a * c) % N
    b = (b * c + d) % N

assert (a * 2019 + b) % N == x

# Part 2
N = 119315717514047
k = 101741582076661
lcr_1 = 1, 0

# Combining two LCRs
def X(x, y, m=N):
    a, b = x
    c, d = y
    return (a * c) % m, (b * c + d) % m

for ln in S.splitlines():
    if ln == 'deal into new stack':
        c = -1
        d = -1
    elif ln.startswith('deal with increment'):
        c = int(ln[len('deal with increment '):])
        d = 0
    elif ln.startswith('cut'):
        c = 1
        d = -int(ln[len('cut '):])

    lcr_1 = X(lcr_1, (c, d))

a_1, b_1 = lcr_1

# Raise the lcr to the power of k using exponentiation by squaring: https://en.wikipedia.org/wiki/Exponentiation_by_squaring#Basic_method
lcr_k = lcr_1
lcr_y = 1, 0

while k > 1:
    if k % 2 == 0:
        lcr_k = X(lcr_k, lcr_k)
        k //= 2
    else:
        lcr_y = X(lcr_k, lcr_y)
        lcr_k = X(lcr_k, lcr_k)
        k = (k - 1) // 2

lcr_k = X(lcr_k, lcr_y)

# Now we have the parameters, a and b, of the shuffling
# sequence when it is applied k times
a_k, b_k = lcr_k

# Solve a_k * x + b_k mod N == 2020  =>  x = (2020 - b_k) * a_k^-1
# https://stackoverflow.com/questions/4798654/modular-multiplicative-inverse-function-in-python
def egcd(a, b):
    if a == 0:
        return (b, 0, 1)
    else:
        g, y, x = egcd(b % a, a)
        return (g, x - (b // a) * y, y)

def modinv(a, m):
    g, x, y = egcd(a, m)
    if g != 1:
        raise Exception('modular inverse does not exist')
    else:
        return x % m

print(((2020 - b_k) * modinv(a_k, N)) % N)