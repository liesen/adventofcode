import z3


SNAFU_DIGITS = {'=': -2, '-': -1, '0': 0, '1': 1, '2': 2}

def snafu_decode(s: str) -> int:
    return sum(
        SNAFU_DIGITS[digit] * 5**power
        for digit, power in zip(reversed(s), range(len(s)))
    )


with open("input.txt") as f:
    ans = sum(snafu_decode(ln.rstrip()) for ln in f)


# SNAFU encode @ans@
solver = z3.Solver()
num_digits = 30  # Just a guess
digits = [z3.Int(f"digit_{i}") for i in range(num_digits)]

for d in digits:
    solver.add(-2 <= d, d <= 2)

solver.add(sum((d * 5**i) for i, d in enumerate(reversed(digits))) == ans)
assert solver.check() == z3.sat
model = solver.model()

leading_zeros = True

for d in digits:
    if leading_zeros and model[d].as_long() == 0:
        continue

    leading_zeros = False
    print(
        next(s for s, x in SNAFU_DIGITS.items() if x == model[d].as_long()),
        end='',
    )

print()
