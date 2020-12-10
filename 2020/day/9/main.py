import fileinput

numbers = [int(line.strip()) for line in fileinput.input('input.txt')]
PREAMBLE_LEN = 25
i = PREAMBLE_LEN + 1

while i < len(numbers) - PREAMBLE_LEN:
    ans1 = numbers[i]
    preamble = numbers[i - PREAMBLE_LEN:i]

    if all(ans1 - m not in preamble for m in preamble):
        break

    i += 1

print(ans1)

# Part 2
acc, lo, hi = 0, 0, 0

while acc != ans1:
    if acc < ans1:
        acc += numbers[hi]
        hi += 1
    elif acc > ans1:
        acc -= numbers[lo]
        lo += 1

assert acc == ans1
ans2 = min(numbers[lo:hi]) + max(numbers[lo:hi])
print(ans2)