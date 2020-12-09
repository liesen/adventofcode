import fileinput

numbers = [int(line.strip()) for line in fileinput.input('input.txt')]
PREAMBLE_LEN = 25
i = PREAMBLE_LEN + 1

while i < len(numbers) - PREAMBLE_LEN:
    ans1 = numbers[i]
    preamble = numbers[i - PREAMBLE_LEN:i]
    ok = False

    for m in preamble:
        if ans1 - m in preamble:
            ok = True
            break

    if not ok:
        print(ans1)
        break

    i += 1

# Part 2
for i in range(len(numbers)):
    s = 0
    j = i

    while j < len(numbers) and s < ans1:
        s += numbers[j]
        j += 1

    if s == ans1:
        print(max(numbers[i:j]) + min(numbers[i:j]))
        break