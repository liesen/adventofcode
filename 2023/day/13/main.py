input = open("input").read().strip()


def find_crease(grid, num_acceptable_smudges=0):
    for i in range(1, len(grid)):
        n = min(i, len(grid) - i)
        num_smudges = sum(
            sum(ch1 != ch2 for ch1, ch2 in zip(r1, r2))
            for r1, r2 in zip(reversed(grid[i - n : i]), grid[i : i + n])
        )

        if num_smudges == num_acceptable_smudges:
            return i


ans1 = 0
ans2 = 0

for lns in input.split("\n\n"):
    grid = lns.splitlines()

    if r := find_crease(grid):
        ans1 += 100 * r
    elif c := find_crease(["".join(r) for r in zip(*grid)]):
        ans1 += c

    if r := find_crease(grid, 1):
        ans2 += 100 * r
    # elif c := find_crease(["".join(r) for r in zip(*grid)], 1):
    elif c := find_crease(["".join(r) for r in zip(*grid)], 1):
        ans2 += c


print(ans1)
print(ans2)
