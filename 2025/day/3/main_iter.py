def max_joltage(bats: str, n: int) -> int:
    N = len(bats)
    start = 0
    ans = 0

    for end in range(N - n, N):
        next_start, d = max(enumerate(bats[start:end], start), key=lambda x: x[1])
        ans = ans * 10 + int(d)
        start = next_start + 1

    return ans


with open(0) as f:
    lines = [ln.rstrip() for ln in f]


print(sum(max_joltage(bats, 2) for bats in lines))
print(sum(max_joltage(bats, 12) for bats in lines))
