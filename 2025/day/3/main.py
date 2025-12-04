from functools import cache


def find(s: list[int], num_digits: int) -> list[int]:
    @cache
    def go(n: int, i: int) -> list[int]:
        # Must use the rest of the digits
        if i == len(s) - n:
            return s[i:]

        # Must use the largest of the remaining digits
        if n == 1:
            return [max(s[i:])]

        return max([s[i]] + go(n - 1, i + 1), go(n, i + 1))

    return go(num_digits, 0)


ans1 = 0
ans2 = 0

with open(0) as f:
    for ln in f:
        n = [int(x) for x in ln.rstrip()]
        ans1 += int("".join(str(x) for x in find(n, 2)))
        ans2 += int("".join(str(x) for x in find(n, 12)))

print(ans1)
print(ans2)
