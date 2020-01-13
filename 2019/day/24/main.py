import numpy as np

with open('input.txt') as fp:
    S = fp.read()
    biodiversity = sum(1 << (r * 5 + c) for r, xs in enumerate(S.splitlines()) for c, x in enumerate(xs) if x == '#')

# Part 1
ans = biodiversity
seen = set()

while ans not in seen:
    seen.add(ans)
    new_biodiversity = ans

    for r in range(5):
        for c in range(5):
            n = 0

            for dr, dc in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
                if 0 <= r + dr < 5 and 0 <= c + dc < 5:
                    ix = (r + dr) * 5 + c + dc
                    n += (ans & (1 << ix)) > 0

            ix = r * 5 + c

            if ans & (1 << ix) and n != 1:
                new_biodiversity &= ~(1 << ix)
            elif not (ans & (1 << ix)) and 1 <= n <= 2:
                new_biodiversity |= 1 << ix

    ans = new_biodiversity

print(ans)