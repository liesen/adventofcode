import fileinput
from collections import Counter
import re

password_re = re.compile('^(\d+)-(\d+) (.): (.*)$')
ans1 = 0
ans2 = 0

with fileinput.input('input.txt') as f:
    for s in f:
        m = password_re.match(s.strip())

        # lo-hi x: password
        lo = int(m.group(1))
        hi = int(m.group(2))
        x = m.group(3)
        password = m.group(4)

        # Part 1
        if lo <= Counter(password).get(x, 0) <= hi:
            ans1 += 1

        # Part 2
        if (password[lo - 1] == x) != (password[hi - 1] == x):
            ans2 += 1

print(ans1)
print(ans2)