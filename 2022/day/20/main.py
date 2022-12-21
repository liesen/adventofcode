from collections import deque


with open("input.txt") as f:
    numbers = [int(ln.rstrip()) for ln in f]
    N = len(numbers)

# Part 1
numbers1 = numbers
res1 = deque(enumerate(numbers1))

for i, x in enumerate(numbers1):
    j = res1.index((i, x))
    res1.rotate(-j)
    res1.popleft()
    res1.rotate(-x)
    res1.appendleft((i, x))

ns1 = [x for i, x in res1]
ans1 = sum(ns1[(ns1.index(0) + i * 1000) % len(ns1)] for i in range(1, 4))
print(ans1)

# Part 2
DECRYPTION_KEY = 811589153
numbers2 = [x * DECRYPTION_KEY for x in numbers1]
res2 = deque(enumerate(numbers2))

for round in range(10):
    for i, x in enumerate(numbers2):
        j = res2.index((i, x))
        res2.rotate(-j)
        res2.popleft()
        res2.rotate(-x)
        res2.appendleft((i, x))

ns2 = [x for i, x in res2]
ans2 = sum(ns2[(ns2.index(0) + i * 1000) % len(ns2)] for i in range(1, 4))
print(ans2)
