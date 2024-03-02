from collections import deque

P = set()  # planets
O = {}  # key --orbits--> value, 1:1

with open("input.txt") as fp:
    for ln in fp:
        x, y = ln.strip().split(")")
        O[y] = x
        P |= set([x, y])  # add x add y

# Part 1
ans1 = 0

for p in P:
    while p != "COM":
        p = O[p]
        ans1 += 1

print(ans1)

# Part 2
# Find path YOU -> COM
p = "YOU"
you_path = deque()

while p != "COM":
    p = O[p]
    you_path.appendleft(p)

# Find path SAN -> COM
p = "SAN"
san_path = deque()

while p != "COM":
    p = O[p]
    san_path.appendleft(p)

# Find intersection
while you_path[0] == san_path[0]:
    you_path.popleft()
    san_path.popleft()

ans2 = len(you_path) + len(san_path)
print(ans2)
