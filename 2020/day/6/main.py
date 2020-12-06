import fileinput

group = []
groups = []

for line in fileinput.input('input.txt'):
    line = line.strip()

    if not line:
        groups.append(group)
        group = []
    else:
        group.append(line)

groups.append(group)

ans1 = 0
ans2 = 0

for i, group in enumerate(groups):
    y = set(a for p in group for a in p)
    ans1 += len(y)

    for p in group:
        y &= set(p)

    ans2 += len(y)


print(ans1)
print(ans2)