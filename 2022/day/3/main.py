lines = [ln.strip() for ln in open("input.txt")]


def priority(x):
    if 'a' <= x <= 'z':
        return ord(x) - ord('a') + 1
    elif 'A' <= x <= 'Z':
        return ord(x) - ord('A') + 27

    raise Exception("bad input")


# Part 1
ans1 = 0

for ln in lines:
    common = set(ln[0:len(ln) // 2]) & set(ln[len(ln) // 2:])
    assert len(common) == 1
    ans1 += sum(map(priority, common))

print(ans1)

# Part 2
ans2 = 0

for i in range(0, len(lines), 3):
    badge = set(lines[i + 0]) & set(lines[i + 1]) & set(lines[i + 2])
    assert len(badge) == 1
    ans2 += sum(map(priority, badge))

print(ans2)
