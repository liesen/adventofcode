with open("input.txt") as f:
    groups = list(map(lambda g: sum(map(int, g.splitlines())), f.read().split("\n\n")))

# Part 1
print(max(groups))

# Part 2
print(sum(sorted(groups)[-3:]))
