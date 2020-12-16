from collections import deque
import fileinput

G = {}

for line in fileinput.input('input.txt'):
    line = line.strip()
    words = line.split()
    key = ' '.join(words[0:2])
    assert words[2] == 'bags'
    assert words[3] == 'contain'

    for i in range(4, len(words), 4):
        if words[i] == 'no':
            n = 0
            break
        else:
            n = int(words[i])

        key2 = ' '.join(words[i+1:i+3])
        assert words[i + 3] in ['bag,', 'bags,', 'bag.', 'bags.'], words[i + 3]
        G.setdefault(key, set()).add((key2, n))

# Part 1
# All nodes that are connected to shiny gold
ans1 = 0

def reachable(x, G):
    yield x
    for y, m in G.get(x, []):
        for z in reachable(y, G):
            yield z

for x in G:
    if x != 'shiny gold':
        ans1 += 'shiny gold' in reachable(x, G)

print(ans1)

# Part 2
# Total number of bags inside the shiny gold bag
def part2(x):
    return sum(n + n * part2(y) for y, n in G.get(x, []))

print(part2('shiny gold'))