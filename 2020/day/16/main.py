import fileinput
import re

scanning_state = 'rules'
rules = {}
nearby_tickets = []

for line in fileinput.input('input.txt'):
    if line == '\n':
        continue

    if line.startswith('your ticket:'):
        scanning_state = 'your ticket'
        continue
    elif line.startswith('nearby tickets:'):
        scanning_state = 'nearby tickets'
        continue

    if scanning_state == 'rules':
        rule_name, s = line.split(':')
        lo0, hi0, lo1, hi1 = map(int, re.findall('\d+', s))
        rules[rule_name] = ((lo0, hi0), (lo1, hi1))
    elif scanning_state == 'your ticket':
        your_ticket = list(map(int, line.split(',')))
    elif scanning_state == 'nearby tickets':
        nearby_tickets.append(list(map(int, line.split(','))))

# Part 1
def test(cond, v):
    (lo0, hi0), (lo1, hi1) = cond
    return lo0 <= v <= hi0 or lo1 <= v <= hi1

ans1 = 0
valid_nearby_tickets = []  # For part 2

for ticket in nearby_tickets:
    valid_ticket = True

    for field in ticket:
        if all(not test(cond, field) for rule_name, cond in rules.items()):
            valid_ticket = False
            ans1 += field

    if valid_ticket:
        valid_nearby_tickets.append(ticket)


print(ans1)

# Part 2
# Possible indices for each rule
candidates_map = {rule_name: set(range(20)) for rule_name in rules}

# Remove impossible indices
for rule_name, cond in rules.items():
    for ticket in valid_nearby_tickets:
        for i, field in enumerate(ticket):
            if not test(cond, field):
                candidates_map[rule_name].remove(i)

# Iteratively find rule with only one possible candidate
# index
ans2 = 1
seen_indices = set()

for rule_name, candidates in sorted(candidates_map.items(), key=lambda x: len(x[1])):
    assert len(candidates - seen_indices) == 1
    ix = (candidates - seen_indices).pop()
    seen_indices.add(ix)

    if rule_name.startswith('departure'):
        ans2 *= your_ticket[ix]

print(ans2)