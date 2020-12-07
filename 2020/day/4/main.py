import fileinput
import re

ans1 = 0
ans2 = 0

rules = {
    'byr': lambda x: re.match('^\d{4}$', x) and (1920 <= int(x) <= 2002),
    'iyr': lambda x: re.match('^\d{4}$', x) and (2010 <= int(x) <= 2020),
    'eyr': lambda x: re.match('^\d{4}$', x) and (2020 <= int(x) <= 2030),
    'hgt': lambda x: \
        (re.match('^\d+cm$', x) and 150 <= int(x[0:-2]) <= 193) or \
        (re.match('^\d+in$', x) and 59 <= int(x[0:-2]) <= 76),
    'hcl': lambda x: re.match('^#([0-9a-f]{6})$', x),
    'ecl': lambda x: x in ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'],
    'pid': lambda x: re.match('^\d{9}$', x),
    'cid': lambda x: True
}

def part1(passport):
    return len(passport.keys() & set(['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'])) == 7

def part2(passport):
    global rules
    for k, v in passport.items():
        if not rules[k](v):
            return False

    return True

# 
passport = {}

for line in fileinput.input('input.txt'):
    if line == '\n':
        if part1(passport):
            ans1 += 1

            if part2(passport):
                ans2 += 1

        passport = {}
    else:
        for kv in line.split():
            k, v = kv.split(':', 2)
            passport[k] = v

# Last passport
if part1(passport):
    ans1 += 1

    if part2(passport):
        ans2 += 1


print(ans1)
print(ans2)

# 225
# 181 low
# 223