from collections import deque, namedtuple
import string


Problem = namedtuple("Problem", ["s", "maxcol", "maxrow", "start", "keys", "doors"])


def parse_problem(s):
    maxcol = s.index(chr(10))
    maxrow = len(s.strip()) // maxcol
    start = next(
        (x, y)
        for x in range(maxcol)
        for y in range(maxrow)
        if s[(maxcol + 1) * y + x] == "@"
    )
    doors = {
        s[(maxcol + 1) * y + x]: (x, y)
        for x in range(maxcol)
        for y in range(maxrow)
        if s[(maxcol + 1) * y + x] in string.ascii_uppercase
    }
    keys = {
        s[(maxcol + 1) * y + x]: (x, y)
        for x in range(maxcol)
        for y in range(maxrow)
        if s[(maxcol + 1) * y + x] in string.ascii_lowercase
    }
    return Problem(
        s=s, maxcol=maxcol, maxrow=maxrow, start=start, keys=keys, doors=doors
    )


with open("input.txt") as fp:
    s = fp.read()
    problem = parse_problem(s)

# Part 1
all_keys = 0

for k in problem.keys:
    all_keys |= 1 << (ord(k) - ord("a"))

q = deque([(problem.start, 0)])
seen = set([(problem.start, 0)])
n = 0
done = False

while not done and q:
    new_q = deque([])

    for pos, keys in q:
        x, y = pos

        if keys == all_keys:
            done = True
            break

        for dx, dy in [(0, -1), (0, 1), (-1, 0), (1, 0)]:
            new_x, new_y = new_pos = x + dx, y + dy

            if not (0 <= new_x <= problem.maxcol or 0 <= new_y <= problem.maxrow):
                continue

            z = problem.s[(problem.maxcol + 1) * new_y + new_x]

            if z == "#":
                continue

            if z in problem.keys:
                new_key = keys | 1 << (ord(z) - ord("a"))

                if (new_pos, new_key) in seen:
                    continue

                seen.add((new_pos, new_key))
                new_q.append((new_pos, new_key))
            elif z in problem.doors:
                if keys & (1 << (ord(z) - ord("A"))) == 0:
                    continue

                if (new_pos, keys) in seen:
                    continue

                seen.add((new_pos, keys))
                new_q.append((new_pos, keys))
            else:
                if (new_pos, keys) in seen:
                    continue

                seen.add((new_pos, keys))
                new_q.append((new_pos, keys))

    if not done:
        n += 1
        q = new_q

print(n)
