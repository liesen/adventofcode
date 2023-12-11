from collections import deque

E = {}


# Transition function
def t(x, r, c):
    match x:
        case "|":
            return ((r - 1, c), (r + 1, c))
        case "-":
            return ((r, c - 1), (r, c + 1))
        case "L":
            return ((r - 1, c), (r, c + 1))
        case "J":
            return ((r - 1, c), (r, c - 1))
        case "7":
            return ((r + 1, c), (r, c - 1))
        case "F":
            return ((r + 1, c), (r, c + 1))


S = []

with open("input") as f:
    for r, ln in enumerate(f):
        S.append(ln.rstrip())

        for c, x in enumerate(ln.rstrip()):
            if x == "S":
                sr, sc = r, c
            elif x == ".":
                pass
            else:
                E[(r, c)] = t(x, r, c)

ans1 = 0  # Half of the loop length
found_s = False

for sx in "-|LJ7F":
    # Replace start pipe with what we think is the pipe
    head, tail = E[(sr, sc)] = t(sx, sr, sc)
    # Replace S with pipe char
    S[sr] = S[sr].replace("S", sx)

    # Both head and tail must be in the grid because S is in the loop
    if head not in E or tail not in E:
        continue

    # We must be able to go back to the starting tile from the head and tail
    # otherwise the S pipe does not form a complete loop
    if (sr, sc) not in E[head] or (sr, sc) not in E[tail]:
        continue

    loop = set([(sr, sc)])
    q = deque([(1, head, tail)])

    while q:
        n, head, tail = q.popleft()

        if head in loop or tail in loop:
            continue

        loop.add(head)
        loop.add(tail)

        if head == tail:
            ans1 = n
            found_s = True
            break

        q.extend((n + 1, h, t) for h in E.get(head, set()) for t in E.get(tail, set()))

    if found_s:
        break

# Part 1
assert ans1 * 2 == len(loop)
print(ans1)

# Part 2: sweep left to right, keeping track of crossings to know if point is
# inside the polygon

# Replace all tiles not part of the loop with '.'
S = [
    "".join(x if (r, c) in loop else "." for c, x in enumerate(ln))
    for r, ln in enumerate(S)
]

ans2 = 0  # Number of tiles not enclosed by the loop

for r, ln in enumerate(S):
    inside_loop = False
    L = None

    for c, x in enumerate(ln):
        match x:
            case ".":
                if inside_loop:
                    ans2 += 1
            case "|":
                inside_loop = not inside_loop
            case "-":
                pass
            case "L":
                L = True
            case "F":
                L = False
            case "7":
                if L:
                    inside_loop = not inside_loop

                L = None
            case "J":
                if not L:
                    inside_loop = not inside_loop

                L = None

print(ans2)
