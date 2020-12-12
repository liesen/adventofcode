import fileinput

# For part 1
n1, e1, a1 = 0, 0, 0

# For part 2
n2, e2 = 0, 0
wn, we = 1, 10

for line in fileinput.input('testinput.txt'):
    d, i = line[0], int(line[1:])

    if d == 'N':
        n1 += i
        wn += i
    elif d == 'S':
        n1 -= i
        wn -= i
    elif d == 'E':
        e1 += i
        we += i
    elif d == 'W':
        e1 -= i
        we -= i
    elif d == 'L':
        a1 = (a1 + i) % 360

        if i == 90:
            wn, we = we, -wn
        elif i == 180:
            wn, we = -wn, -we
        elif i == 270:
            wn, we = -we, wn
        else:
            assert False, line.strip()
    elif d == 'R':
        a1 = (a1 - i) % 360

        if i == 90:
            wn, we = -we, wn
        elif i == 180:
            wn, we = -wn, -we
        elif i == 270:
            wn, we = we, -wn
        else:
            assert False, line.strip()
    elif d == 'F':
        if a1 == 0:
            e1 += i
        elif a1 == 90:
            n1 += i
        elif a1 == 180:
            e1 -= i
        elif a1 == 270:
            n1 -= i

        n2 += wn * i
        e2 += we * i

# Part 1
print(abs(n1) + abs(e1))

# Part 2
print(abs(n2) + abs(e2))
