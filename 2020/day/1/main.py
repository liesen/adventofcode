import fileinput

with fileinput.input('input.txt') as f:
    numbers = {int(ln) for ln in f}

# Part 1
for x in numbers:
    if 2020 - x in numbers:
        print(x * (2020 - x))
        break

# Part 2
done = False

for x in numbers:
    for y in numbers:
        if 2020 - x - y in numbers:
            print(x * y * (2020 - x - y))
            done = True
            break
    
    if done: break