import fileinput

G = [x.strip() for x in fileinput.input('input.txt')]

numrows = len(G)
numcols = len(G[0])

ans1 = 0
ans2 = 1

DX = [1, 3, 5, 7, 1]
DY = [1, 1, 1, 1, 2]

for dx, dy in zip(DX, DY):
    num_trees = 0

    for i, y in enumerate(range(0, numrows, dy)):
        x = (i * dx) % numcols
        num_trees += 1 if G[y][x] == '#' else 0
    
    # Part 1
    if dx == 3 and dy == 1:
        ans1 = num_trees

    # Part 2
    ans2 *= num_trees

print(ans1)
print(ans2)