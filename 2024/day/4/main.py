from itertools import product

with open("input") as f:
    S = [ln.rstrip() for ln in f]
    numrows = len(S)
    numcols = len(S[0])

# Part 1
print(
    sum(
        1
        for r, c, dr, dc in product(
            range(numrows), range(numcols), [-1, 0, 1], [-1, 0, 1]
        )
        if all(
            0 <= r + dr * i < numrows
            and 0 <= c + dc * i < numcols
            and S[r + dr * i][c + dc * i] == ch
            for i, ch in enumerate("XMAS")
        )
    )
)

# Part 2
ans2 = 0

for r, c in product(range(1, numrows - 1), range(1, numcols - 1)):
    ans2 += all(
        [
            S[r - 1][c - 1] == "M",
            S[r - 1][c + 1] == "M",
            S[r][c] == "A",
            S[r + 1][c - 1] == "S",
            S[r + 1][c + 1] == "S",
        ]
    )
    ans2 += all(
        [
            S[r - 1][c - 1] == "M",
            S[r - 1][c + 1] == "S",
            S[r][c] == "A",
            S[r + 1][c - 1] == "M",
            S[r + 1][c + 1] == "S",
        ]
    )
    ans2 += all(
        [
            S[r - 1][c - 1] == "S",
            S[r - 1][c + 1] == "S",
            S[r][c] == "A",
            S[r + 1][c - 1] == "M",
            S[r + 1][c + 1] == "M",
        ]
    )
    ans2 += all(
        [
            S[r - 1][c - 1] == "S",
            S[r - 1][c + 1] == "M",
            S[r][c] == "A",
            S[r + 1][c - 1] == "S",
            S[r + 1][c + 1] == "M",
        ]
    )

print(ans2)
