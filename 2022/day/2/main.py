ans1 = 0
ans2 = 0

with open("input.txt") as fp:
    for ln in fp:
        p1, p2 = ln[0], ln[2]
        
        # Convert to Rock=0, Paper=1, Scissor=2
        q1, q2 = ord(p1) - ord('A'), ord(p2) - ord('X')

        if (q1 + 1) % 3 == q2:  # Win
            ans1 += 6 + q2 + 1
        elif q1 == q2:  # Draw
            ans1 += 3 + q2 + 1
        else:  # Lose
            ans1 += 0 + q2 + 1

        if q2 == 0:  # Lose
            ans2 += 0 + (q1 - 1) % 3 + 1
        elif q2 == 1:  # Draw
            ans2 += 3 + q1 + 1
        else:  # Win
            ans2 += 6 + (q1 + 1) % 3 + 1

print(ans1)
print(ans2)
