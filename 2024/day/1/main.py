from collections import Counter

lefts: list[int] = []
rights: list[int] = []

with open("input") as f:
    for line in f:
        left, right = map(int, line.rstrip().split())
        lefts.append(left)
        rights.append(right)

# Part 1
total_distance = 0

for left, right in zip(sorted(lefts), sorted(rights)):
    distance = max(left, right) - min(left, right)
    total_distance += distance

print(total_distance)

# Part 2: count number of occurances in the right list
counter = Counter(rights)
total_similarity_score = 0

for left in lefts:
    similarity_score = left * counter[left]
    total_similarity_score += similarity_score

print(total_similarity_score)

