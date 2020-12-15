spoken_numbers = {}

for i, x in enumerate('16,1,0,18,12,14,19'.split(',')):
    last_number_spoken = int(x)
    spoken_numbers[last_number_spoken] = i  # Zero indexed

while i + 1 < 30000000:

    j = spoken_numbers.get(last_number_spoken, i)
    spoken_numbers[last_number_spoken] = i
    last_number_spoken = i - j
    i += 1

    # Part 1
    if i + 1 == 2020:
        print(last_number_spoken)

# Part 2
print(last_number_spoken)