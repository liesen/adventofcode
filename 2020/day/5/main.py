import fileinput

def parse_seat(s):
    row_lo, row_hi = 0, 127
    seat_lo, seat_hi = 0, 7

    for x in s:
        if x == 'B':
            row_lo = (row_lo + row_hi + 1) // 2
        elif x == 'F':
            row_hi = row_lo + (row_hi - row_lo) // 2
        elif x == 'R':
            seat_lo = (seat_lo + seat_hi + 1) // 2
        elif x == 'L':
            seat_hi = seat_lo + (seat_hi - seat_lo) // 2
            
    assert row_lo == row_hi, (s, row_lo, row_hi)
    assert seat_lo == seat_hi, (s, seat_lo, seat_hi)
    return row_lo, seat_lo

def seat_id(seat):
    row, col = seat
    return row * 8 + col

seat_ids = set(seat_id(parse_seat(line.strip())) for line in fileinput.input('input.txt'))

# Part 1
print(max(seat_ids))

# Part 2
for i in range(min(seat_ids), max(seat_ids)):
    if i not in seat_ids and i - 1 in seat_ids and i + 1 in seat_ids:
        print(i)