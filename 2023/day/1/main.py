import fileinput


digit_digits = [(v, str(v)) for v in range(1, 10)]
digit_words = list(
    zip(
        range(1, 10),
        ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"],
    )
)


def opt_rindex(s, pat):
    try:
        yield s.rindex(pat)
    except ValueError:
        return


def opt_index(s, pat):
    try:
        yield s.index(pat)
    except ValueError:
        return


ans1 = 0
ans2 = 0

with fileinput.input("input") as f:
    for ln in f:
        # Part 1
        digits = [x for x in ln if x.isdigit()]
        first_digit1 = int(digits[0])
        last_digit1 = int(digits[-1])
        ans1 += first_digit1 * 10 + last_digit1

        # Part 2
        _, first_digit2 = min(
            (i, v) for (v, w) in digit_words + digit_digits for i in opt_index(ln, w)
        )
        _, last_digit2 = max(
            (i, v) for (v, w) in digit_words + digit_digits for i in opt_rindex(ln, w)
        )
        ans2 += first_digit2 * 10 + last_digit2


print(ans1)
print(ans2)
