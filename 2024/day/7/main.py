from collections.abc import Generator


def parse_line(s: str):
    i = s.index(": ")
    return int(s[:i]), [int(term) for term in s[i + len(" ") :].split()]


def evaluate(terms: list[int], part2: bool = False) -> Generator[int]:
    match terms:
        case [x]:
            yield x

        case [x, *xs]:
            for y in evaluate(xs, part2):
                yield y + x
                yield y * x

                if part2:
                    yield int(f"{y}{x}")

        case _:
            assert False, "unreachable"


ans1 = 0
ans2 = 0

with open("input") as f:
    for ln in f:
        expected, terms = parse_line(ln.rstrip())
        rev_terms = list(reversed(terms))

        if expected in evaluate(rev_terms):
            ans1 += expected

        if expected in evaluate(rev_terms, part2=True):
            ans2 += expected


print(ans1)
print(ans2)
