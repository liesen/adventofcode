import fileinput
from dataclasses import dataclass
from functools import cache


@dataclass
class Card:
    card_id: int
    winning_numbers: set[int]
    numbers_you_have: set[int]


def parse_line(ln) -> Card:
    assert ln.startswith("Card ")
    card_id, rest = ln[len("Card ") :].split(":")
    card_id = int(card_id)
    winning, have = rest.split(" | ")
    winning = set(int(x) for x in winning.split())
    have = set(int(x) for x in have.split())
    return Card(card_id, winning, have)


with fileinput.input("input") as f:
    cards = [parse_line(ln) for ln in f]


# Part 1
ans1 = 0

for card in cards:
    n = len(card.numbers_you_have & card.winning_numbers)
    score = int(2 ** (n - 1))
    ans1 += score

print(ans1)


# Part 2
@cache
def count_cards(i):
    if i >= len(cards):
        return 0

    card = cards[i]
    n = len(card.numbers_you_have & card.winning_numbers)
    return 1 + sum(count_cards(i + di + 1) for di in range(n))


ans2 = sum(count_cards(i) for i in range(len(cards)))
print(ans2)
