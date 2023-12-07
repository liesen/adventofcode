from collections import Counter


hands = []

for ln in open("input"):
    hand, bid = ln.split()
    hands.append((hand, int(bid)))


(
    HIGH_CARD,
    ONE_PAIR,
    TWO_PAIR,
    THREE_OF_A_KIND,
    FULL_HOUSE,
    FOUR_OF_A_KIND,
    FIVE_OF_A_KIND,
) = range(7)


def hand_type(hand: str) -> int:
    match sorted(Counter(hand).values()):
        case [5]:
            return FIVE_OF_A_KIND
        case [1, 4]:
            return FOUR_OF_A_KIND
        case [2, 3]:
            return FULL_HOUSE
        case [1, 1, 3]:
            return THREE_OF_A_KIND
        case [1, 2, 2]:
            return TWO_PAIR
        case [1, 1, 1, 2]:
            return ONE_PAIR
        case [1, 1, 1, 1, 1]:
            return HIGH_CARD
        case _:
            raise Exception("unreachable")


# Part 1
RANKS = "23456789TJQKA"


def hand_strength(hand):
    return (
        hand_type(hand),
        RANKS.index(hand[0]),
        RANKS.index(hand[1]),
        RANKS.index(hand[2]),
        RANKS.index(hand[3]),
        RANKS.index(hand[4]),
    )


print(
    sum(
        (i + 1) * bid
        for i, (hand, bid) in enumerate(
            sorted(hands, key=lambda hb: hand_strength(hb[0]))
        )
    )
)


# Part 2
def replace_joker(hand):
    c = Counter(hand)

    if "J" not in c:
        return hand

    del c["J"]

    if not c:
        return "AAAAA"

    def count_cards(x):
        card, n = x
        return (n, -RANKS.index(card))

    best_card, _ = max(c.items(), key=count_cards)
    return hand.replace("J", best_card)


JOKER_RANKS = "J23456789TQKA"


def joker_hand_strength(hand):
    return (
        hand_type(replace_joker(hand)),
        JOKER_RANKS.index(hand[0]),
        JOKER_RANKS.index(hand[1]),
        JOKER_RANKS.index(hand[2]),
        JOKER_RANKS.index(hand[3]),
        JOKER_RANKS.index(hand[4]),
    )


print(
    sum(
        (i + 1) * bid
        for i, (hand, bid) in enumerate(
            sorted(hands, key=lambda hb: joker_hand_strength(hb[0]))
        )
    )
)
