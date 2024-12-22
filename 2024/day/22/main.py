from collections.abc import Generator
from itertools import islice

secrets = [int(ln.rstrip()) for ln in open(0)]


def mix(value: int, secret: int) -> int:
    return value ^ secret


def prune(secret: int) -> int:
    return secret % 16777216


def evolve(secret: int) -> int:
    secret = prune(mix(secret, secret * 64))
    secret = prune(mix(secret, secret // 32))
    secret = prune(mix(secret, secret * 2048))
    return secret


def generate(secret: int) -> Generator[int]:
    yield secret

    while True:
        secret = evolve(secret)
        yield secret


# Part 1
ans1 = 0

for secret in secrets:
    ans1 += next(islice(generate(secret), 2000, 2001))

print(ans1)

# Part 2
changes: list[dict[tuple[int, int, int, int], int]] = []

for secret in secrets:
    prices = list(islice(map(lambda p: p % 10, generate(secret)), 0, 2001))
    buyer_changes: dict[tuple[int, int, int, int], int] = {}

    for p0, p1, p2, p3, p4 in zip(
        prices, prices[1:], prices[2:], prices[3:], prices[4:]
    ):
        four_consecutive_changes = (p1 - p0, p2 - p1, p3 - p2, p4 - p3)

        # Only keep first seen price for four consecutive changes
        if four_consecutive_changes in buyer_changes:
            continue

        buyer_changes[four_consecutive_changes] = p4

    changes.append(buyer_changes)

# Max over the sum over each secret for all possible four consecutive changes
print(
    max(
        sum(buyer_changes.get(four_consecutive_changes, 0) for buyer_changes in changes)
        for four_consecutive_changes in {
            four_consecutive_changes
            for buyer_changes in changes
            for four_consecutive_changes in buyer_changes
        }
    )
)
