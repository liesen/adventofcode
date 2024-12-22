secrets = [int(ln.rstrip()) for ln in open(0)]


def mix(value, secret):
    return value ^ secret


def prune(secret):
    return secret % 16777216


def evolve(secret):
    secret = prune(mix(secret, secret * 64))
    secret = prune(mix(secret, secret // 32))
    secret = prune(mix(secret, secret * 2048))
    return secret


def generate(n, secret):
    secrets = [secret] * (n + 1)
    i = 1

    while i <= n:
        secrets[i] = evolve(secrets[i - 1])
        i += 1

    return secrets


# Part 1
ans1 = 0

for secret in secrets:
    ans1 += generate(2000, secret)[2000]

print(ans1)

# Part 2
changes: list[dict[tuple[int, int, int, int], int]] = []

for secret in secrets:
    prices = list(map(lambda p: p % 10, generate(2000, secret)))
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
