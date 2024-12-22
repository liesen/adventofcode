def mix(value, secret):
    return value ^ secret


def prune(secret):
    return secret % 16777216


def evolve(secret):
    secret = prune(mix(secret, secret * 64))
    secret = prune(mix(secret, secret // 32))
    secret = prune(mix(secret, secret * 2048))
    return secret


# Part 1
ans1 = 0

for ln in open(0):
    secret = int(ln.rstrip())
    n = 0

    while n < 2000:
        secret = evolve(secret)
        n += 1

    ans1 += secret

print(ans1)
