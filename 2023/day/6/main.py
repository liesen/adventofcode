import math

# Time:        53     89     76     98
# Distance:   313   1090   1214   1201


def dist(t, T):
    return t * (T - t)


def count_wins1(T, W):
    ans = 0

    for t in range(T + 1):
        if t * (T - t) > W:
            ans += 1

    return ans


# Part 1
print(
    count_wins1(53, 313)
    * count_wins1(89, 1090)
    * count_wins1(76, 1214)
    * count_wins1(98, 1201)
)


def count_wins2(T, D):
    t0 = int(T / 2 - math.sqrt((-T / 2) ** 2 - D))

    # Adjust for integers
    while dist(t0, T) < D:
        t0 += 1

    t1 = int(T / 2 + math.sqrt((-T / 2) ** 2 - D))

    while dist(t1, T) > D:
        t1 += 1

    return t1 - t0


print(count_wins2(53897698, 313109012141201))
