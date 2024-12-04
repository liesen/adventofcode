from itertools import pairwise


with open("input") as f:
    reports = [[int(n) for n in ln.strip().split()] for ln in f]


def safe1(levels: list[int]) -> bool:
    diffs = [a - b for a, b in pairwise(levels)]
    increasing = all(-3 <= d <= -1 for d in diffs)
    decreasing = all(1 <= d <= 3 for d in diffs)
    return increasing or decreasing


def safe2(levels: list[int]) -> bool:
    return any(safe1(levels[:i] + levels[i + 1 :]) for i in range(len(levels)))


ans1 = sum(safe1(report) for report in reports)
print(ans1)

ans2 = sum(safe2(report) for report in reports)
print(ans2)
