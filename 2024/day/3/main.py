import re


def eval1(s: str) -> int:
    return sum(int(a) * int(b) for a, b in re.findall(r"mul\((\d{1,3}),(\d{1,3})\)", s))


def eval2(s: str) -> int:
    do = True
    ans = 0

    for op in re.findall(r"mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)", s):
        if op == "do()":
            do = True
        elif op == "don't()":
            do = False
        elif do:
            ans += eval1(op)

    return ans


with open("input") as f:
    S = f.read()


ans1 = eval1(S)
print(ans1)

ans2 = eval2(S)
print(ans2)
