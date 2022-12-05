import re


ans1 = 0
ans2 = 0

with open("input.txt") as f:
    for ln in f:
        lo1, hi1, lo2, hi2 = map(int, re.findall(r"\d+", ln))
        ans1 += (lo1 <= lo2 and hi1 >= hi2) or (lo2 <= lo1 and hi2 >= hi1)
        ans2 += (lo1 <= lo2 <= hi1) or (lo2 <= lo1 <= hi2)
        
print(ans1)
print(ans2)