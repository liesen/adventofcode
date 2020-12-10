import fileinput
from collections import Counter

adapters = [int(x) for x in fileinput.input('input.txt')]
output = max(adapters) + 3


jolts = sorted([0, output] + adapters)
#print(ys)

d1, d3 = 0, 0

for i in range(len(jolts) - 1):
    d = jolts[i + 1] - jolts[i] 
    
    if d == 1:
        d1 += 1
    elif d == 3:
        d3 += 1

print(d1 * d3)

#
def part2(jolts):
    DP = {}

    def dp(i):
        if i == len(jolts) - 1:
            return 1

        if i in DP:
            return DP[i]
        
        n = 0
        j = i + 1

        while j < len(jolts) and jolts[j] - jolts[i] <= 3:
            n += dp(j)
            j += 1

        DP[i] = n
        return n

    return dp(0)
    
print(part2(jolts))

# low
# 12089663946752
# 13816758796288