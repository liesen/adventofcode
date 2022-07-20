import heapq
from functools import cache

# Tools
NEITHER = 0
TORCH = 1
CLIMBING_GEAR = 2
GEARS = {0: 'neither', 1: 'torch', 2: 'climbing gear'}

# Region types
ROCKY = 0
WET = 1
NARROW = 2
REGIONS = {0: 'rocky', 1: 'wet', 2: 'narrow'}

# Problem
depth = 3879
target_x, target_y = 8, 713


def normalize(x):
    return (x + depth) % 20183

@cache
def get_erosion_level(x, y):
    if x == 0 and y == 0:
        return normalize(0)

    if x == target_x and y == target_y:
        return normalize(0)

    if x == 0:
        return normalize(y * 48271)

    if y == 0:
        return normalize(x * 16807)

    a = get_erosion_level(x - 1, y)
    b = get_erosion_level(x, y - 1)
    z = normalize(a * b)
    return z

def risk_level(x, y):
    return get_erosion_level(x, y) % 3

region_type = risk_level

assert region_type(0, 0) == ROCKY, "region type at mouth should be rocky"
assert region_type(target_x, target_y) == ROCKY, "region type at target should be rocky"


# Part 1
ans1 = sum(risk_level(x, y) for x in range(target_x + 1) for y in range(target_y + 1))
print(ans1)

# Part 2
def get_available_tools(region_type):
    if region_type == ROCKY:
        return {TORCH, CLIMBING_GEAR}
    
    if region_type == WET:
        return {NEITHER, CLIMBING_GEAR}
    
    if region_type == NARROW:
        return {NEITHER, TORCH}

    raise Exception("unknown region type: " + region_type)


queue = []
heapq.heapify(queue)
heapq.heappush(queue, (0, 0, 0, TORCH))
seen = set()

while queue:
    cost, x, y, gear = heapq.heappop(queue)

    if (x, y, gear) in seen:
        continue

    seen.add((x, y, gear))

    if x == target_x and y == target_y and gear == TORCH:
        ans2 = cost
        break

    for dx, dy in [(0, -1), (0, 1), (-1, 0), (1, 0)]:
        new_x, new_y = x + dx, y + dy

        if new_x < 0 or new_y < 0:
            continue

        # new_gear must also be valid for current region
        for new_gear in get_available_tools(region_type(x, y)) & get_available_tools(region_type(new_x, new_y)):
            new_cost = cost + 1

            if new_gear != gear:
                new_cost += 7
            
            heapq.heappush(queue, (new_cost, new_x, new_y, new_gear))


print(ans2)
