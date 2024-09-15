from math import prod
import fileinput
import z3


with fileinput.input("input.txt") as f:
    ingredients: list[list[int]] = [
        [
            int(prop_str[prop_str.rindex(" ") :].rstrip())
            for prop_str in ln[ln.index(": ") + len(": ") :].split(", ")
        ]
        for ln in f
    ]

# Example
# ingredients = [[-1, -2, 6, 3, 8], [2, 3, -2, -1, 3]]
capacities, durabilities, flavors, textures, calories = zip(*ingredients)

opt = z3.Optimize()

# Track how much of each ingredient we use
amounts = z3.IntVector("amounts", len(ingredients))

# Amount of each ingredient must be positive and sum up to 100
for amt in amounts:
    opt.add(amt >= 0)

opt.add(sum(amounts) == 100)

# Find the optimal cookie
cookie_score = prod(
    z3.If(signed_propscore > 0, signed_propscore, 0)
    for props in [capacities, durabilities, flavors, textures]
    for signed_propscore in (sum(prop * amt for prop, amt in zip(props, amounts)),)
)
opt.maximize(cookie_score)
assert opt.check() == z3.sat
print(opt.model().eval(cookie_score))

# Part 2
opt.push()

# Allow just 500 calories :(
opt.add(sum(prop * amt for prop, amt in zip(calories, amounts)) == 500)

opt.maximize(cookie_score)
assert opt.check() == z3.sat
print(opt.model().eval(cookie_score))
