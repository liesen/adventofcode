from functools import cache

L = [ln.rstrip() for ln in open(0)]
available_towel_patterns = L[0].split(", ")
desired_designs = L[2:]


# Part 1
def match(design: str) -> bool:
    @cache
    def dfs(i: int) -> bool:
        if i == len(design):
            return True

        return any(
            dfs(i + len(towel))
            for towel in available_towel_patterns
            if design[i:].startswith(towel)
        )

    return dfs(0)


print(sum(match(design) for design in desired_designs))


# Part 2


def count(design: str) -> int:
    @cache
    def dfs(i: int) -> int:
        if i == len(design):
            return 1

        return sum(
            dfs(i + len(w))
            for w in available_towel_patterns
            if design[i:].startswith(w)
        )

    return dfs(0)


print(sum(count(design) for design in desired_designs))
