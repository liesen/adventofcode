from functools import cache

input = """aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out"""
lines = input.splitlines()

with open(0) as f:
    lines = [ln.rstrip() for ln in f]

G = {ln[0 : ln.index(":")]: set(ln[ln.index(":") + 1 :].split()) for ln in lines}


# Part 1
@cache
def count_paths_out(u: str) -> int:
    if u == "out":
        return 1

    return sum(count_paths_out(v) for v in G.get(u, set()))


print(count_paths_out("you"))


# Part 2
@cache
def count_paths_out_via_dac_and_fft(
    u: str, dac: bool = False, fft: bool = False
) -> int:
    if u == "out" and dac and fft:
        return 1

    return sum(
        count_paths_out_via_dac_and_fft(v, dac or u == "dac", fft or u == "fft")
        for v in G.get(u, set())
    )


print(count_paths_out_via_dac_and_fft("svr"))
