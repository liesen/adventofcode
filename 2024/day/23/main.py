from collections import defaultdict
from collections.abc import Generator
import networkx as nx

N: dict[str, set[str]] = defaultdict(set)

for ln in open(0):
    i = ln.index("-")
    k = ln[:i]
    v = ln[i + 1 :].rstrip()
    N[k].add(v)
    N[v].add(k)

ans1 = len(
    {
        ",".join(sorted([u, v, w]))
        for u in N
        if u.startswith("t")
        for v in N[u]
        for w in N[v]
        if u in N[w]
    }
)

print(ans1)

# Part 2 using networkx
max_clique = max(
    nx.find_cliques(nx.Graph((k, v) for k, vs in N.items() for v in vs)), key=len
)
ans2 = ",".join(sorted(max_clique))
print(ans2)


# Part 2 using the Bron-Kerbosch algorithm: https://rosettacode.org/wiki/Bron%E2%80%93Kerbosch_algorithm
def bron_kerbosch(
    current_clique: set[str],
    potential_candidates: set[str],
    excluded_vertices: set[str],
) -> Generator[set[str]]:
    if not potential_candidates and not excluded_vertices:
        if len(current_clique) > 2:
            yield current_clique

        return

    pivot_vertex = max(
        potential_candidates | excluded_vertices, key=lambda v: len(N[v])
    )

    for v in potential_candidates - N[pivot_vertex]:
        yield from bron_kerbosch(
            current_clique | {v}, potential_candidates & N[v], excluded_vertices & N[v]
        )
        potential_candidates.remove(v)
        excluded_vertices.add(v)


max_clique = max(bron_kerbosch(set(), set(N.keys()), set()), key=len)
ans2 = ",".join(sorted(max_clique))
print(ans2)
