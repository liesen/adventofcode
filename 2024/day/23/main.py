import networkx as nx

G = nx.Graph()

for ln in open(0):
    i = ln.index("-")
    k = ln[:i]
    v = ln[i + 1 :].rstrip()
    G.add_edge(k, v)

ans1 = len(
    {
        ",".join(sorted([u, v, w]))
        for u in G
        if u.startswith("t")
        for v in G[u]
        for w in G[v]
        if u in G[w]
    }
)

print(ans1)

# Part 2
max_clique = max(nx.find_cliques(G), key=len)
print(",".join(sorted(max_clique)))
