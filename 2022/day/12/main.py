from collections import deque


with open("input.txt") as f:
    G = {
        (r, c): x
        for r, ln in enumerate(f)
        for c, x in enumerate(ln.rstrip())
    }
    
    S = next(p for p, x in G.items() if x == 'S')
    G[S] = 'a'
    E = next(p for p, x in G.items() if x == 'E')
    G[E] = 'z'
    
    G = {p: ord(x) - ord('a') for p, x in G.items()}

def find_path_length(G, S, E):
    queue = deque([(S, 0)])
    seen = set()

    while queue:
        p, n = queue.popleft()

        if p in seen:
            continue

        seen.add(p)
        
        if p == E:
            return n
        
        for dr, dc in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            r, c = p
            q = (r + dr, c + dc)

            if q in G and G[q] - G[p] <= 1: 
                queue.append((q, n + 1))

# Part 1
print(find_path_length(G, S, E))

# Part 2
print(
    min(
        n
        for s in G
        if G[s] == 0
        for n in [find_path_length(G, s, E)]
        if n is not None
    )
)