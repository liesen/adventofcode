import string
from collections import deque

with open('input.txt') as fp:
    S = fp.readlines()

maxrow = len(S)
maxcol = len(S[0])

_portals = []

for r in range(maxrow):
    for c in range(maxcol):
        z = S[r][c]
        
        if z == '.':
            if r > 1:
                if S[r - 2][c] in string.ascii_uppercase and \
                    S[r - 1][c] in string.ascii_uppercase:
                    _portals.append((S[r - 2][c] + S[r - 1][c], (r, c)))

            if r < maxrow - 1:
                if S[r + 1][c] in string.ascii_uppercase and \
                    S[r + 2][c] in string.ascii_uppercase:
                    _portals.append((S[r + 1][c] + S[r + 2][c], (r, c)))
                    
            if c > 1:
                if S[r][c - 2] in string.ascii_uppercase and \
                    S[r][c - 1] in string.ascii_uppercase:
                    _portals.append((S[r][c - 2] + S[r][c - 1], (r, c)))

            if c < maxcol - 1:
                if S[r][c + 1] in string.ascii_uppercase and \
                    S[r][c + 2] in string.ascii_uppercase:
                    _portals.append((S[r][c + 1] + S[r][c + 2], (r, c)))

start = next(p for s, p in _portals if s == 'AA')
end = next(p for s, p in _portals if s == 'ZZ')
portals = {p: s for s, p in _portals if s not in ['AA', 'ZZ']}


# Part 1
q = deque([(start, 0)])
visited = set()
paths = []

while q:
    p, path_len = q.popleft()
    
    if p in visited:
        continue
        
    visited.add(p)
        
    if p == end:
        break
        
    if p in portals:
        p, s = next((q, s) for q, s in portals.items() if s == portals[p] and p != q)
        path_len += 1
        
    r, c = p
    
    for dr, dc in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
        rr, cc = pp = (r + dr, c + dc)
        
        if not (0 <= rr < maxrow and 0 <= cc < maxcol):
            continue
        
        if S[rr][cc] != '.':
            continue
    
        q.append((pp, path_len + 1))

print(path_len)
