import string
from collections import deque, namedtuple


with open('input.txt') as fp:
    G = [list(row) for row in fp.read().splitlines()]


S = '''             Z L X W       C                 
             Z P Q B       K                 
  ###########.#.#.#.#######.###############  
  #...#.......#.#.......#.#.......#.#.#...#  
  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  
  #.#...#.#.#...#.#.#...#...#...#.#.......#  
  #.###.#######.###.###.#.###.###.#.#######  
  #...#.......#.#...#...#.............#...#  
  #.#########.#######.#.#######.#######.###  
  #...#.#    F       R I       Z    #.#.#.#  
  #.###.#    D       E C       H    #.#.#.#  
  #.#...#                           #...#.#  
  #.###.#                           #.###.#  
  #.#....OA                       WB..#.#..ZH
  #.###.#                           #.#.#.#  
CJ......#                           #.....#  
  #######                           #######  
  #.#....CK                         #......IC
  #.###.#                           #.###.#  
  #.....#                           #...#.#  
  ###.###                           #.#.#.#  
XF....#.#                         RF..#.#.#  
  #####.#                           #######  
  #......CJ                       NM..#...#  
  ###.#.#                           #.###.#  
RE....#.#                           #......RF
  ###.###        X   X       L      #.#.#.#  
  #.....#        F   Q       P      #.#.#.#  
  ###.###########.###.#######.#########.###  
  #.....#...#.....#.......#...#.....#.#...#  
  #####.#.###.#######.#######.###.###.#.#.#  
  #.......#.......#.#.#.#.#...#...#...#.#.#  
  #####.###.#####.#.#.#.#.###.###.#.###.###  
  #.......#.....#.#...#...............#...#  
  #############.#.#.###.###################  
               A O F   N                     
               A A D   M                     
'''

# G = [list(row) for row in S.splitlines()]


maxrow = len(G)
maxcol = len(G[0])

Label = namedtuple('Label', ['pos', 'id'])
labels = {}
E = {}

for r in range(maxrow):
    for c in range(maxcol):
        if G[r][c] == '.':
            for dr, dc in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
                if G[r + dr][c + dc] == '.':
                    E.setdefault((r, c), set()).add(((r + dr, c + dc), 1))
                    E.setdefault((r + dr, c + dc), set()).add(((r, c), 1))

            if (r > 1 and
                    'A' <= G[r - 2][c] <= 'Z' and 'A' <= G[r - 1][c] <= 'Z'):
                labels[(r, c)] = Label((r, c), G[r - 2][c] + G[r - 1][c])

            if (r < maxrow - 1 and
                    'A' <= G[r + 1][c] <= 'Z' and 'A' <= G[r + 2][c] <= 'Z'):
                labels[(r, c)] = Label((r, c), G[r + 1][c] + G[r + 2][c])

            if (c > 1 and
                    'A' <= G[r][c - 2] <= 'Z' and 'A' <= G[r][c - 1] <= 'Z'):
                labels[(r, c)] = Label((r, c), G[r][c - 2] + G[r][c - 1])

            if (c < maxcol - 1 and
                    'A' <= G[r][c + 1] <= 'Z' and 'A' <= G[r][c + 2] <= 'Z'):
                labels[(r, c)] = Label((r, c), G[r][c + 1] + G[r][c + 2])

start = next(p.pos for p in labels.values() if p.id == 'AA')
end = next(p.pos for p in labels.values() if p.id == 'ZZ')

# Add edges for portals (not to E because of part 2)
EP = {}

for p in labels.values():
    if p.id in ['AA', 'ZZ']:
        continue

    for q in labels.values():
        if q.id in ['AA', 'ZZ']:
            continue

        if q.id == p.id and q.pos != p.pos:
            EP[p.pos] = q.pos
            EP[q.pos] = p.pos

# Compress graph by finding the shortest path between all portals
if False:
    E = {}

    for src in labels:
        q = deque([(src, 0)])
        visited = set()

        while q:
            p, n = q.popleft()
            r, c = p

            if G[r][c] != '.':
                continue
            
            if p in visited:
                continue

            visited.add(p)

            if p != src and p in labels:
                E.setdefault(src, set()).add((p, n))
                continue

            for dr, dc in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
                q.append(((r + dr, c + dc), n + 1))

# Part 1
q = deque([(start, 0)])
visited = set()

while q:
    p, path_len = q.popleft()
    
    if p in visited:
        continue
        
    visited.add(p)
        
    if p == end:
        break

    for pp, n in E[p]:
        q.append((pp, path_len + n))

    if p in EP:
        q.append((EP[p], path_len + 1))

print(path_len)

# Part 2
q = deque([(start, 0, 0)])
visited = set()

while q:
    p, level, path_len = q.popleft()
    r, c = p

    if p == end and level == 0:
        break

    if (r, c, level) in visited:
        continue

    visited.add((r, c, level))

    for qq, n in E[p]:
        q.append((qq, level, path_len + n))
    
    if p in EP:
        if r == 2 or r == maxrow - 3 or c == 2 or c == maxcol - 3:
            if level > 0:
                q.append((EP[p], level - 1, path_len + 1))
        else:
            q.append((EP[p], level + 1, path_len + 1))

print(path_len)