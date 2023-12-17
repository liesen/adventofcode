import heapq

grid = {
    (r, c): int(ch) for r, row in enumerate(open("input")) for c, ch in enumerate(row.rstrip())
}

maxrow = max(r for r, c in grid)
maxcol = max(c for r, c in grid)

# Part 1
seen = set()
q = [(0, dr, dc, dr, dc, 1) for dr, dc in [(1, 0), (0, 1)]]
heapq.heapify(q)

while q:
    heat, r, c, dr, dc, streak = heapq.heappop(q)
    
    if streak >= 3:
        continue
    
    if not (h := grid.get((r, c))):
        continue

    if (r, c) == (maxrow, maxcol):
        print(heat + h)  # Part 1 answer
        break
    
    if (r, c, dr, dc, streak) in seen:
        continue
    
    seen.add((r, c, dr, dc, streak))
    
    heapq.heappush(q, (heat + h, r + dr, c + dc, dr, dc, streak + 1))
    heapq.heappush(q, (heat + h, r + dc, c - dr, dc, -dr, 0))
    heapq.heappush(q, (heat + h, r - dc, c + dr, -dc, dr, 0))
        
# Part 2
seen = set()
q = [(0, dr, dc, dr, dc, 1) for dr, dc in [(1, 0), (0, 1)]]
heapq.heapify(q)

while q:
    heat, r, c, dr, dc, streak = heapq.heappop(q)
    
    if streak >= 10:
        continue
    
    if not (h := grid.get((r, c))):
        continue

    if (r, c) == (maxrow, maxcol) and streak >= 3:
        print(heat + h)  # Part 2 answer
        break
    
    if (r, c, dr, dc, streak) in seen:
        continue
    
    seen.add((r, c, dr, dc, streak))
    
    heapq.heappush(q, (heat + h, r + dr, c + dc, dr, dc, streak + 1))
    
    if streak >= 3:
        heapq.heappush(q, (heat + h, r + dc, c - dr, dc, -dr, 0))
        heapq.heappush(q, (heat + h, r - dc, c + dr, -dc, dr, 0))