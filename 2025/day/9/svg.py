with open(0) as f:
    point_strs = [ln.rstrip() for ln in f]

points = [tuple(map(int, p.split(","))) for p in point_strs]
xmin = min(x for x, y in points)
xmax = max(x for x, y in points)
ymin = min(y for x, y in points)
ymax = max(y for x, y in points)
xpad = max(1, (xmax - xmin) // 20)  # 5%
ypad = max(1, (ymax - ymin) // 20)  # 5%

print(
    f'<svg viewBox="{xmin - xpad} {ymin - ypad} {xmax + xpad} {ymax + ypad}" xmlns="http://www.w3.org/2000/svg">'
)
print('  <polygon points="', end="")
print(" ".join(point_strs), end="")
print('" />')

for x, y in points:
    print(f'  <circle cx="{x}" cy="{y}" r="3" fill="red" stroke="red" />')

print("</svg>")
