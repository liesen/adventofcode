def parse_line(ln: str):
    # Parse sensor
    ix = len("Sensor at x=")
    jx = ln.index(',', ix)
    x = int(ln[ix:jx])
    iy = jx + len(", y=")
    jy = ln.index(':', iy)
    y = int(ln[iy:jy])
    sensor = (x, y)
    # Parse beacon
    ix = jy + len(": closest beacon is at x=")
    jx = ln.index(',', ix)
    x = int(ln[ix:jx])
    iy = jx + len(", y=")
    jy = len(ln)
    y = int(ln[iy:jy])
    nearest_beacon = (x, y)
    return (sensor, nearest_beacon)

with open("input.txt") as f:
    sensors_and_beacons = [parse_line(ln.rstrip()) for ln in f]
    yy = 2000000

# Part 1
intervals = []  # Intervals of x coordinates on row yy for each sensor

for sensor, beacon in sensors_and_beacons:
    sensor_x, sensor_y = sensor
    beacon_x, beacon_y = beacon
    # all x s.t. dist(sensor, (x, yy)) <= dist(sensor, beacon)
    # |sensor_x - x| + |sensor_y - yy| <= |sensor_x - beacon_x| + |sensor_y - beacon_y|
    # |sensor_x - x| <= |sensor_x - beacon_x| + |sensor_y - beacon_y| - |sensor_y - yy|
    # sensor_x - x <= dx <= sensor_x + x
    # x1 = sensor_x - dx, x2 = sensor_x + dx
    dx = abs(sensor_x - beacon_x) + abs(sensor_y - beacon_y) - abs(sensor_y - yy)

    if dx < 0:
        # Row yy not covered by beacon signal
        continue

    intervals.append((sensor_x - dx, sensor_x + dx))

# Merge intervals
(lo1, hi1), *ss = sorted(intervals)
merged_intervals = []

while ss:
    (lo2, hi2), *ss = ss

    if hi1 < lo2:
        merged_intervals.append((lo1, hi1))
        lo1, hi1 = lo2, hi2
    elif hi2 > hi1:
        hi1 = hi2

merged_intervals.append((lo1, hi1))
print(sum(hi - lo for lo, hi in merged_intervals))

# Part 2
import z3

solver = z3.Solver()

distress_beacon_x = z3.Int("x")
distress_beacon_y = z3.Int("y")

solver.add(
    distress_beacon_x >= 0,
    distress_beacon_x <= 4000000,
)
solver.add(
    distress_beacon_y >= 0,
    distress_beacon_y <= 4000000,
)

def z3abs(a):
    return z3.If(a < 0, -a, a)

for sensor, beacon in sensors_and_beacons:
    sensor_x, sensor_y = sensor
    beacon_x, beacon_y = beacon
    closest_beacon_dist = abs(sensor_y - beacon_y) + abs(sensor_x - beacon_x)
    distress_beacon_dist = (
        z3abs(sensor_y - distress_beacon_y)
        + z3abs(sensor_x - distress_beacon_x)
    )
    solver.add(closest_beacon_dist < distress_beacon_dist)

assert solver.check() == z3.sat
model = solver.model()
print(
    model[distress_beacon_x].as_long() * 4000000
    + model[distress_beacon_y].as_long()
)

