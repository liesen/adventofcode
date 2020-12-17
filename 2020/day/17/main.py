from dataclasses import dataclass
from typing import Tuple, Set
import fileinput


def adj3(p):
    x, y, z = p

    for dx in [-1, 0, 1]:
        for dy in [-1, 0, 1]:
            for dz in [-1, 0, 1]:
                if not (dx == 0 and dy == 0 and dz == 0):
                    yield (x + dx, y + dy, z + dz)

assert len(set(adj3((0, 0, 0)))) == 26

@dataclass
class ConwayCubes:
    ll: Tuple[int, int, int]
    ur: Tuple[int, int, int]
    active: Set[Tuple[int, int, int]]

    def step(self):
        llx, lly, llz = self.ll
        urx, ury, urz = self.ur
        new_active = set()

        for x in range(llx - 1, urx + 2):
            for y in range(lly - 1, ury + 2):
                for z in range(llz - 1, urz + 2):
                    p = (x, y, z)
                    k = sum(1 for n in adj3(p) if n in self.active)

                    if p in self.active:
                        if 2 <= k <= 3:
                            new_active.add(p)
                    else:
                        if k == 3:
                            new_active.add(p)

        return ConwayCubes(ll=(llx - 1, lly - 1, llz - 1),
                           ur=(urx + 1, ury + 1, urz + 1),
                           active=new_active)


active3 = set()
llx, lly, urx, ury = 0, 0, 0, 0

for y, line in enumerate(fileinput.input('testinput.txt')):
    for x, ch in enumerate(line.strip()):
        llx = min(x, llx)
        lly = min(y, lly)
        urx = max(x, urx)
        ury = max(y, ury)

        if ch == '#':
            active3.add((x, y, 0))

# Part 1
cc3 = ConwayCubes((llx, lly, 0), (urx, ury, 0), active3)
print(len(cc3.step().step().step().step().step().step().active))

# Part 2
active4 = set((x, y, z, 0) for x, y, z in active3)

def adj4(p):
    x, y, z, w = p

    for dx in [-1, 0, 1]:
        for dy in [-1, 0, 1]:
            for dz in [-1, 0, 1]:
                for dw in [-1, 0, 1]:
                    if not (dx == 0 and dy == 0 and dz == 0 and dw == 0):
                        yield (x + dx, y + dy, z + dz, w + dw)

assert len(set(adj4((0, 0, 0, 0)))) == 80

@dataclass
class ConwayHypercubes:
    ll: Tuple[int, int, int, int]
    ur: Tuple[int, int, int, int]
    active: Set[Tuple[int, int, int, int]]

    def step(self):
        llx, lly, llz, llw = self.ll
        urx, ury, urz, urw = self.ur
        new_active = set()

        for x in range(llx - 1, urx + 2):
            for y in range(lly - 1, ury + 2):
                for z in range(llz - 1, urz + 2):
                    for w in range(llw - 1, urw + 2):
                        p = (x, y, z, w)
                        k = sum(1 for n in adj4(p) if n in self.active)

                        if p in self.active:
                            if 2 <= k <= 3:
                                new_active.add(p)
                        else:
                            if k == 3:
                                new_active.add(p)

        return ConwayHypercubes(
            ll=(llx - 1, lly - 1, llz - 1, llw - 1),
            ur=(urx + 1, ury + 1, urz + 1, urw + 1),
            active=new_active)

    # This function only considers neighbors + neighbors when
    # evolving, which is much more efficient
    def faststep(self):
        new_active = set()
        seen = set()

        for p in self.active | set(q for p in self.active for q in adj4(p)):
            if p in seen:
                continue

            seen.add(p)
            k = sum(1 for q in adj4(p) if q in self.active)

            if p in self.active and 2 <= k <= 3:
                new_active.add(p)
            elif p not in self.active and k == 3:
                new_active.add(p)

        return ConwayHypercubes(ll=None, ur=None, active=new_active)

cc4 = ConwayHypercubes((llx, lly, 0, 0), (urx, ury, 0, 0), active4)
# print(len(cc4.step().step().step().step().step().step().active))
print(len(cc4.faststep().faststep().faststep().faststep().faststep().faststep().active))

# Experimental Conway Cubes where the coordinates
# are of any dimension represented by lists. More
# general but slower than the specialized versions.
@dataclass
class ConwayAnycubes:
    dim: int  # Coordinate dimensions
    size: int  # Size of the grid
    active: Set[object]  # Set of active cubes

    @staticmethod
    def neighbors(p):
        def go(q, i):
            if i == -1:
                yield q
                return

            for d in [-1, 0, 1]:
                q[i] = p[i] + d

                for q in go(q, i - 1):
                    yield q

        for q in go([0] * len(p), len(p) - 1):
            if p != q:
                yield q

    def step(self):
        def go(p, i):
            if i == -1:
                k = sum(1 for q in self.neighbors(p) if tuple(q) in self.active)

                if tuple(p) in self.active and 2 <= k <= 3:
                    yield p
                elif tuple(p) not in self.active and k == 3:
                    yield p

                return

            for d in range(-self.size - 1, self.size + 2):
                p[i] = d
                
                for q in go(p, i - 1):
                    yield q

        return ConwayAnycubes(dim=self.dim, size=self.size + 1,
                              active=set(tuple(q) for q in go([0] * self.dim, self.dim - 1)))

# print(len(ConwayAnycubes(dim=4, size=3, active=active4).step().step().step().step().step().step().active))
