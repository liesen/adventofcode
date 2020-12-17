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

for y, line in enumerate(fileinput.input('input.txt')):
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

cc4 = ConwayHypercubes((llx, lly, 0, 0), (urx, ury, 0, 0), active4)
print(len(cc4.step().step().step().step().step().step().active))
