from dataclasses import dataclass


@dataclass
class Snek:
    headx: int
    heady: int
    tailx: int
    taily: int
    
    @property
    def head(self):
        return (self.heady, self.headx)

    @property
    def tail(self):
        return (self.taily, self.tailx)
    
    def move(self, motion):
        stepdir, _steplen = motion.split(" ", maxsplit=2)
        steplen = int(_steplen)
        
        hx, hy = self.headx, self.heady
        tx, ty = self.tailx, self.taily
        
        match stepdir:
            case "U": 
                dx, dy = 0, -1
            case "D":
                dx, dy = 0, 1
            case "L":
                dx, dy = -1, 0
            case "R":
                dx, dy = 1, 0

        for _ in range(steplen):
            hx += dx
            hy += dy
            
            if abs(hx - tx) > 1:
                tx += dx
                ty = hy

            if abs(hy - ty) > 1:
                tx = hx
                ty += dy
                
            assert 0 <= abs(hx - tx) <= 1
            assert 0 <= abs(hy - ty) <= 1
            yield Snek(hx, hy, tx, ty)


assert next(Snek(0, 0, 0, 0).move("R 1")) == Snek(1, 0, 0, 0), "R 1"
assert next(next(Snek(0, 0, 0, 0).move("R 1")).move("L 1")) == Snek(0, 0, 0, 0), "R 1 . L 1 == id"
assert next(Snek(1, 0, 0, 0).move("R 1")) == Snek(2, 0, 1, 0)
assert next(Snek(1, 0, 0, 0).move("L 1")) == Snek(0, 0, 0, 0)
assert next(Snek(2, 1, 1, 0).move("D 1")) == Snek(2, 2, 2, 1)
assert next(Snek(2, 1, 1, 0).move("L 1")) == Snek(1, 1, 1, 0)


with open("input.txt") as f:
    motions = [ln.strip() for ln in f]

snek = Snek(0, 0, 0, 0)
seen = set()
seen.add(snek.tail)

for motion in motions:
    for snek in snek.move(motion):
        seen.add(snek.tail)
        
# Part 1
print(len(seen))