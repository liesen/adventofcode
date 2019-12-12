import numpy as np


class Moon:
    def __init__(self, pos, vel=None):
        self.pos = pos
        
        if vel is None:
            self.vel = np.array([0, 0, 0])
        else:
            self.vel = vel
            
    @property
    def pot(self):
        return np.sum(np.abs(self.pos))
    
    @property
    def kin(self):
        return np.sum(np.abs(self.vel))
        
    def __repr__(self):
        return f'pos=<x={self.pos[0]},y={self.pos[1]},z={self.pos[2]}>, vel=<x={self.vel[0]},y={self.vel[1]},z={self.vel[2]}>, pot={self.pot}, kin={self.kin}'


moons = [Moon(np.array([1, -4, 3])),
         Moon(np.array([-14, 9, -4])),
         Moon(np.array([-4, -6, 7])),
         Moon(np.array([6, -9, -11]))]

# Part 1
for i in range(1000):
    moons = [Moon(a.pos + vel, vel)
             for a in moons
             for vel in [a.vel + np.sum([np.clip(b.pos - a.pos, -1, 1) for b in moons if a != b], axis=0)]]

print(sum(m.pot * m.kin for m in moons))