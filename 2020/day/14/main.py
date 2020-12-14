import fileinput

N = 36

class Mem1:
    def __init__(self, and_mask = 0, or_mask = 0):
        self.mem = {}
        self.and_mask = and_mask
        self.or_mask = or_mask

    def set_mask(self, mask):
        self.and_mask = 0
        self.or_mask = 0

        for i, x in enumerate(mask):
            if x == '0':
                pass
            elif x == '1':
                self.or_mask |= 1 << (N - i - 1)
            elif x == 'X':
                self.and_mask |= 1 << (N - i - 1)

    def sum_values(self):
        return sum(self.mem.values())

    def __setitem__(self, key, value):
        self.mem[key] = (value & self.and_mask) | self.or_mask


class Mem2:
    def __init__(self, mask = None):
        self.mem = {}
        self.mask = mask

    def sum_values(self):
        return sum(self.mem.values())

    def set_mask(self, mask):
        self.mask = mask

    def apply_mask(self, value):
        values = set([value])

        for i in range(N):
            new_values = set()
            x = self.mask[i]

            for v in values:
                if x == '0':
                    new_values.add(v)
                elif x == '1':
                    new_values.add(v | (1 << (N - i - 1)))
                elif x == 'X':
                    new_values.add(v & ~(1 << (N - i - 1)))
                    new_values.add(v | (1 << (N - i - 1)))

            values = new_values

        return values

    def __setitem__(self, key, value):
        for k in self.apply_mask(key):
            self.mem[k] = value


with fileinput.input('input.txt') as f:
    mem1 = Mem1()
    mem2 = Mem2()

    for line in f:
        if line.startswith('mask = '):
            mask = line[len('mask = '):-1]
            mem1.set_mask(mask)
            mem2.set_mask(mask)
        else:
            exec(line, None, {'mem': mem1})
            exec(line, None, {'mem': mem2})

# Part 1
print(mem1.sum_values())

# Part 2
print(mem2.sum_values())