from intcode import Intcode

with open('input.txt') as fp:
    prog = [int(i) for line in fp for i in line.split(',')]

p = Intcode([109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])
p.run()
assert p.halt
assert list(p.outputs) == p.prog

p = Intcode([1102,34915192,34915192,7,4,7,99,0])
p.run()
assert p.halt
assert len(str(p.outputs.pop())) == 16

p = Intcode([104,1125899906842624,99])
p.run()
assert p.halt
assert p.outputs.pop() == p.prog[1]

p = Intcode(prog)
p.run([1])
assert p.halt
print(p.outputs)