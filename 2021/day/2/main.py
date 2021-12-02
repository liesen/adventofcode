from functools import partial, reduce
from fileinput import input

def step1(pos, line):
    match (pos, line.split(' ')):
        case ((hpos, vpos), ["forward", x]): return (hpos + int(x), vpos)
        case ((hpos, vpos), ["down", x]): return (hpos, vpos + int(x))
        case ((hpos, vpos), ["up", x]): return (hpos, vpos - int(x))

def step2(pos, line):
  match (pos, line.split(' ')):
      case ((hpos, vpos, aim), ["forward", x]):
         return (hpos + int(x), vpos + aim * int(x), aim)
      case ((hpos, vpos, aim), ["down", x]):
          return (hpos, vpos, aim + int(x))
      case ((hpos, vpos, aim), ["up", x]):
          return (hpos, vpos, aim - int(x))

with input() as f:
    pos1 = (0, 0)
    pos2 = (0, 0, 0)

    for line in f:
        pos1 = step1(pos1, line.strip())
        pos2 = step2(pos2, line.strip())

print(pos1[0] * pos1[1])
print(pos2[0] * pos2[1])
