from collections import deque, namedtuple
from itertools import permutations
from functools import reduce
from intcode import Intcode

with open('input.txt') as fp:
    prog = [int(i) for line in fp for i in line.split(',')]

# Part 1
def feedback(input_signal, amp):
    amp.run([input_signal])
    return amp.outputs.pop()

print(max(reduce(feedback, [Intcode(prog, [phase]) for phase in phases], 0) for phases in permutations(range(5))))

# Part 2
def feedback_loop(input_signal, amps):
    while not all(amp.halt for amp in amps):
        input_signal = output_signal = reduce(feedback, amps, input_signal)

    return output_signal

print(max(feedback_loop(0, [Intcode(prog, [phase]) for phase in phases]) for phases in permutations(range(5, 10))))