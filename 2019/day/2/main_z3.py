import z3


s = z3.Solver()

verb = z3.Int("verb")
s.add(verb >= 0, verb < 99)
noun = z3.Int("noun")
s.add(noun >= 0, noun < 99)

with open("input.txt") as f:
    prog = [int(x) for x in f.read().rstrip().split(",")]

    # Restore the gravity assist program
    prog[1] = 12
    prog[2] = 2
    exprs = z3.IntVector("x", len(prog))
    exprs[1] = noun
    exprs[2] = verb

    for i, n in enumerate(prog):
        if i != 1 and i != 2:
            s.add(exprs[i] == n)


# Run program
pc = 0

while (op := prog[pc]) != 99:
    if op == 1:
        exprs[prog[pc + 3]] = exprs[prog[pc + 1]] + exprs[prog[pc + 2]]
        prog[prog[pc + 3]] = prog[prog[pc + 1]] + prog[prog[pc + 2]]
        pc += 4
    elif op == 2:
        exprs[prog[pc + 3]] = exprs[prog[pc + 1]] * exprs[prog[pc + 2]]
        prog[prog[pc + 3]] = prog[prog[pc + 1]] * prog[prog[pc + 2]]
        pc += 4
    else:
        assert False, "panic"


# Part 1
ans1 = prog[0]
print(ans1)

# Part 2
s.add(exprs[0] == 19690720)
ans2 = z3.Int("ans2")
s.add(ans2 == (100 * noun) + verb)
assert s.check() == z3.sat
print(s.model()[ans2])
