import z3


solver1, env1 = z3.Solver(), {}
solver2, env2 = z3.Solver(), {}

with open("input.txt") as f:
    for ln in f:
        for part, (solver, env) in enumerate([(solver1, env1), (solver2, env2)]):
            part2 = part + 1 == 2
            ln = ln.rstrip()

            def lookup(name):
                return env.setdefault(name, z3.Int(name))

            monkey_name = ln[0:4]
            monkey_var = lookup(monkey_name)

            if part2 and monkey_name == "humn":
                continue

            # Parse expression
            assert ln[4] == ':'
            monkey_job_expr = ln[6:]

            if monkey_job_expr.isdigit():
                solver.add(monkey_var == int(monkey_job_expr))
            else:
                left, op, right = monkey_job_expr.split()
                assert len(left) == 4 and len(right) == 4
                assert len(op) == 1 and op in "+-*/"

                if part2 and monkey_name == "root":
                    solver.add(lookup(left) == lookup(right))
                    continue

                match op:
                    case '+':
                        solver.add(monkey_var == lookup(left) + lookup(right))

                    case '-':
                        solver.add(monkey_var == lookup(left) - lookup(right))

                    case '*':
                        solver.add(monkey_var == lookup(left) * lookup(right))

                    case '/':
                        solver.add(monkey_var == lookup(left) / lookup(right))

# Part 1
assert solver1.check() == z3.sat
print(solver1.model()[env1["root"]])

# Part 2
solver2.add(env2["humn"] < 3848301405791)  # 3848301405791 too high
assert solver2.check() == z3.sat
print(solver2.model()[env2["humn"]])
