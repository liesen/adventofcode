import z3

L = [ln.rstrip() for ln in open(0)]

# Part 1
s = z3.Solver()
vars: dict[str, z3.BoolRef] = {}

for ln in L[: L.index("")]:
    gate, _val = ln.split(": ")
    vars[gate] = z3.Bool(gate)
    s.add(vars[gate] == bool(int(_val)))

for ln in L[L.index("") + 1 :]:
    match ln.split(" "):
        case [in1, "AND", in2, "->", output]:
            for gate in [in1, in2, output]:
                if gate not in vars:
                    vars[gate] = z3.Bool(gate)

            s.add(vars[output] == vars[in1] & vars[in2])
        case [in1, "OR", in2, "->", output]:
            for gate in [in1, in2, output]:
                if gate not in vars:
                    vars[gate] = z3.Bool(gate)

            s.add(vars[output] == vars[in1] | vars[in2])
        case [in1, "XOR", in2, "->", output]:
            for gate in [in1, in2, output]:
                if gate not in vars:
                    vars[gate] = z3.Bool(gate)

            s.add(vars[output] == (vars[in1] != vars[in2]))


zs = [z for z in vars if z.startswith("z")]
ans1 = z3.BitVec("ans1", len(zs))

# Set nth bit of ans1 to zn
for z in zs:
    n = int(z[1:])
    s.add(z3.Extract(n, n, ans1) == z3.If(vars[z], z3.BitVecVal(1, 1), z3.BitVecVal(0, 1)))

assert s.check() == z3.sat
m = s.model()
print(m[ans1].as_long())