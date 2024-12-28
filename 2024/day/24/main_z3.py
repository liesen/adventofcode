import sys
from abc import ABC
from dataclasses import dataclass

import z3

L = [ln.rstrip() for ln in open(0)]


@dataclass
class Gate(ABC):
    input1: str
    input2: str
    output: str

    def inputs_and_outputs(self):
        return [self.input1, self.input2, self.output]


@dataclass
class And(Gate):
    pass


@dataclass
class Or(Gate):
    pass


@dataclass
class Xor(Gate):
    pass


def parse_wire_value(ln):
    wire, _val = ln.split(": ")
    return (wire, bool(int(_val)))


wires = dict(parse_wire_value(ln) for ln in L[: L.index("")])


def parse_gate(ln: str) -> Gate:
    match ln.split(" "):
        case [input1, "AND", input2, "->", output]:
            return And(input1, input2, output)
        case [input1, "OR", input2, "->", output]:
            return Or(input1, input2, output)
        case [input1, "XOR", input2, "->", output]:
            return Xor(input1, input2, output)

    raise Exception("bad input")


gates = [parse_gate(ln) for ln in L[L.index("") + 1 :]]
inputs_and_outputs = set(wires) | {w for g in gates for w in g.inputs_and_outputs()}


# Part 1
def simulate(wires: dict[str, bool], gates: list[Gate], part2: bool = False) -> int:
    s = z3.Solver()
    vars = {
        wire: z3.Bool(wire)
        for wire in set(wires)
        | {wire for gate in gates for wire in gate.inputs_and_outputs()}
    }

    for wire, value in wires.items():
        s.add(vars[wire] == value)

    for wire in gates:
        match wire:
            case And(input1, input2, output):
                s.add(vars[output] == (vars[input1] & vars[input2]))

            case Or(input1, input2, output):
                s.add(vars[output] == (vars[input1] | vars[input2]))

            case Xor(input1, input2, output):
                s.add(vars[output] == (vars[input1] != vars[input2]))

            case _:
                raise Exception("unreachable")

    nbits = sum(1 for b in vars if b.startswith("z"))

    def pack(var, nbits):
        """Pack the bits starting with var into a bit vector"""
        zvar = z3.BitVec(var, nbits)

        for b in vars:
            if b.startswith(var):
                n = int(b[1:])
                s.add(
                    z3.Extract(n, n, zvar)
                    == z3.If(vars[b], z3.BitVecVal(1, 1), z3.BitVecVal(0, 1))
                )

        return zvar

    Z = pack("z", nbits)

    if part2:
        X = pack("x", nbits)
        Y = pack("y", nbits)
        s.add(X + Y == Z)

    assert s.check() == z3.sat
    m = s.model()
    return m[Z].as_long()


# Part 1
print(simulate(wires, gates))


# Part 2
# Some knots found by visually inspecting the Graphviz output
with open("24.dot", "w+") as file:
    file.write("digraph G {\n")
    file.write(f"  {{ rank = same; {'; '.join(wires)}; }}\n")

    file.write("  subgraph {\n")
    file.write(
        "    "
        + " -> ".join(
            (
                x_in + " -> " + y_in
                for x_in, y_in in zip(
                    sorted(w for w in wires if w.startswith("x")),
                    sorted(w for w in wires if w.startswith("y")),
                )
            ),
        )
        + " [style=invis]\n"
    )
    file.write("  }\n")

    file.write("  subgraph {\n")
    file.write(
        "     "
        + " -> ".join(sorted(w for w in inputs_and_outputs if w.startswith("z")))
        + " [style=invis]\n"
    )
    file.write("  }\n")

    for i, gate in enumerate(gates):
        match gate:
            case And(input1, input2, output):
                label = "AND"
            case Or(input1, input2, output):
                label = "OR"
            case Xor(input1, input2, output):
                label = "XOR"

        file.write(f'  gate_{i} [label="{label}"]\n')
        file.write(f"  {{{input1}, {input2}}} -> gate_{i} -> {output}\n")

    file.write("}\n")
    file.flush()


# Swaps to fix the adder
for gate in gates:
    match gate:
        # Pair 1
        case And("mcr", "sjd", "z08"):
            gate.output = "mvb"
        case Xor("sjd", "mcr", "mvb"):
            gate.output = "z08"

        # Pair 2
        case And("x18", "y18", "z18"):
            gate.output = "wss"
        case Xor("mfk", "fmm", "wss"):
            gate.output = "z18"

        # Pair 3
        case Or("fwj", "vsq", "z23"):
            gate.output = "bmn"
        case Xor("qmd", "bpr", "bmn"):
            gate.output = "z23"

        # Pair 4
        case Xor("x14", "y14", "jss"):
            gate.output = "rds"
        case And("x14", "y14", "rds"):
            gate.output = "jss"

        case _:
            pass


# Most knots can be found by "validating" the adders:
#
# x XOR y = z'
# x AND y = t
#
# Cin XOR z' = z
# Cin AND z' = t'
#
# t OR t' = Cout
def find_and(inputs):
    for g in gates:
        match g:
            case And(input1, input2, _) if {input1, input2} == inputs:
                return g


def find_or(inputs):
    for g in gates:
        match g:
            case Or(input1, input2, _) if {input1, input2} == inputs:
                return g


def find_xor(inputs):
    for g in gates:
        match g:
            case Xor(input1, input2, _) if {input1, input2} == inputs:
                return g


# Find first half adder
xor0 = find_xor({"x00", "y00"})
assert xor0
assert xor0.output == "z00"
and0 = find_and({"x00", "y00"})
assert and0
carry = and0.output

# Check full adders
for i in range(1, 45):
    inputs = {f"x{i:02d}", f"y{i:02d}"}
    output = f"z{i:02d}"
    # x XOR y
    xor0 = find_xor(inputs)
    assert xor0, f"[{output}] could not find xor gate with inputs: {inputs}"
    # x AND y
    and0 = find_and(inputs)
    assert and0
    # cin XOR (x XOR y) = output
    xor1 = find_xor({carry, xor0.output})
    assert xor1, f"[{output}] could not find xor gate with inputs: {carry}, {xor0.output}. probably {xor0} needs swapping."
    assert (
        xor1.output == output
    ), f"[{output}] expected {xor1} to have output: {output}. possible swap: {next((g for g in gates if g.output == output), None)}."
    # cin AND (x XOR y)
    and1 = find_and({carry, xor0.output})
    assert and1
    # (x AND y) OR (cin AND (x XOR y)) = Cout
    or1 = find_or({and0.output, and1.output})
    assert or1
    carry = or1.output


# Inspect digits in the binary output of the gates and compare with the binary
# digits from just adding x and y
x_in = int(
    "".join(
        str(int(wires[w]))
        for w in sorted((w for w in wires if w.startswith("x")), reverse=True)
    ),
    2,
)
y_in = int(
    "".join(
        str(int(wires[w]))
        for w in sorted((w for w in wires if w.startswith("y")), reverse=True)
    ),
    2,
)
expected = x_in + y_in
actual = simulate(wires, gates)
print("expected:", f"{expected:b}", file=sys.stderr)
print("  actual:", f"{actual:b}", file=sys.stderr)
num_equal_bits = sum(a == b for a, b in zip(f"{expected:b}", f"{actual:b}"))
print("num equal bits:", num_equal_bits, "/", len(f"{actual:b}"), file=sys.stderr)
print(
    "incorrect bits:",
    ", ".join(
        str(i) for i, (a, b) in enumerate(zip(f"{expected:b}", f"{actual:b}")) if a != b
    )
    or "none",
    file=sys.stderr,
)

simulate(wires, gates, True)  # Throws unless x + y == z
print("bmn,jss,mvb,rds,wss,z08,z18,z23")
