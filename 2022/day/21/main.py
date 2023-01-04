from abc import ABC
from dataclasses import dataclass


yell = print


class Expr(ABC):
    pass

@dataclass
class Var(Expr):
    name: str

@dataclass
class Lit(Expr):
    value: int

@dataclass
class Bin(Expr):
    op: str
    lhs: Expr
    rhs: Expr


def parse_expr(s: str) -> Expr:
    if s.isdigit():
        return Lit(int(s))

    if s.isalpha():
        return Var(s)

    left, op, right = s.split()
    assert len(left) == 4 and len(right) == 4
    assert len(op) == 1 and op in "+-*/"
    return Bin(op, parse_expr(left), parse_expr(right))


exprs = {}

with open("input.txt") as f:
    for ln in f:
        ln = ln.rstrip()
        monkey_name = ln[0:4]
        assert ln[4] == ':'
        monkey_job_expr = ln[6:]
        exprs[monkey_name] = parse_expr(monkey_job_expr)

# Part 1
def eval1(exp, env):
    match exp:
        case Lit(val): return val
        case Var(var): return eval1(env[var], env)
        case Bin('+', lhs, rhs): return eval1(lhs, env) + eval1(rhs, env)
        case Bin('-', lhs, rhs): return eval1(lhs, env) - eval1(rhs, env)
        case Bin('*', lhs, rhs): return eval1(lhs, env) * eval1(rhs, env)
        case Bin('/', lhs, rhs): return eval1(lhs, env) // eval1(rhs, env)

yell(eval1(exprs["root"], exprs))

# Part 2
match exprs["root"]:
    case Bin(_, a, b): pass
    case _: raise Exception("root not a binary expression")

def contains_humn(expr, env):
    match expr:
        case Lit(_): return False
        case Var(var):
            return var == "humn" or contains_humn(env[var], env)
        case Bin(_, lhs, rhs):
            return contains_humn(lhs, env) or contains_humn(rhs, env)

# One of the expressions contains humn, the other is a single value
assert contains_humn(a, exprs)
assert not contains_humn(b, exprs)

def simplify(expr, env):
    match expr:
        case Lit(val):
            return False, Lit(val)
        case Var("humn"):
            return False, Var("humn")
        case Var(var):
            return True, env[var]
        case Bin('+', Lit(a), Lit(b)):
            return True, Lit(a + b)
        case Bin('+', lhs, rhs):
            a, lhs_ = simplify(lhs, env)
            b, rhs_ = simplify(rhs, env)
            return (a or b), Bin('+', lhs_, rhs_)
        case Bin('-', Lit(a), Lit(b)):
            return True, Lit(a - b)
        case Bin('-', lhs, rhs):
            a, lhs_ = simplify(lhs, env)
            b, rhs_ = simplify(rhs, env)
            return (a or b), Bin('-', lhs_, rhs_)
        case Bin('*', Lit(a), Lit(b)):
            return True, Lit(a * b)
        case Bin('*', lhs, rhs):
            a, lhs_ = simplify(lhs, env)
            b, rhs_ = simplify(rhs, env)
            return (a or b), Bin('*', lhs_, rhs_)
        case Bin('/', Lit(a), Lit(b)):
            return True, Lit(a // b)
        case Bin('/', lhs, rhs):
            a, lhs_ = simplify(lhs, env)
            b, rhs_ = simplify(rhs, env)
            return (a or b), Bin('/', lhs_, rhs_)
        case _:
            return False, expr

# Simplify expression and replace all variables except humn
cont = True

while cont:
    cont, a = simplify(a, exprs)

# a now has only one variable, humn:
def make_equal(ans: int, expr: Expr, env: dict[str, Expr]) -> int:
    match expr:
        case Var("humn"):
            return ans
        case Var(var):
            return make_equal(ans, env[var], env)
        case Bin('+', Lit(a), rhs):
            # ans == a + rhs
            return make_equal(ans - a, rhs, env)
        case Bin('+', lhs, Lit(b)):
            # ans == lhs + a
            return make_equal(ans - b, lhs, env)
        case Bin('-', Lit(a), rhs):
            # ans == a - rhs
            return make_equal(a - ans, rhs, env)
        case Bin('-', lhs, Lit(b)):
            # ans == lhs - a
            return make_equal(ans + b, lhs, env)
        case Bin('*', Lit(a), rhs):
            # ans == a * rhs
            assert ans % a == 0
            return make_equal(ans // a, rhs, env)
        case Bin('*', lhs, Lit(b)):
            # ans == lhs * a
            assert ans % b == 0
            return make_equal(ans // b, lhs, env)
        case Bin('/', lhs, Lit(b)):
            # ans == lhs / b
            return make_equal(ans * b, lhs, env)
        case _:
            raise Exception("bad expression", expr)

bval = eval1(b, exprs)
yell(make_equal(bval, a, exprs))
