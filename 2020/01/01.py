import sys

def find2sum(xs):
    for (i, x) in enumerate(xs):
        for (j, y) in enumerate(xs):
            if i != j and x + y == 2020:
                return x, y

def find3sum(xs):
    for (i, x) in enumerate(xs):
        for (j, y) in enumerate(xs):
            for (k, z) in enumerate(xs):
                if i != j != k and x + y + z == 2020:
                    return x, y, z

inp = [int(l.strip()) for l in sys.stdin]
x, y = find2sum(inp)
print(x * y)
x, y, z = find3sum(inp)
print(x * y * z)
