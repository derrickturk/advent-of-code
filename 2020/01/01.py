import sys

def find2sum(xs):
    for (i, x) in enumerate(xs):
        for (j, y) in enumerate(xs):
            if i != j and x + y == 2020:
                return x, y

inp = [int(l.strip()) for l in sys.stdin]
x, y = find2sum(inp)
print(x * y)
