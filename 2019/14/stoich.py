import sys
from math import ceil

def oreFor(graph, target, n, surplus=None):
    if target == 'ORE':
        return n

    if surplus is None:
        surplus = dict()

    if target in surplus:
        n -= surplus[target]
        surplus[target] = 0

    (pc, ib) = graph[target]
    # print(f'can make {pc} {target} at a time')
    repeat = ceil(n / pc)
    extra = pc * repeat - n
    # print(f'repeat {repeat} times with surplus {extra}')

    if target in surplus:
        surplus[target] += extra
    else:
        surplus[target] = extra

    needed = 0
    for (c, i) in ib:
        # print(f'need to make {c * repeat} of {i}')
        needed += oreFor(graph, i, c * repeat, surplus)
    return needed

def parseStoich(line):
    inputs, product = line.rstrip().split(' => ')
    product_count, product = product.split(' ')
    product_count = int(product_count)
    input_breakdown = list()
    for i in inputs.split(', '):
        input_count, i = i.split(' ')
        input_breakdown.append((int(input_count), i))
    return (product_count, product, input_breakdown)

def stoichGraph(stoichs):
    return { p: (pc, ib) for (pc, p, ib) in stoichs }

if __name__ == '__main__':
    graph = stoichGraph(parseStoich(l) for l in sys.stdin)
    # print(graph)
    print(oreFor(graph, 'FUEL', 1))
