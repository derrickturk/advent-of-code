from collections import namedtuple
from sys import stdin

Claim = namedtuple('Claim', 'id left top width height')

def parse(line):
    id, _atsign, xy, wh = line.split()
    id = id[1:] # drop the #
    x, y = xy[:-1].split(',') # drop the :
    w, h = wh.split('x')
    return Claim(int(id), int(x), int(y), int(w), int(h))

def parse_claims(stream):
    return (parse(line.rstrip()) for line in stream)

def right(claim):
    return claim.left + claim.width - 1

def bottom(claim):
    return claim.top + claim.height - 1

def max_xy(claims):
    max_x = max(right(c) for c in claims)
    max_y = max(bottom(c) for c in claims)
    return (max_x, max_y)

def contested(claims):
    max_x, max_y = max_xy(claims)
    count = [[0 for y in range(max_y + 1)] for x in range(max_x + 1)]
    for c in claims:
        for i in range(c.width):
            for j in range(c.height):
                count[c.left + i][c.top + j] += 1
    return sum(sum(1 for c in row if c > 1) for row in count)

def disjoint(claim1, claim2):
    return (
        right(claim1) < claim2.left or
        right(claim2) < claim1.left or
        bottom(claim1) < claim2.top or
        bottom(claim2) < claim1.top
    )

def overlaps(claim1, claim2):
    return not disjoint(claim1, claim2)

def loners(claims):
    return [
        c for c in claims
        if not(any(d != c and overlaps(d, c) for d in claims))
    ]

if __name__ == '__main__':
    claims = list(parse_claims(stdin))
    # part 1
    print(contested(claims))
    # part 2
    for c in loners(claims):
        print(c.id)
