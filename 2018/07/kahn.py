from sys import argv, exit, stdin, stderr
from typing import TypeVar, List, Tuple, Set, Callable

Edge = Tuple[str, str]
Graph = List[Edge]

def initials(g: Graph) -> Set[str]:
    tos = { t for (t, _) in g }
    return { f for (_, f) in g if not f in tos }

def topo_sort(g: Graph) -> List[str]:
    edges = list(g)
    s = initials(g)
    l: List[str] = []
    while s:
        n = sorted(s)[0]
        s.remove(n)
        l.append(n)
        i = 0
        while i < len(edges):
            t, f = edges[i]
            if f == n:
                del edges[i]
                if t not in { t for t, _ in edges }:
                    s.add(t)
            else:
                i += 1
    if edges:
        raise ValueError('g contained a cycle')
    return l

def parse(line: str) -> Edge:
    _, frm, _, _, _, _, _, to, _, _ = line.split()
    return (to, frm)

def main(argv):
    try:
        g = [parse(l) for l in stdin]
        print(''.join(topo_sort(g)))
    except ValueError:
        print('invalid input', file=stderr)

if __name__ == '__main__':
    exit(main(argv))
