import sys

from typing import Iterable, Optional

class Dir:
    def __init__(self, name: str, parent: Optional['Dir'] = None):
        self.name = name
        self.size = 0
        self.children: dict[str, 'Dir'] = {}
        self.parent = parent

    def ensure_subdir(self, name: str) -> 'Dir':
        if not name in self.children:
            self.children[name] = Dir(name, self)
        return self.children[name]

    def percolate_size(self, incr: int) -> None:
        self.size += incr
        p = self.parent
        while p is not None:
            p.size += incr
            p = p.parent

    def descendants(self) -> Iterable['Dir']:
        yield self
        for c in self.children.values():
            yield from c.descendants()

    def dump(self, indent = 0) -> None:
        print(f'{" " * indent}{self.name}: {self.size}')
        cs = sorted(self.children.keys())
        for c in cs:
            self.children[c].dump(indent = indent + 2)

def main() -> None:
    root = Dir('/')
    for l in sys.stdin:
        match l.strip().split():
            case ['$', 'cd', '/']:
                cd = root
            case ['$', 'cd', '..']:
                if cd.parent is None:
                    raise ValueError('cd .. at root!')
                cd = cd.parent
            case ['$', 'cd', d]:
                cd = cd.ensure_subdir(d)
            case ['$', 'ls']:
                continue
            case ['dir', d]:
                cd.ensure_subdir(d)
            case [sz, _]:
                cd.percolate_size(int(sz))

    print(sum(d.size for d in root.descendants() if d.size <= 100000))
    unused = 70000000 - root.size
    gap = 30000000 - unused
    print(min(d.size for d in root.descendants() if d.size >= gap))

if __name__ == '__main__':
    main()
