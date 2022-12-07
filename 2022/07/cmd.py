import sys

class Dir:
    def __init__(self, name, parent = None):
        self.name = name
        self.size = 0
        self.children = {}
        self.parent = parent

    def ensure_subdir(self, name):
        if not name in self.children:
            self.children[name] = Dir(name, self)
        return self.children[name]

    def percolate_size(self, incr):
        self.size += incr
        p = self.parent
        while p is not None:
            p.size += incr
            p = p.parent

    def descendants(self):
        yield self
        for c in self.children.values():
            yield from c.descendants()

def main():
    root = Dir('/')
    for l in sys.stdin:
        match l.strip().split():
            case ['$', 'cd', '/']:
                cd = root
            case ['$', 'cd', '..']:
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
