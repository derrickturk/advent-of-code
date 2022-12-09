import sys
from functools import reduce

import pmap
from plist import Cons, List

from typing import Iterator, NamedTuple

class Dir(NamedTuple):
    file_total: int
    children: pmap.Map[str, 'Dir']

    def total_size(self) -> int:
        return self.file_total + sum(
          c.total_size() for c in pmap.values(self.children))

    def rooted_total_sizes(self) -> Iterator[int]:
        yield self.total_size()
        for c in pmap.values(self.children):
            yield from c.rooted_total_sizes()

class _Step(NamedTuple):
    name: str
    file_total: int
    rest: pmap.Map[str, Dir]

class _Z(NamedTuple):
    path: List[_Step]
    focus: Dir

    def cd(self, child: str) -> '_Z':
        path, (file_total, children) = self
        match pmap.get(children, child):
            case None:
                return _Z(
                  Cons(_Step(child, file_total, children), path),
                  Dir(0, pmap.empty())
                )
            case c:
                assert c is not None
                return _Z(
                  Cons(
                    _Step(child, file_total, pmap.remove(children, child)),
                    path
                  ),
                  c
                )
        raise ValueError('impossible')

    def cd_parent(self) -> '_Z':
        match self:
            case _Z(None, _):
                return self
            case _Z(Cons(_Step(name, sz, others), rest), this):
                return _Z(rest, Dir(sz, pmap.insert(others, name, this)))
        raise ValueError('impossible')

    def root(self) -> '_Z':
        match self:
            case _Z(None, _):
                return self
            case _:
                return self.cd_parent().root()

    def increment_size(self, incr: int) -> '_Z':
        return self._replace(
          focus=self.focus._replace(file_total=self.focus.file_total + incr))

def apply(z: _Z, cmd: str) -> _Z:
    match cmd.strip().split():
        case ['$', 'cd', '/']:
            return z.cd_parent()
        case ['$', 'cd', '..']:
            return z.cd_parent()
        case ['$', 'cd', c]:
            return z.cd(c)
        case ['$', 'ls'] | ['dir', _]:
            return z
        case [sz, _] if sz.isdecimal():
            return z.increment_size(int(sz))
        case _:
            raise ValueError('bad command')
    raise ValueError('impossible')

def main() -> None:
    root = reduce(apply, sys.stdin, _Z(None, Dir(0, pmap.empty()))).root().focus
    gap = 30000000 - (70000000 - root.total_size())
    sizes = list(root.rooted_total_sizes())
    print(sum(s for s in sizes if s <= 100000))
    print(min(s for s in sizes if s >= gap))

if __name__ == '__main__':
    main()
