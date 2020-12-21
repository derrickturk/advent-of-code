import sys
import re

from copy import copy
from typing import Iterable, Iterator, List, Set, TextIO, Tuple

def binhashdot(chars: Iterable[str]) -> int:
    return int(''.join('1' if c == '#' else '0' for c in chars), base=2)

class Tile:
    __slots__ = (
        '_tile_id',
        '_contents',
        '_north_edge',
        '_south_edge',
        '_east_edge',
        '_west_edge'
    )

    _tile_id: int
    _contents: List[str]
    _north_edge: Tuple[int, int]
    _south_edge: Tuple[int, int]
    _east_edge: Tuple[int, int]
    _west_edge: Tuple[int, int]

    def __init__(self, tile_id: int, contents: List[str]) -> None:
        self._tile_id = tile_id
        self._contents = contents
        self._north_edge = (
            binhashdot(self._contents[0]),
            binhashdot(reversed(self._contents[0]))
        )
        self._south_edge = (
            binhashdot(self._contents[-1]),
            binhashdot(reversed(self._contents[-1]))
        )
        self._east_edge = (
            binhashdot(line[-1] for line in self._contents),
            binhashdot(line[-1] for line in reversed(self._contents))
        )
        self._west_edge = (
            binhashdot(self._contents[0]),
            binhashdot(reversed(self._contents[0]))
        )

    @property
    def tile_id(self) -> int:
        return self._tile_id

    @property
    def north_edge(self) -> int:
        return self._north_edge[0]

    @property
    def south_edge(self) -> int:
        return self._south_edge[0]

    @property
    def east_edge(self) -> int:
        return self._east_edge[0]

    @property
    def west_edge(self) -> int:
        return self._west_edge[0]

    @property
    def north_edge_flipped(self) -> int:
        return self._north_edge[1]

    @property
    def south_edge_flipped(self) -> int:
        return self._south_edge[1]

    @property
    def east_edge_flipped(self) -> int:
        return self._east_edge[1]

    @property
    def west_edge_flipped(self) -> int:
        return self._west_edge[1]

    def flipv(self) -> 'Tile':
        flipped = copy(self)
        flipped._contents = list(reversed(self._contents))
        flipped._north_edge = self._south_edge
        flipped._south_edge = self._north_edge
        flipped._west_edge = (self._west_edge[1], self._west_edge[0])
        flipped._east_edge = (self._east_edge[1], self._east_edge[0])
        return flipped

    def fliph(self) -> 'Tile':
        flipped = copy(self)
        flipped._contents = [''.join(reversed(c)) for c in self._contents]
        flipped._north_edge = (self._north_edge[1], self._north_edge[0])
        flipped._south_edge = (self._south_edge[1], self._south_edge[0])
        flipped._west_edge = self._east_edge
        flipped._east_edge = self._west_edge
        return flipped

    def __repr__(self) -> str:
        return f'Tile({self._tile_id}, {repr(self._contents)})'

    def __str__(self) -> str:
        contents = '\n'.join(self._contents)
        return f'''Tile id: {self._tile_id}
{contents}
'''

    def possible_edges(self) -> Set[int]:
        return {
            self.north_edge, self.north_edge_flipped,
            self.south_edge, self.south_edge_flipped,
            self.east_edge, self.east_edge_flipped,
            self.west_edge, self.west_edge_flipped
        }

ID_RE = re.compile(r'Tile (\d+):')

def parse_tiles(stream: TextIO) -> Iterator[Tile]:
    lines = iter(stream)
    try:
        while True:
            line = next(lines)
            match = ID_RE.match(line)
            if not match:
                raise ValueError('invalid tile header')
            tile_id = int(match.group(1))

            contents = list()
            while True:
                line = next(lines).rstrip()
                if line == '':
                    break
                contents.append(line)
            yield Tile(tile_id, contents)
    except StopIteration:
        yield Tile(tile_id, contents)

def main() -> int:
    tiles = list(parse_tiles(sys.stdin))
    print(len(tiles))
    print(tiles[-1])

    for start in tiles:
        for edge in (start.north_edge, start.south_edge, start.west_edge, start.east_edge):
            poss = [t for t in tiles if edge in t.possible_edges() and t.tile_id != start.tile_id]
            if len(poss) == 1:
                print(f'got it: {start.tile_id}, {poss[0].tile_id}')
    print(poss)

    return 0

if __name__ == '__main__':
    sys.exit(main())
