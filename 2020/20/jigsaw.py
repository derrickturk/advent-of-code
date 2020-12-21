import sys
import re

from enum import Enum, auto
from copy import copy
from typing import Dict, Iterable, Iterator, List, TextIO, Tuple

def binhashdot(chars: Iterable[str]) -> int:
    return int(''.join('1' if c == '#' else '0' for c in chars), base=2)

class Dir(Enum):
    NORTH = auto()
    SOUTH = auto()
    EAST = auto()
    WEST = auto()

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

        if (len(self._contents) == 0 or
                any(len(c) != len(self._contents) for c in self._contents)):
            raise ValueError('not a square tile!')

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

    @property
    def edges(self) -> List[Tuple[Dir, int]]:
        return [
            (Dir.NORTH, self.north_edge),
            (Dir.SOUTH, self.south_edge),
            (Dir.EAST, self.east_edge),
            (Dir.WEST, self.west_edge),
        ]

    # def __eq__(self, other: 'Tile') -> bool:
    #     return self.tile_id == other.tile_id

    # a b c    g h i
    # d e f -> d e f
    # g h i    a b c

    def flipv(self) -> 'Tile':
        flipped = copy(self)
        flipped._contents = list(reversed(self._contents))
        flipped._north_edge = self._south_edge
        flipped._south_edge = self._north_edge
        flipped._west_edge = (self._west_edge[1], self._west_edge[0])
        flipped._east_edge = (self._east_edge[1], self._east_edge[0])
        return flipped

    # a b c    c b a
    # d e f -> f e d
    # g h i    i h g

    def fliph(self) -> 'Tile':
        flipped = copy(self)
        flipped._contents = [''.join(reversed(c)) for c in self._contents]
        flipped._north_edge = (self._north_edge[1], self._north_edge[0])
        flipped._south_edge = (self._south_edge[1], self._south_edge[0])
        flipped._west_edge = self._east_edge
        flipped._east_edge = self._west_edge
        return flipped

    # a b c    g d a
    # d e f -> h e b
    # g h i    i f c

    def r90(self) -> 'Tile':
        dim = len(self._contents)
        return Tile(self.tile_id, [
            ''.join(self._contents[dim - i - 1][j] for i in range(dim))
            for j in range(len(self._contents))
        ])

    # a b c    i h g
    # d e f -> f e d
    # g h i    c b a

    def r180(self) -> 'Tile':
        dim = len(self._contents)
        return Tile(self.tile_id, [
            ''.join(self._contents[dim - j - 1][dim - i - 1] for i in range(dim))
            for j in range(len(self._contents))
        ])

    # a b c    c f i
    # d e f -> b e h
    # g h i    a d g

    def r270(self) -> 'Tile':
        dim = len(self._contents)
        return Tile(self.tile_id, [
            ''.join(self._contents[i][dim - j - 1] for i in range(dim))
            for j in range(len(self._contents))
        ])

    def __eq__(self, other: object) -> bool:
        return isinstance(other, Tile) and self._tile_id == other._tile_id 

    def __repr__(self) -> str:
        return f'Tile({self._tile_id}, {repr(self._contents)})'

    def __str__(self) -> str:
        contents = '\n'.join(self._contents)
        return f'''Tile id: {self._tile_id}
{contents}
'''

    def possible_edges(self) -> List[Tuple[int, Dir, bool]]:
        return [
            (self.north_edge, Dir.NORTH, False),
            (self.south_edge, Dir.SOUTH, False),
            (self.east_edge, Dir.EAST, False),
            (self.west_edge, Dir.WEST, False),
            (self.north_edge_flipped, Dir.NORTH, True),
            (self.south_edge_flipped, Dir.SOUTH, True),
            (self.east_edge_flipped, Dir.EAST, True),
            (self.west_edge_flipped, Dir.WEST, True),
        ]

World = Dict[Tuple[int, int], Tile]

def adjust(t: Tile, source_edge: Dir, target_edge: Dir, flip: bool) -> Tile:
    if source_edge == Dir.NORTH:
        if target_edge == Dir.SOUTH:
            if flip:
                return t.fliph()
            return t
        elif target_edge == Dir.NORTH:
            if flip:
                return t.flipv()
            # TODO
    raise NotImplementedError

def nucleate(tiles: List[Tile], world: World) -> None:
    open_edges: List[Tuple[Tuple[int, int], Dir, int]] = list()

    if len(world) == 0:
        t = tiles.pop()
        world[(0, 0)] = t
        for d, edge in t.edges:
            open_edges.append(((0, 0), d, edge))

    while tiles and open_edges:
        for ((x, y), d, val) in open_edges:
            poss = [(t, d, flip)
              for t in tiles
              for (e, d, flip) in t.possible_edges()
              if val == e
            ]

            if len(poss) == 1:
                add_t, add_d, flip = poss[0]
                tiles.remove(t)
                # TODO

        print(tiles)
        print(open_edges)

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
    # tiles = list(parse_tiles(sys.stdin))

    t = Tile(1, ['abc', 'def', 'ghi'])

    # world: World = dict()
    # nucleate(tiles, world)

    # corners = set()
    # for t in tiles:
    #     for _, e in t.edges:
    #         found = False
    #         for u in tiles:
    #             if t == u:
    #                 continue
    #             if e in [v for v, _1, _2 in u.possible_edges()]:
    #                 found = True
    #                 break
    #         if not found:
    #             corners.add(t.tile_id)
    # print(corners)

    return 0

if __name__ == '__main__':
    sys.exit(main())
