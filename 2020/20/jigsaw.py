import sys
import re

from math import sqrt
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
            binhashdot(line[0] for line in self._contents),
            binhashdot(line[0] for line in reversed(self._contents))
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

def adjust(t: Tile, source_edge: Dir, target_edge: Dir, flip: bool) -> Tile:
    if source_edge == Dir.NORTH:
        if target_edge == Dir.NORTH:
            ret = t.r180()
            if flip: # already "flipped" by 180
                return ret
            else:
                return ret.fliph()
        elif target_edge == Dir.SOUTH:
            ret = t
            if flip:
                return ret.fliph()
            else:
                return ret
        elif target_edge == Dir.EAST:
            ret = t.r90()
            if flip: # already "flipped" by 90
                return ret
            else:
                return ret.fliph()
        else: # WEST
            ret = t.r270()
            if flip:
                return ret.fliph()
            else:
                return ret

    elif source_edge == Dir.SOUTH:
        if target_edge == Dir.NORTH:
            ret = t
            if flip:
                return ret.fliph()
            else:
                return ret
        elif target_edge == Dir.SOUTH:
            ret = t.r180()
            if flip: # already "flipped" by 180
                return ret
            else:
                return ret.fliph()
        elif target_edge == Dir.EAST:
            ret = t.r270()
            if flip:
                return ret.fliph()
            else:
                return ret
        else: # WEST
            ret = t.r90()
            if flip: # already "flipped" by 90
                return ret
            else:
                return ret.fliph()

    elif source_edge == Dir.EAST:
        if target_edge == Dir.NORTH:
            ret = t.r270()
            if flip:
                return ret.flipv()
            else:
                return ret
        elif target_edge == Dir.SOUTH:
            ret = t.r90()
            if flip: # already "flipped" by 90
                return ret
            else:
                return ret.flipv()
        elif target_edge == Dir.EAST:
            ret = t.r180()
            if flip: # already "flipped" by 180
                return ret
            else:
                return ret.flipv()
        else: # WEST
            ret = t
            if flip:
                return ret.flipv()
            else:
                return ret

    else: # WEST
        if target_edge == Dir.NORTH:
            ret = t.r90()
            if flip: # already "flipped" by 90
                return ret
            else:
                return ret.flipv()
        elif target_edge == Dir.SOUTH:
            ret = t.r270()
            if flip:
                return ret.flipv()
            else:
                return ret
        elif target_edge == Dir.EAST:
            ret = t
            if flip:
                return ret.flipv()
            else:
                return ret
        else: # WEST
            ret = t.r180()
            if flip: # already "flipped" by 180
                return ret
            else:
                return ret.flipv()

    raise NotImplementedError

def fill(init_edge: int, tiles: List[Tile]) -> List[List[Tile]]:
    dim_f = sqrt(len(tiles))
    if not dim_f.is_integer():
        raise ValueError('non-square puzzle!')
    dim = int(dim_f)
    if dim == 0:
        raise ValueError('empty puzzle!')

    rows = list()
    for _ in range(dim):
        row = list()
        found = False
        for t in tiles:
            # first fill down the first tile...
            for e, d, f in t.possible_edges():
                if e == init_edge:
                    row.append(adjust(t, Dir.SOUTH, d, f))
                    init_edge = row[-1].east_edge
                    tiles.remove(t)
                    found = True
                    break
            if found:
                break
        if not found:
            raise ValueError('assumption violated!')

        # then fill east...
        for _ in range(dim - 1):
            found = False
            for t in tiles:
                for e, d, f in t.possible_edges():
                    if e == init_edge:
                        row.append(adjust(t, Dir.EAST, d, f))
                        init_edge = row[-1].east_edge
                        tiles.remove(t)
                        found = True
                        break
                if found:
                    break
            if not found:
                raise ValueError(f'assumption violated: {init_edge}!')

        init_edge = row[0].south_edge
        rows.append(row)

    return rows

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

def corners(tiles: List[Tile]) -> List[Tuple[Tile, int, int]]:
    result: List[Tuple[Tile, int, int]] = list()
    for t in tiles:
        unmatched_edges = list()
        for _, e in t.edges:
            found = False
            for u in tiles:
                if t == u:
                    continue
                for v, _, _ in u.possible_edges():
                    if v == e:
                        found = True
                        break
                if found:
                    break
            if not found:
                unmatched_edges.append(e)
        if len(unmatched_edges) == 2:
            result.append((t, unmatched_edges[0], unmatched_edges[1]))

    if len(result) != 4:
        raise ValueError('assumptions violated!')

    return result

def dump_world(world: List[List[Tile]], stream: TextIO) -> None:
    for row in world:
        for i in range(len(row[0]._contents)):
            for t in row:
                stream.write(t._contents[i])
            stream.write('\n')

def main() -> int:
    tiles = list(parse_tiles(sys.stdin))
    corner_tiles = corners(tiles)

    prod = 1
    for c, _, _ in corner_tiles:
        prod *= c.tile_id
    print(prod)

    init_edge = corner_tiles[0][1] # an arbitrary choice
    world = fill(init_edge, tiles)

    dump_world(world, sys.stdout)

    return 0

if __name__ == '__main__':
    sys.exit(main())
