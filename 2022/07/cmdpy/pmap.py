# a persistent finite map (basic bitch BST + zippers)

from enum import Enum, auto
from functools import reduce
from typing import Generic, NamedTuple, NewType, Protocol, TypeAlias, TypeVar

from plist import Cons, List

# a protocol for types supporting < and >
O = TypeVar('O', bound='Ordered')

class Ordered(Protocol):
    def __lt__(self: O, other: O) -> bool:
        ...

    def __gt__(self: O, other: O) -> bool:
        ...
K = TypeVar('K', bound='Ordered')
V = TypeVar('V')

# data-carrying nodes of a BST
class _B(Generic[K, V], NamedTuple):
    key: K
    value: V
    left: 'Map[K, V]'
    right: 'Map[K, V]'

Map: TypeAlias = _B[K, V] | None

def empty() -> Map[K, V]:
    return None

class _Dir(Enum):
    _Left = auto()
    _Right = auto()

class _Step(Generic[K, V], NamedTuple):
    dir: _Dir
    key: K
    value: V
    other: Map[K, V]

class _Z(Generic[K, V], NamedTuple):
    path: List[_Step[K, V]]
    focus: Map[K, V]

    def up(self) -> '_Z[K, V]':
        match self:
            case _Z(None, _): # type: ignore
                raise ValueError('already at root!')
            case _Z(Cons(_Step(_Dir._Left, k, v, r), rest), m): # type: ignore
                return _Z(rest, _B(k, v, m, r))
            case _Z(Cons(_Step(_Dir._Right, k, v, l), rest), m): # type: ignore
                return _Z(rest, _B(k, v, l, m))
        raise ValueError('impossible')

    def left(self) -> '_Z[K, V]':
        match self:
            case _Z(_, None): # type: ignore
                raise ValueError('already at leaf!')
            case _Z(path, _B(k, v, l, r)): # type: ignore
                return _Z(Cons(_Step(_Dir._Left, k, v, r), path), l)
        raise ValueError('impossible')

    def right(self) -> '_Z[K, V]':
        match self:
            case _Z(_, None): # type: ignore
                raise ValueError('already at leaf!')
            case _Z(path, _B(k, v, l, r)): # type: ignore
                return _Z(Cons(_Step(_Dir._Right, k, v, l), path), r)
        raise ValueError('impossible')

    def root(self) -> '_Z[K, V]':
        match self:
            case _Z(None, _): # type: ignore
                return self
            case _:
                return self.up().root()
        raise ValueError('impossible')

    def find(self, key: K) -> '_Z[K, V]':
        if self.focus is None:
            return self
        if key < self.focus.key:
            return self.left().find(key)
        if key > self.focus.key:
            return self.right().find(key)
        return self

    def _min(self) -> '_Z[K, V]':
        match self.focus:
            case _B(_, _, _B(_, _, _, _), _):
                return self.left()._min()
            case _B(_, _, _, _):
                return self
            case None:
                raise ValueError('no keys!')

def get(map: Map[K, V], key: K) -> V | None:
    z = _Z(None, map).find(key)
    match z.focus:
        case _B(_, value, _, _):
            return value
        case None:
            return None

def insert(map: Map[K, V], key: K, value: V) -> Map[K, V]:
    z = _Z(None, map).find(key)
    match z.focus:
        case _B(_, _, _, _):
            z_new = z._replace(focus=z.focus._replace(value=value))
        case None:
            z_new = z._replace(focus=_B(key, value, None, None))
    return z_new.root().focus

def remove(map: Map[K, V], key: K) -> Map[K, V]:
    z = _Z(None, map).find(key)
    match z.focus:
        case _B(_, _, None, None):
            z_new = z._replace(focus=None)
        case _B(_, _, l, None):
            z_new = z._replace(focus=l)
        case _B(_, _, None, r):
            z_new = z._replace(focus=r)
        case _B(_, _, l, r):
            # the sucky case
            min_r = _Z(None, r)._min()
            assert min_r.focus is not None
            z_new = z._replace(
              focus=_B(
                  min_r.focus.key,
                  min_r.focus.value,
                  l,
                  min_r._replace(focus=None).root().focus
              )
            )
        case None:
            z_new = z
    return z_new.root().focus

__all__ = [
    'Map',
    'empty',
    'get',
    'insert',
    'remove',
]
