# a persistent finite map (basic bitch BST + zippers)

from enum import Enum, auto
from functools import reduce
from typing import Generic, NamedTuple, NewType, Protocol, TypeAlias, TypeVar

# a protocol for types supporting < and >
O = TypeVar('O', bound='Ordered')

class Ordered(Protocol):
    def __lt__(self: O, other: O) -> bool:
        ...

    def __gt__(self: O, other: O) -> bool:
        ...

T = TypeVar('T')
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

# a persistent list, for use as a stack
class _Cons(Generic[T], NamedTuple):
    head: T
    tail: '_List[T]'

_List: TypeAlias = _Cons[T] | None

class _Dir(Enum):
    _Left = auto()
    _Right = auto()

class _Step(Generic[K, V], NamedTuple):
    dir: _Dir
    key: K
    value: V
    other: Map[K, V]

class _Z(Generic[K, V], NamedTuple):
    path: _List[_Step[K, V]]
    focus: Map[K, V]

def _up(z: _Z[K, V]) -> _Z[K, V]:
    match z:
        case _Z(None, _): # type: ignore
            raise ValueError('already at root!')
        case _Z(_Cons(_Step(_Dir._Left, k, v, r), rest), m): # type: ignore
            return _Z(rest, _B(k, v, m, r))
        case _Z(_Cons(_Step(_Dir._Right, k, v, l), rest), m): # type: ignore
            return _Z(rest, _B(k, v, l, m))
    raise ValueError('impossible')

def _left(z: _Z[K, V]) -> _Z[K, V]:
    match z:
        case _Z(_, None): # type: ignore
            raise ValueError('already at leaf!')
        case _Z(path, _B(k, v, l, r)): # type: ignore
            return _Z(_Cons(_Step(_Dir._Left, k, v, r), path), l)
    raise ValueError('impossible')

def _right(z: _Z[K, V]) -> _Z[K, V]:
    match z:
        case _Z(_, None): # type: ignore
            raise ValueError('already at leaf!')
        case _Z(path, _B(k, v, l, r)): # type: ignore
            return _Z(_Cons(_Step(_Dir._Right, k, v, l), path), r)
    raise ValueError('impossible')

def _root(z: _Z[K, V]) -> _Z[K, V]:
    match z:
        case _Z(None, _): # type: ignore
            return z
        case _:
            return _root(_up(z))
    raise ValueError('impossible')

def _find(z: _Z[K, V], key: K) -> _Z[K, V]:
    if z.focus is None:
        return z
    if key < z.focus.key:
        return _find(_left(z), key)
    if key > z.focus.key:
        return _find(_right(z), key)
    return z

def _min(z: _Z[K, V]) -> _Z[K, V]:
    match z.focus:
        case _B(_, _, _B(_, _, _, _), _):
            return _min(_left(z))
        case _B(_, _, _, _):
            return z
        case None:
            raise ValueError('no keys!')

def get(map: Map[K, V], key: K) -> V | None:
    z = _find(_Z(None, map), key)
    match z.focus:
        case _B(_, value, _, _):
            return value
        case None:
            return None

def insert(map: Map[K, V], key: K, value: V) -> Map[K, V]:
    z = _find(_Z(None, map), key)
    match z.focus:
        case _B(_, _, _, _):
            z_new = z._replace(focus=z.focus._replace(value=value))
        case None:
            z_new = z._replace(focus=_B(key, value, None, None))
    return _root(z_new).focus

def remove(map: Map[K, V], key: K) -> Map[K, V]:
    z = _find(_Z(None, map), key)
    match z.focus:
        case _B(_, _, None, None):
            z_new = z._replace(focus=None)
        case _B(_, _, l, None):
            z_new = z._replace(focus=l)
        case _B(_, _, None, r):
            z_new = z._replace(focus=r)
        case _B(_, _, l, r):
            # the sucky case
            min_r = _min(_Z(None, r))
            assert min_r.focus is not None
            z_new = z._replace(
              focus=_B(
                  min_r.focus.key,
                  min_r.focus.value,
                  l,
                  _root(min_r._replace(focus=None)).focus
              )
            )
        case None:
            z_new = z
    return _root(z_new).focus

__all__ = [
    'Map',
    'empty',
    'get',
    'insert',
    'remove',
]
