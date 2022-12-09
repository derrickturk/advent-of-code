from typing import Generic, NamedTuple, TypeAlias, TypeVar

T = TypeVar('T')

# a persistent list, for use as a stack
class Cons(Generic[T], NamedTuple):
    head: T
    tail: 'List[T]'

List: TypeAlias = Cons[T] | None

__all__ = [
    'List',
    'Cons',
]
