import sys
from hashlib import md5
from itertools import islice

from typing import Callable, Dict, Iterable, Optional, NamedTuple, Tuple

def nth(iterable, n, default=None):
    "Returns the nth item or a default value"
    return next(islice(iterable, n, None), default)

class MaybeKey(NamedTuple):
    ix: int
    hash: str
    repeated: str
    expires: int
    confirmed: bool

def run(s: str, n: int) -> Optional[str]:
    count = 0
    last = None
    for c in s:
        if c == last:
            count += 1
            if count >= n:
                return c
        else:
            count = 1
            last = c
    return None

def has_run(s: str, n: int, of: str) -> bool:
    count = 0
    for c in s:
        if c == of:
            count += 1
            if count == n:
                return True
        else:
            count = 0
    return False

def hash1(s: str) -> str:
    return md5(s.encode('ascii')).hexdigest()

def hashn(s: str, n: int) -> str:
    for _ in range(n + 1):
        s = hash1(s)
    return s

def cachy_hashn(s: str, n: int) -> str:
    seen: Dict[str, str] = {}
    for _ in range(n + 1):
        if s in seen:
            s = seen[s]
        else:
            h = hash1(s)
            seen[s] = h
            s = h
    return s

def keys(salt: str, hasher: Callable[[str], str] = hash1
  ) -> Iterable[Tuple[int, str]]:
    ix = 0
    maybe_keys = []
    while True:
        hash = hasher(salt + str(ix))

        maybe_keys = [
            m._replace(confirmed = m.confirmed or has_run(hash, 5, m.repeated))
            for m in maybe_keys
            if ix < m.expires
        ]

        if maybe_keys and maybe_keys[0].confirmed:
            yield (maybe_keys[0].ix, maybe_keys[0].hash)
            maybe_keys = maybe_keys[1:]

        match run(hash, 3):
            case str(c):
                maybe_keys.append(MaybeKey(ix=ix, hash=hash, repeated=c,
                    expires=ix + 1001, confirmed=False))
            case None:
                pass

        ix += 1

def main(args: list[str]) -> int:
    match args:
        case [_, salt]:
            # print(nth(keys(salt), 63))
            print(nth(keys(salt, lambda s: cachy_hashn(s, 2016)), 63))
        case [prog, *_]:
            print(f'Usage: {prog} salt', file=sys.stderr)
            return 1
        case _:
            return 1
    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv))
