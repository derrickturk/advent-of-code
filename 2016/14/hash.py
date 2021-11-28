import sys
from hashlib import md5
from itertools import islice

from typing import Iterable, Optional, NamedTuple, Tuple

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

def keys(salt: str) -> Iterable[Tuple[int, str]]:
    ix = 0
    maybe_keys = []
    while True:
        hash = md5((salt + str(ix)).encode('ascii')).hexdigest()

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
            print(nth(keys(salt), 63))
        case [prog, *_]:
            print(f'Usage: {prog} salt', file=sys.stderr)
            return 1
        case _:
            return 1
    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv))
