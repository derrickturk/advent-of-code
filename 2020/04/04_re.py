import sys
import re

from typing import (
    Any,
    Callable,
    Dict,
    Iterable,
    List,
    Match,
    Optional,
    Pattern,
    TextIO
)

class Validator:
    __slots__ = ('_re', '_pred')
    _re: Pattern[str]
    _pred: Optional[Callable[[Match[str]], bool]]

    def __init__(self, patt: str,
            pred: Optional[Callable[[Match[str]], bool]] = None) -> None:
        self._re = re.compile(patt)
        self._pred = pred

    def __call__(self, val: str) -> bool:
        match = self._re.fullmatch(val)
        if not match:
            return False
        if self._pred is not None:
            return self._pred(match)
        return True

def _check_height(m: Match[str]) -> bool:
    height = int(m.group(1))
    if m.group(2) == 'in':
        return 59 <= height <= 76
    return 150 <= height <= 193

REQUIRED_KEYS: Dict[str, Validator] = {
    'byr': Validator(r'\d{4}', lambda m: 1920 <= int(m.group(0)) <= 2002),
    'iyr': Validator(r'\d{4}', lambda m: 2010 <= int(m.group(0)) <= 2020),
    'eyr': Validator(r'\d{4}', lambda m: 2020 <= int(m.group(0)) <= 2030),
    'hgt': Validator(r'(\d+)(in|cm)', _check_height),
    'hcl': Validator(r'#[0-9a-f]{6}'),
    'ecl': Validator(r'(amb|blu|brn|gry|grn|hzl|oth)'),
    'pid': Validator(r'\d{9}'),
};

def parse_passports(stream: TextIO) -> Iterable[Dict[str, str]]:
    this_passport: Dict[str, str] = dict()
    for line in stream:
        line = line.rstrip()
        if line == '' and len(this_passport) > 0:
            yield this_passport
            this_passport = dict()
        for entry in line.split():
            k, v = entry.split(':')
            if k in this_passport:
                raise ValueError(f'duplicate key: {k}')
            this_passport[k] = v
    if len(this_passport) > 0:
        yield this_passport

def valid1(passport: Dict[str, str]) -> bool:
    return all(k in passport for k in REQUIRED_KEYS.keys())

def valid2(passport: Dict[str, str]) -> bool:
    return all(k in passport and v(passport[k])
      for (k, v) in REQUIRED_KEYS.items())

def main(argv: List[str]) -> int:
    passports = list(parse_passports(sys.stdin))
    print(f'{sum(1 if valid1(p) else 0 for p in passports)} valid1')
    print(f'{sum(1 if valid2(p) else 0 for p in passports)} valid2')
    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv))
