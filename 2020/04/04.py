import sys

from typing import (
    Any,
    Callable,
    Dict,
    Iterable,
    List,
    Optional,
    TextIO,
    TypeVar,
    Tuple
)

# some cheeky parsers and combinators

T = TypeVar('T')
U = TypeVar('U')
Parser = Callable[[str], Optional[Tuple[T, str]]]

def pure(x: T) -> Parser[T]:
    return lambda s: (x, s)

def fail() -> Parser[T]:
    return lambda _: None

def unsigned(val: str) -> Optional[Tuple[int, str]]:
    digits = ''
    remaining = val
    while remaining and remaining[0].isdigit():
        digits += remaining[0]
        remaining = remaining[1:]
    if digits:
        return int(digits), remaining
    return None

def lit(literal: str) -> Parser[str]:
    def parse(val: str) -> Optional[Tuple[str, str]]:
        if val.startswith(literal):
            return literal, val[len(literal):]
        return None
    return parse

def any_char(chars: str) -> Parser[str]:
    def parse(val: str) -> Optional[Tuple[str, str]]:
        taken = ''
        remaining = val
        while remaining and remaining[0] in chars:
            taken += remaining[0]
            remaining = remaining[1:]
        if taken:
            return taken, remaining
        return None
    return parse

def char_where(fn: Callable[[str], bool]) -> Parser[str]:
    def parse(val: str) -> Optional[Tuple[str, str]]:
        if val and fn(val[0]):
            return val[0], val[1:]
        return None
    return parse

def n_chars(n: int) -> Parser[str]:
    def parse(val: str) -> Optional[Tuple[str, str]]:
        if len(val) < n:
            return None
        return val[:n], val[n:]
    return parse

def n_digit_unsigned(n: int) -> Parser[int]:
    # there's a nice way to do this with guard and lambda,
    #   but mypy sucks at lambdas, and Python sucks at lambdas
    def parse(val: str) -> Optional[Tuple[int, str]]:
        res = unsigned(val)
        if res is not None:
            parsed, rest = res
            if len(rest) == len(val) - n:
                return res
        return None
    return parse

def alt(*parsers: Parser[T]) -> Parser[T]:
    def parse(val: str) -> Optional[Tuple[T, str]]:
        for p in parsers:
            res = p(val)
            if res is not None:
                return res
        return None
    return parse

def map(p: Parser[T], fn: Callable[[T], U]) -> Parser[U]:
    def parse(val: str) -> Optional[Tuple[U, str]]:
        res = p(val)
        if res is None:
            return None
        parsed, rest = res
        return fn(parsed), rest
    return parse

def and_then(p: Parser[T], fn: Callable[[T], Parser[U]]) -> Parser[U]:
    def parse(val: str) -> Optional[Tuple[U, str]]:
        res = p(val)
        if res is None:
            return None
        parsed, rest = res
        next_p = fn(parsed)
        return next_p(rest)

    return parse

def followed_by(p: Parser[T], u: Parser[U]) -> Parser[U]:
    return and_then(p, lambda _: u)

def guard(p: Parser[T], fn: Callable[[T], bool]) -> Parser[T]:
    return and_then(p, lambda x: pure(x) if fn(x) else fail())

def just(p: Parser[T]) -> Parser[T]:
    def parse(val: str) -> Optional[Tuple[T, str]]:
        res = p(val)
        if res is not None:
            parsed, rest = res
            if rest == '':
                return parsed, rest
        return None
    return parse

def repeat(p: Parser[T], n: int) -> Parser[List[T]]:
    def parse(val: str) -> Optional[Tuple[List[T], str]]:
        results: List[T] = list()
        rest = val
        for _ in range(n):
            res = p(rest)
            if res is None:
                return None
            parsed, rest = res
            results.append(parsed)
        return results, rest
    return parse

def year_between(lower: int, upper: int) -> Parser[int]:
    return guard(n_digit_unsigned(4), lambda yr: lower <= yr <= upper)

def does_parse(p: Parser[T], val: str) -> bool:
    return just(p)(val) is not None

# fuck Python lambdas!
def height() -> Parser[Tuple[int, str]]:
    def bind_height(height: int) -> Parser[Tuple[int, str]]:
        def bind_unit(unit: str) -> Parser[Tuple[int, str]]:
            if unit == 'cm':
                if not 150 <= height <= 193:
                    return fail()
            else:
                if not 59 <= height <= 76:
                    return fail()
            return pure((height, unit))
        return and_then(alt(lit('cm'), lit('in')), bind_unit)
    return and_then(unsigned, bind_height)

REQUIRED_KEYS: Dict[str, Parser[Any]] = {
    'byr': year_between(1920, 2002),
    "iyr": year_between(2010, 2020),
    "eyr": year_between(2020, 2030),
    "hgt": height(),
    "hcl": followed_by(lit('#'), repeat(
             char_where(lambda c: c in '0123456789abcdef'), 6)),
    "ecl": alt(lit('amb'), lit('blu'), lit('brn'), lit('gry'),
             lit('grn'), lit('hzl'), lit('oth')),
    "pid": guard(n_chars(9), lambda cs: all(c.isdigit() for c in cs)),
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
    return all(k in passport and does_parse(v, passport[k])
      for (k, v) in REQUIRED_KEYS.items())

def main(argv: List[str]) -> int:
    passports = list(parse_passports(sys.stdin))
    print(f'{sum(1 if valid1(p) else 0 for p in passports)} valid1')
    print(f'{sum(1 if valid2(p) else 0 for p in passports)} valid2')
    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv))
