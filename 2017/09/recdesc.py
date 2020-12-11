import sys

from typing import List, Tuple, Union

# a Group is a List of either Groups or Garbage
class Group:
    def __init__(self, contents: 'List[Union[Group, Garbage]]') -> None:
        self.contents = contents

    def __repr__(self) -> str:
        return f'Group({self.contents})'

    def score(self, nesting_level: int = 1) -> int:
        return nesting_level + sum(
          g.score(nesting_level + 1)
          for g in self.contents if isinstance(g, Group))

    def total_garbage(self) -> int:
        total = 0
        for g in self.contents:
            if isinstance(g, Group):
                total += g.total_garbage()
            elif isinstance(g, Garbage):
                total += g.garbage_chars()
            else:
                raise ValueError('Run mypy you buffoon!!!!!')
        return total

class Escaped:
    def __init__(self, char: str) -> None:
        self.char = char

    def __repr__(self) -> str:
        return f'Escaped({self.char})'

# a Garbage is a List of either ordinary chars or Escaped chars
class Garbage:
    def __init__(self, contents: 'List[Union[str, Escaped]]') -> None:
        self.contents = contents

    def __repr__(self) -> str:
        return f'Garbage({self.contents})'

    def garbage_chars(self) -> int:
        return sum(1 for c in self.contents if not isinstance(c, Escaped))

def item(inp: str) -> Tuple[Union[Group, Garbage], str]:
    try:
        return group(inp)
    except:
        return garbage(inp)

def group(inp: str) -> Tuple[Group, str]:
    if inp[0] != '{':
        raise ValueError("Expected '{'")
    contents = list()
    rest = inp[1:]

    try:
        first_item, rest = item(inp[1:])
        contents.append(first_item)
        while rest[0] == ',':
            rest = rest[1:]
            next_item, rest = item(rest)
            contents.append(next_item)
    except:
        pass

    if rest[0] != '}':
        raise ValueError("Expected '}'")

    return Group(contents), rest[1:]

def garbage(inp: str) -> Tuple[Garbage, str]:
    if inp[0] != '<':
        raise ValueError("Expected '<'")
    contents: List[Union[str, Escaped]] = list()
    rest = inp[1:]

    while rest[0] != '>':
        if rest[0] == '!':
            contents.append(Escaped(rest[1]))
            rest = rest[2:]
        else:
            contents.append(rest[0])
            rest = rest[1:]

    if rest[0] != '>':
        raise ValueError("Expected '>'")

    return Garbage(contents), rest[1:]

def main() -> None:
    stream, rest = group(sys.stdin.read().strip())
    if len(rest) > 0:
        print('remaining characters - invalid input!')
    else:
        print(stream.score())
        print(stream.total_garbage())

if __name__ == '__main__':
    main()
