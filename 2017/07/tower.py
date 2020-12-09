import sys

from typing import Dict, List, Iterable, NamedTuple, Optional, Tuple

class Disc(NamedTuple):
    weight: int
    children: List[str]

DiscWorld = Dict[str, Disc]

def parse_disc(line: str, world: DiscWorld) -> None:
    try_split = line.split('->')
    if len(try_split) == 2:
        spec, childspec = try_split
        children = childspec.strip().split(', ')
    else:
        spec = line
        children = []
    name, weight = spec.strip().split()
    world[name] = Disc(int(weight[1:-1]), children)

def parent_map(world: DiscWorld) -> Dict[str, str]:
    parents: Dict[str, str] = dict()
    for name, disc in world.items():
        for c in disc.children:
            parents[c] = name
    return parents

def root(parents: Dict[str, str]) -> str:
    where = next(iter(parents))
    while where in parents:
        where = parents[where]
    return where

def memo_weight(name: str, world: DiscWorld, memo: Dict[str, int]) -> int:
    if name in memo:
        return memo[name]
    w, cs = world[name]
    return w + sum(memo_weight(c, world, memo) for c in cs)

def balanced(name: str, world: DiscWorld, memo: Dict[str, int]) -> bool:
    _, cs = world[name]
    if len(cs) == 0:
        return True

    return all(memo_weight(cs[0], world, memo) == memo_weight(c, world, memo)
      for c in cs)

def modal(xs: Iterable[int]) -> int:
    counts: Dict[int, int] = dict()
    for x in xs:
        if x in counts:
            counts[x] += 1
        else:
            counts[x] = 1
    tups = list(counts.items())
    tups.sort(key=lambda t: t[1])
    return tups[-1][0]

def naughty(name: str, world: DiscWorld, memo: Dict[str, int]
        ) -> Optional[Tuple[str, int]]:
    w, cs = world[name]
    weights = [memo_weight(c, world, memo) for c in cs]
    if len(weights) > 0:
        modal_weight = modal(weights)
        for weight, c in zip(weights, cs):
            if weight != modal_weight and balanced(c, world, memo):
                self_weight = world[c].weight
                return (c, self_weight + modal_weight - weight)
            rec = naughty(c, world, memo)
            if rec is not None:
                return rec
    return None

def main() -> int:
    world: DiscWorld = dict()
    for line in sys.stdin:
        parse_disc(line, world)
    root_name = root(parent_map(world))
    print(root_name)
    memo: Dict[str, int] = dict()
    print(naughty(root_name, world, memo))
    return 0

if __name__ == '__main__':
    sys.exit(main())
