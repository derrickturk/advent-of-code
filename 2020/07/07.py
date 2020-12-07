import sys

from typing import Dict, List, NamedTuple, TextIO, Tuple

class Rule(NamedTuple):
    color: str
    contents: List[Tuple[int, str]]

def parse_rule(line: str) -> Rule:
    outer, inner = line.split('contain')
    adj, col, _ = outer.strip().split()
    contents = inner.strip().split(',')
    color = f'{adj} {col}'
    content_desc: List[Tuple[int, str]] = list()
    if contents[0] == 'no other bags.':
        return Rule(color, content_desc)
    for cont in contents:
        num, adj, col, _ = cont.split()
        content_desc.append((int(num), f'{adj} {col}'))
    return Rule(color, content_desc)

def parse_ruletable(stream: TextIO) -> Dict[str, Rule]:
    result: Dict[str, Rule] = dict()
    for line in stream:
        rule = parse_rule(line)
        result[rule.color] = rule
    return result

def reachable(rules: Dict[str, Rule], source: str, target: str) -> bool:
    if source == target:
        return True
    for child in rules[source].contents:
        if reachable(rules, child[1], target):
            return True
    return False

def contained_bags(rules: Dict[str, Rule], source: str) -> int:
    sum = 0
    for child in rules[source].contents:
        count, color = child
        sum += count
        sum += count * contained_bags(rules, color)
    return sum

def main() -> int:
    rules = parse_ruletable(sys.stdin)
    print(sum(
        1 if reachable(rules, source, 'shiny gold') and source != 'shiny gold' else 0
        for source in rules))
    print(contained_bags(rules, 'shiny gold'))
    return 0

if __name__ == '__main__':
    sys.exit(main())
