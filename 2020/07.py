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

def main() -> int:
    rules = parse_ruletable(sys.stdin)
    print(sum(
        1 if reachable(rules, source, 'shiny gold') and source != 'shiny gold' else 0
        for source in rules))
    return 0

if __name__ == '__main__':
    sys.exit(main())
