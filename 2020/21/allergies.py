import sys

from typing import List, NamedTuple, Optional, Set

class Food(NamedTuple):
    ingredients: Set[str]
    allergens: Set[str]

def parse(inp: str) -> List[Food]:
    menu = list()
    for l in inp.split('\n'):
        if l == '':
            continue
        lefty, righty = l.rstrip().split('(')
        if righty[-1] != ')' or not righty.startswith('contains'):
            raise ValueError('invalid input')
        ingrs = set(lefty.strip().split())
        allergens = set(righty[len('contains '):-1].split(', '))
        menu.append(Food(ingrs, allergens))
    return menu

def ingredients_might_have(allergen: str, foods: List[Food]) -> Set[str]:
    maybe = { i for f in foods for i in f.ingredients }
    for f in foods:
        if allergen in f.allergens:
            maybe &= f.ingredients
    return maybe

def main() -> int:
    menu = parse(sys.stdin.read())

    safe_ingrs = { i for f in menu for i in f.ingredients }
    all_allergens = { a for f in menu for a in f.allergens }

    for a in all_allergens:
        safe_ingrs -= ingredients_might_have(a, menu)

    print(sum(1 for f in menu for i in f.ingredients if i in safe_ingrs))

    return 0

if __name__ == '__main__':
    sys.exit(main())
