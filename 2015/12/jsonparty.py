import json
import sys

def treesum(tree) -> int:
    match tree:
        case int(n): return n
        case [*vals]: return sum(map(treesum, vals))
        case {**vals}: return sum(map(treesum, vals.values()))
        case _: return 0

def treesum_red(tree) -> int:
    match tree:
        case int(n): return n
        case [*vals]: return sum(map(treesum_red, vals))
        case {**vals} if 'red' in vals.values(): return 0
        case {**vals}: return sum(map(treesum_red, vals.values()))
        case _: return 0

def main() -> int:
    tree = json.load(sys.stdin)
    print(treesum(tree))
    print(treesum_red(tree))
    return 0

if __name__ == '__main__':
    sys.exit(main())
