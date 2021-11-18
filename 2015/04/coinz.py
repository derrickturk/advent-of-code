import sys
from hashlib import md5

def proof_of_work(key: str, difficulty: int = 5) -> int:
    target = '0' * difficulty
    num = 0
    while not md5((key + str(num)).encode('ascii')).hexdigest().startswith(target):
        num += 1
    return num

def main(args: list[str]) -> int:
    match args:
        case [_, key]:
            print(proof_of_work(key, difficulty=5))
            print(proof_of_work(key, difficulty=6))
        case [prog, *_]:
            print(f'Usage: {prog} secret-key', file=sys.stderr)
            return 1
        case _:
            return 1
    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv))
