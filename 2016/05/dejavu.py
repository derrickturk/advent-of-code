import sys
from hashlib import md5

def proof_of_work(key: str, difficulty: int = 5) -> int:
    target = '0' * difficulty
    num = 0
    while not md5((key + str(num)).encode('ascii')).hexdigest().startswith(target):
        num += 1
    return num

def passwd(key: str) -> str:
    salt = 0
    pwd = []
    for _ in range(8):
        hash = md5((key + str(salt)).encode('ascii')).hexdigest()
        while not hash.startswith('00000'):
            salt += 1
            hash = md5((key + str(salt)).encode('ascii')).hexdigest()
        pwd.append(hash[5])
        salt += 1
    return ''.join(pwd)

def passwd2(key: str) -> str:
    salt = 0
    pwd = [None] * 8
    while any(p is None for p in pwd):
        hash = md5((key + str(salt)).encode('ascii')).hexdigest()
        while not hash.startswith('00000'):
            salt += 1
            hash = md5((key + str(salt)).encode('ascii')).hexdigest()
        if '0' <= hash[5] < '8':
            pos = int(hash[5])
            if pwd[pos] is None:
                pwd[pos] = hash[6]
        salt += 1
    return ''.join(pwd)

def main(args: list[str]) -> int:
    match args:
        case [_, key]:
            print(passwd(key))
            print(passwd2(key))
        case [prog, *_]:
            print(f'Usage: {prog} secret-key', file=sys.stderr)
            return 1
        case _:
            return 1
    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv))
