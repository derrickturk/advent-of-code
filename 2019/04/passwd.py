import sys

def valid1(passwd):
    passwd_digits = str(passwd)
    seen_dup = False
    for i in range(1, len(passwd_digits)):
        if passwd_digits[i] < passwd_digits[i - 1]:
            return False
        elif passwd_digits[i] == passwd_digits[i - 1]:
            seen_dup = True
    return seen_dup

def valid2(passwd):
    passwd_digits = str(passwd)
    seen_run = 1
    seen_exact_2 = False
    for i in range(1, len(passwd_digits)):
        if passwd_digits[i] < passwd_digits[i - 1]:
            return False
        elif passwd_digits[i] == passwd_digits[i - 1]:
            seen_run += 1
        else:
            if seen_run == 2:
                seen_exact_2 = True
            seen_run = 1
    seen_exact_2 = seen_exact_2 or seen_run == 2
    return seen_exact_2

def main(argv):
    _, start, last = argv
    start = int(start)
    last = int(last)
    print(sum(1 for _ in filter(valid1, range(start, last + 1))))
    print(sum(1 for _ in filter(valid2, range(start, last + 1))))
    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv))
