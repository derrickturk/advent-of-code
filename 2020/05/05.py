import sys

def seat_id(pos: str) -> int:
    return int(''.join('1' if c == 'B' or c == 'R' else '0' for c in pos), 2)

if __name__ == '__main__':
    seats = set(seat_id(l.strip()) for l in sys.stdin)
    print(max(seats))
    for s in seats:
        if s - 2 in seats and not s - 1 in seats:
            print(s - 1)
