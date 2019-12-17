import sys

def scaffold_cells(stream):
    cells = set()
    for i, line in enumerate(stream):
        for j, cell in enumerate(line.rstrip()):
            if cell in '#^<>v':
                cells.add((j, i))
    return cells

def intersections(cells):
    for (x, y) in cells:
        if ((x + 1, y) in cells and (x - 1, y) in cells
                and (x, y + 1) in cells and (x, y - 1) in cells):
            yield (x, y)

def main(argv):
    cells = scaffold_cells(sys.stdin)
    total = 0
    for (x, y) in intersections(cells):
        # print(f'int at {x}, {y} = {x * y}')
        total += x * y
    print(total)

if __name__ == '__main__':
    sys.exit(main(sys.argv))
