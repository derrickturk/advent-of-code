function abs(x) {
    return x < 0 ? -x : x
}

function manhattan(x0, y0, x1, y1) {
    return abs(x0 - x1) + abs(y0 - y1)
}

function cmp_x_s(i1, v1, i2, v2) {
    return v1["x_s"] - v2["x_s"]
}

BEGIN {
    FS = "[ =,:]+"
    part1_row = 2000000
    part2_max_x = 4000000
    part2_max_y = 4000000
    # part1_row = 10
    # part2_max_x = 20
    # part2_max_y = 20
}

{
    known[NR]["x_s"] = $4
    known[NR]["y_s"] = $6
    known[NR]["x_b"] = $12
    known[NR]["y_b"] = $14
}

END {
    asort(known, known, "cmp_x_s")

    for (i = 1; i <= NR; ++i) {
        x_s = known[i]["x_s"]
        y_s = known[i]["y_s"]
        x_b = known[i]["x_b"]
        y_b = known[i]["y_b"]
        dist = manhattan(x_s, y_s, x_b, y_b)
        y_dist = abs(y_s - part1_row)
        if (y_dist <= dist)
            for (j = 0; j <= (dist - y_dist); ++j) {
                if (y_b != part1_row || x_s + j != x_b)
                    seen[x_s + j] = 1
                if (y_b != part1_row || x_s - j != x_b)
                    seen[x_s - j] = 1
            }
    }

    for (v in seen)
        ++count

    print count

    # slow approach to part2... or is?
    for (j = 0; j <= part2_max_y; ++j) {
        i = 0
        while (1) {
            good = 1
            for (k = 1; k <= NR; ++k) {
                x_s = known[k]["x_s"]
                y_s = known[k]["y_s"]
                x_b = known[k]["x_b"]
                y_b = known[k]["y_b"]
                dist = manhattan(x_s, y_s, x_b, y_b)
                y_dist = abs(y_s - j)

                if (manhattan(x_s, y_s, i, j) <= dist) {
                    good = 0
                    i = x_s + (dist - y_dist + 1)
                    break
                }
            }

            if (good == 1) {
                print i * 4000000 + j
                exit 0
            }

            if (i > part2_max_x)
                break
        }
    }
}
