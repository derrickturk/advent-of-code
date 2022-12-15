function abs(x) {
    return x < 0 ? -x : x
}

function manhattan(x0, y0, x1, y1) {
    return abs(x0 - x1) + abs(y0 - y1)
}

BEGIN { FS = "[ =,:]+"; part1_row = 2000000 }
{
    known[NR]["x_s"] = $4
    known[NR]["y_s"] = $6
    known[NR]["x_b"] = $12
    known[NR]["y_b"] = $14
}

END {
    for (i = 1; i < NR; ++i) {
        x_s = known[i]["x_s"]
        y_s = known[i]["y_s"]
        x_b = known[i]["x_b"]
        y_b = known[i]["y_b"]
        dist = manhattan(x_s, y_s, x_b, y_b)
        y_dist = abs(y_s - part1_row)
        if (y_dist <= dist)
            for (j = 0; j <= (dist - y_dist); ++j) {
                # print x_s, y_s, x_b, y_b, "induces", x_s + j, "and", x_s - j
                if (y_b != part1_row || x_s + j != x_b)
                    seen[x_s + j] = 1
                if (y_b != part1_row || x_s - j != x_b)
                    seen[x_s - j] = 1
            }
    }

    for (v in seen)
        ++count

    print count
}
