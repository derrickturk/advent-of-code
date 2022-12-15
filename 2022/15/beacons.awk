function abs(x) {
    return x < 0 ? -x : x
}

function manhattan(x0, y0, x1, y1) {
    return abs(x0 - x1) + abs(y0 - y1)
}

# local dist, i, x, dy
# walk the perimeter "+ 1"; i.e. of the diamond with one greater
#   manhattan radius
function for_perimeter1(record, f, dist, i, x, dy) {
    x_s = record["x_s"]
    y_s = record["y_s"]
    x_b = record["x_b"]
    y_b = record["y_b"]
    dist = record["radius"] + 1
    for (i = 0; i <= dist * 2; ++i) {
        x = x_s - dist + i
        d_y = dist - abs(x - x_s)
        @f(x, y_s - d_y)
        if (d_y > 0) {
            @f(x, y_s + d_y)
        }
    }
}

function printxy(x, y) {
    print x, y
}

function see(x, y) {
    if (x < 0 || y < 0 || x > part2_max_x || y > part2_max_y)
        return

    bad = 0
    for (k in known) {
        if (manhattan(known[k]["x_s"], known[k]["y_s"], x, y) <= known[k]["radius"]) {
            bad = 1
            break
        }
    }

    if (bad == 0) {
        print x, y, x * 4000000 + y
        exit 0
    }
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
    known[NR]["radius"] = manhattan($4, $6, $12, $14)
}

END {
    for (i = 1; i <= NR; ++i) {
        x_s = known[i]["x_s"]
        y_s = known[i]["y_s"]
        x_b = known[i]["x_b"]
        y_b = known[i]["y_b"]
        dist = known[i]["radius"]
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

    for (k in known) {
        for_perimeter1(known[k], "see")
    }

    print "no solution to part 2" >"/dev/stderr"
    exit 1
}
