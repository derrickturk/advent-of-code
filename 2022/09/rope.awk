function abs(x) {
    return x < 0 ? -x : x
}

# local di, dj
function movei(h, di, dj) {
    di = abs(is[h] - is[h + 1])
    dj = abs(js[h] - js[h + 1])

    if (di <= 1 && dj <= 1)
        return

    if (di == 2 && dj == 0)
        is[h + 1] += is[h] > is[h + 1] ? 1 : - 1
    else if (dj == 2 && di == 0)
        js[h + 1] += js[h] > js[h + 1] ? 1 : -1
    else if (di != 0 && dj != 0) {
        is[h + 1] += is[h] > is[h + 1] ? 1 : -1
        js[h + 1] += js[h] > js[h + 1] ? 1 : -1
    }

    vs[h + 1][is[h + 1], js[h + 1]] = 1
}

BEGIN {
    for (i = 0; i <= 9; ++i) {
        is[i] = 0
        js[i] = 0
        vs[i][0, 0] = 1
    }
}

$1 == "R" {
    for (i = 0; i < $2; ++i) {
        ++is[0]
        for (j = 0; j < 10; ++j)
            movei(j)
    }
}

$1 == "L" {
    for (i = 0; i < $2; ++i) {
        --is[0]
        for (j = 0; j < 10; ++j)
            movei(j)
    }
}

$1 == "D" {
    for (i = 0; i < $2; ++i) {
        ++js[0]
        for (j = 0; j < 10; ++j)
            movei(j)
    }
}

$1 == "U" {
    for (i = 0; i < $2; ++i) {
        --js[0]
        for (j = 0; j < 10; ++j)
            movei(j)
    }
}

END {
    for (cell in vs[1])
        ++count
    for (cell in vs[9])
        ++count2
    print count
    print count2
}
