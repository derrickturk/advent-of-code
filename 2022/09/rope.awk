function abs(x) {
    return x < 0 ? -x : x
}

function move(di, dj) {
    di = abs(hi - ti)
    dj = abs(hj - tj)
    if (di <= 1 && dj <= 1)
        return

    if (di == 2 && dj == 0)
        ti += hi > ti ? 1 : - 1
    else if (dj == 2 && di == 0)
        tj += hj > tj ? 1 : -1
    else if (di != 0 && dj != 0) {
        ti += hi > ti ? 1 : -1
        tj += hj > tj ? 1 : -1
    }

    v[ti, tj] = 1
}

BEGIN { hi = 0; hj = 0; ti = 0; tj = 0; v[0, 0] = 1 }

$1 == "R" { for (i = 0; i < $2; ++i) { ++hi; move() } }
$1 == "L" { for (i = 0; i < $2; ++i) { --hi; move() } }
$1 == "D" { for (i = 0; i < $2; ++i) { ++hj; move() } }
$1 == "U" { for (i = 0; i < $2; ++i) { --hj; move() } }

END {
    for (cell in v)
        ++count
    print count
}
