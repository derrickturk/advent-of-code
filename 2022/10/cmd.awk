BEGIN { x = 1; cycle = 1; disp = "" }

function check() {
    if ((cycle - 20) % 40 == 0)
        result += cycle * x
    pos = (cycle - 1) % 40
    if (x == pos || x == pos - 1 || x == pos + 1)
        disp = disp "#"
    else
        disp = disp " "
    if (cycle % 40 == 0)
        disp = disp "\n"
}

$1 == "noop" { check(); ++cycle }
$1 == "addx" { check(); ++cycle; check(); ++cycle; x += $2 }

END {
    while (cycle < 220) {
        ++cycle
        check()
    }
    print result
    print disp
}
