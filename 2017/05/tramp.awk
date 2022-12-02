{ prog[NR - 1] = $0 }
END {
    i = 0
    while (i < NR) {
        offset = prog[i]
        ++prog[i]
        i += offset
        ++steps
    }
    print steps
}
