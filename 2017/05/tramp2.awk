{ prog[NR - 1] = $0 }
END {
    i = 0
    while (i < NR) {
        offset = prog[i]
        if (offset >= 3)
            --prog[i]
        else
            ++prog[i]
        i += offset
        ++steps
    }
    print steps
}
