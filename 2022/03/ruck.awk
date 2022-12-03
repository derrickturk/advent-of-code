BEGIN {
    FPAT = "[A-Za-z]"
    for (i = 0; i < 26; ++i) {
        priority[sprintf("%c", 97 + i)] = i + 1
        priority[sprintf("%c", 65 + i)] = i + 27
    }
}

{
    delete first_half
    delete second_half
    delete threeset[int(NR % 3)]
    for (i = 1; i <= NF / 2; ++i) {
        first_half[$i] = 1
        second_half[$(i + NF / 2)] = 1
        threeset[int(NR % 3)][$i] = 1
        threeset[int(NR % 3)][$(i + NF / 2)] = 1
    }
    for (c in first_half)
        if (second_half[c] == 1)
            score += priority[c]
}

NR % 3 == 0 {
    for (c in threeset[1])
        if (threeset[2][c] == 1 && threeset[0][c] == 1)
            threescore += priority[c]
}

END { print score; print threescore }
