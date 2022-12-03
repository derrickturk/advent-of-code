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
    for (i = 1; i <= NF / 2; ++i) {
        first_half[$i] = 1
        second_half[$(i + NF / 2)] = 1
    }
    for (c in first_half)
        if (second_half[c] == 1)
            score += priority[c]
}

END { print score }
