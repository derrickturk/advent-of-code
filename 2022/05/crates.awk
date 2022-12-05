BEGIN { FIELDWIDTHS = "4 4 4 4 4 4 4 4 4" }

/\[/ {
    for (i = 1; i <= 9; ++i) {
        if ($i !~ /^\s+$/) {
            stack[i] = stack[i] substr($i, 2, 1)
            stack2[i] = stack2[i] substr($i, 2, 1)
        }
    }
}

/^ 1/ {
    FIELDWIDTHS = ""; FS = " "
}

/^move/ {
    for (i = 0; i < $2; ++i) {
        stack[$6] = substr(stack[$4], 1, 1) stack[$6]
        stack[$4] = substr(stack[$4], 2)
    }

    to_move2 = substr(stack2[$4], 1, $2)
    stack2[$6] = to_move2 stack2[$6]
    stack2[$4] = substr(stack2[$4], $2 + 1)
}

END {
    ORS = ""
    for (i = 1; i <= 9; ++i) { print substr(stack[i], 1, 1) }
    print "\n"
    for (i = 1; i <= 9; ++i) { print substr(stack2[i], 1, 1) }
    print "\n"
}
