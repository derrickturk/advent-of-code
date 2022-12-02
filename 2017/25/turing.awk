function step() {
    val = tape[pos] + 0
    tape[pos] = rules[state][val][0]
    pos += rules[state][val][1]
    state = rules[state][val][2]
}
function rstrip(s) { return substr(s, 1, length(s) - 1) }
BEGIN { pos = 0 }
/^Begin/ { state = rstrip($4) }
/^Perform/ { n_steps = $6 }
/^In/ { rule_state = rstrip($3) }
/If/ { rule_value = rstrip($6) }
/Write/ { rules[rule_state][rule_value][0] = rstrip($5) }
/right/ { rules[rule_state][rule_value][1] = 1 }
/left/ { rules[rule_state][rule_value][1] = -1 }
/Continue/ { rules[rule_state][rule_value][2] = rstrip($5) }
END {
    for (i = 0; i < n_steps; ++i)
        step()
    for (j in tape)
        if (tape[j] == 1)
            ++checksum
    print checksum
}
