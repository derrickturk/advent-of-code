function read(src) {
    return src ~ "[abcd]" ? regs[src] : src
}

function step() {
    switch (code[ip][1]) {
        case "cpy": regs[code[ip][3]] = read(code[ip][2]); ++ip; break
        case "inc": ++regs[code[ip][2]]; ++ip; break
        case "dec": --regs[code[ip][2]]; ++ip; break
        case "jnz":
            ip += read(code[ip][2]) ? code[ip][3] : 1; break
        default: print "bad opcode: " code[ip][1] >"/dev/stderr"; exit 1
    }
}

BEGIN { ip = 1; regs["a"] = a; regs["b"] = b; regs["c"] = c; regs["d"] = d }

{
    for (i = 1; i <= NF; ++i)
        code[NR][i] = $i
}

END {
    while (ip <= length(code))
        step()
    print regs["a"]
}
