function step() {
    switch (code[ip][1]) {
        case "hlf": regs[code[ip][2]] /= 2; ++ip; break
        case "tpl": regs[code[ip][2]] *= 3; ++ip; break
        case "inc": ++regs[code[ip][2]]; ++ip; break
        case "jmp": ip += code[ip][2]; break
        case "jie": ip += regs[code[ip][2]] % 2 ? 1 : code[ip][3]; break
        case "jio": ip += regs[code[ip][2]] == 1 ? code[ip][3] : 1; break
        default: print "bad opcode: " code[ip][1] >"/dev/stderr"; exit 1
    }
}

BEGIN { FS = "[ ,]+"; ip = 1; regs["a"] = a; regs["b"] = b }

{
    for (i = 1; i <= NF; ++i)
        code[NR][i] = $i
}

END {
    while (ip <= length(code))
        step()
    print regs["b"]
}
