function check(cond, r, val) {
    switch (cond) {
        case "<": return regs[r] < val;
        case ">": return regs[r] > val;
        case "<=": return regs[r] <= val;
        case ">=": return regs[r] >= val;
        case "==": return regs[r] == val;
        case "!=": return regs[r] != val;
        default: print "bad op" >"/dev/stderr"; exit 1
    }
}
/inc/ { if (check($6, $5, $7)) regs[$1] += $3 }
/dec/ { if (check($6, $5, $7)) regs[$1] -= $3 }
END { asort(regs, regs, "@val_num_desc"); print regs[1]; }
