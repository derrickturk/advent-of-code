function read(src) {
    return code[src]
}

function write(dst, word) {
    code[dst] = word
}

function step() {
    switch (code[ip]) {
        case 1:
            write(code[ip + 3], read(code[ip + 1]) + read(code[ip + 2]));
            ip += 4;
            return 1;
        case 2:
            write(code[ip + 3], read(code[ip + 1]) * read(code[ip + 2]));
            ip += 4;
            return 1;
        case 99: return 0
    }
}

BEGIN { FS = ","; ip = 0 }
{ for (i = 1; i <= NF; ++i) code[i - 1] = $i + 0 }
NR > 1 { print "Invalid input file" >"/dev/stderr"; exit 1 }
END {
    code[1] = 12
    code[2] = 2
    while (step() == 1);
    print code[0]
}
