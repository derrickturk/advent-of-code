function read(src, mode) {
    switch (mode) {
        case 0: return code[src]
        case 1: return src
        default: print "Invalid read mode " mode >"/dev/stderr"; exit 1
    }
}

function write(dst, mode, word) {
    switch (mode) {
        case 0: code[dst] = word; break
        default: print "Invalid write mode " mode >"/dev/stderr"; exit 1
    }
}

# locals: word, val, op, mode1, mode2, mode3
function step(word, val) {
    word = code[ip]
    op = word % 100
    word = int(word / 100)
    mode1 = word % 10
    word = int(word / 10)
    mode2 = word % 10
    word = int(word / 10)
    mode3 = word % 10
    word = int(word / 10)
    switch (op) {
        case 1:
            write(code[ip + 3], mode3,
              read(code[ip + 1], mode1) + read(code[ip + 2], mode2))
            ip += 4
            return 1
        case 2:
            write(code[ip + 3], mode3,
              read(code[ip + 1], mode1) * read(code[ip + 2], mode2))
            ip += 4
            return 1
        case 3:
            if ((getline val <"/dev/stdin") == 0) {
                print "Input failure" >"/dev/stderr"
                exit 1
            }
            write(code[ip + 1], mode1, val + 0)
            ip += 2
            return 1
        case 4:
            print read(code[ip + 1], mode1)
            ip += 2
            return 1
        case 5:
            if (read(code[ip + 1], mode1) != 0)
                ip = read(code[ip + 2], mode2)
            else
                ip += 3
            return 1
        case 6:
            if (read(code[ip + 1], mode1) == 0)
                ip = read(code[ip + 2], mode2)
            else
                ip += 3
            return 1
        case 7:
            write(code[ip + 3], mode3,
              read(code[ip + 1], mode1) < read(code[ip + 2], mode2) ? 1 : 0)
            ip += 4
            return 1
        case 8:
            write(code[ip + 3], mode3,
              read(code[ip + 1], mode1) == read(code[ip + 2], mode2) ? 1 : 0)
            ip += 4
            return 1
        case 99:
            return 0
    }
}

BEGIN { FS = ","; ip = 0 }
{ for (i = 1; i <= NF; ++i) code[i - 1] = $i + 0 }
NR > 1 { print "Invalid input file" >"/dev/stderr"; exit 1 }
END {
    while (step() == 1);
}
