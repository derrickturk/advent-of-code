function outcome(they, you) {
    switch (they you) {
        case "AY":
        case "BZ":
        case "CX": return 6;
        case "AX":
        case "BY":
        case "CZ": return 3;
        default: return 0;
    }
}

$2 == "X" { score += 1 }
$2 == "Y" { score += 2 }
$2 == "Z" { score += 3 }
{ score += outcome($1, $2) }
END { print score }
