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

function lose(they) { return they == "A" ? "Z" : they == "B" ? "X" : "Y" }
function draw(they) { return they == "A" ? "X" : they == "B" ? "Y" : "Z" }
function win(they) { return they == "A" ? "Y" : they == "B" ? "Z" : "X" }

$2 == "X" { score += 1; pick = lose($1) }
$2 == "Y" { score += 2; pick = draw($1) }
$2 == "Z" { score += 3; pick = win($1) }
pick == "X" { score2 += 1 }
pick == "Y" { score2 += 2 }
pick == "Z" { score2 += 3 }
{ score += outcome($1, $2); score2 += outcome($1, pick) }
END { print score; print score2 }
