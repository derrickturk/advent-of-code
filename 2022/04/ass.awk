function between(x, a, b) {
    return x >= a && x <= b
}
BEGIN { FS = "[-,]" }
($1 <= $3 && $2 >= $4) || ($3 <= $1 && $4 >= $2) { ++count }
between($1, $3, $4) || between($2, $3, $4) || between($3, $1, $2) || between($4, $1, $2) { ++count2 }
END { print count; print count2 }
