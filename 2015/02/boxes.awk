function max(a, b) { return b > a ? b : a }
BEGIN { FS = "x" }
{
    biggest = max($1, max($2, $3))
    paper += 2 * ($1 * $2 + $1 * $3 + $2 * $3) + $1 * $2 * $3 / biggest
    ribbon += ($1 + $2 + $3 - biggest) * 2 + $1 * $2 * $3
}
END { print paper; print ribbon }
