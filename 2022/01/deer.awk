/^$/ { max = this > max ? this : max; this = 0 }
/[[:digit:]]+/ { this += $1; }
END { max = this > max ? this : max; print max }
