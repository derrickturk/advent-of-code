/^up/ { y -= $2; aim -= $2 }
/^down/ { y += $2; aim += $2 }
/^forward/ { x += $2; y2 += aim * $2 }
END { print x * y; print x * y2 }
