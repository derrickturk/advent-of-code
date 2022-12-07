/^\$/ && $2 == "cd" {
    if ($3 == "/") {
        dir = "/"
    } else if ($3 == "..") {
        sub(/\w+\/$/, "", dir)
    } else {
        dir = dir $3 "/"
    }
}

/^[0-9]/ {
    sizes[dir] += $1
    for (d in sizes)
        if (d != dir && index(dir, d) == 1)
            sizes[d] += $1
}

END {
    for (d in sizes) {
        if (sizes[d] <= 100000)
            result += sizes[d]
    }
    print result
}
