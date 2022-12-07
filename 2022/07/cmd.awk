BEGIN { sizes["/"] = 0 }

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
    for (d in sizes)
        if (index(dir, d) == 1)
            sizes[d] += $1
}

/^dir/ {
    sizes[dir $2 "/"] = sizes[dir $2 "/"] + 0
}

END {
    gap = 30000000 - (70000000 - sizes["/"])
    asort(sizes, sizes, "@val_num_asc")
    for (d in sizes) {
        if (sizes[d] <= 100000)
            result += sizes[d]
        if (result2 == "" && sizes[d] >= gap)
            result2 = sizes[d]
    }
    print result
    print result2
}
