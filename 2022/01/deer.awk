/^$/ { seen[NR] = this; this = 0 }
/[[:digit:]]+/ { this += $1; }
END {
    seen[NR] = this
    asort(seen, seen, "@val_num_desc")
    print seen[1]
    print seen[1] + seen[2] + seen[3]
}
