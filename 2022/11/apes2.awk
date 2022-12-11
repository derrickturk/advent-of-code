function push(q, val) {
    if (empty(q)) {
        q["head"] = 0
        q["tail"] = 0
        q[0] = val
    } else {
        q[q["tail"] + 1] = val
        q["tail"] = q["tail"] + 1
    }
}

# local val, new_head
function pop(q, val, new_head) {
    if (empty(q))
        return ""
    val = q[q["head"]]
    new_head = q["head"] + 1
    delete q[q["head"]]
    if (new_head > q["tail"]) {
        delete q["head"]
        delete q["tail"]
    } else {
        q["head"] = new_head
    }
    return val
}

function peek(q) {
    if (empty(q))
        return ""
    return q[q["head"]]
}

function empty(q) {
    return q["head"] == ""
}

BEGIN { base = 1 }

/Monkey/ { sub(/:/, "", $2); m = $2 }
/Operation/ { op[m] = $5; operand[m] = $6 }
/Test/ { test[m] = $4; base *= $4 }
/true/ { true[m] = $6 }
/false/ { false[m] = $6 }
/Starting/ {
    items[m]["_"] = 0 # dumb but required to make items[m] an array
                      #   before calling queue functions
    for (i = 3; i <= NF; ++i) {
        sub(/,/, "", $i)
        push(items[m], $i)
    }
}

# local other
function apply(i, val, other) {
    other = operand[i] == "old" ? val : operand[i]
    if (op[i] == "+")
        return (val + other) % base
    if (op[i] == "*")
        return (val * other) % base
    print "bad operator: " op[i] >"/dev/stderr"
    exit 1
}

# local item
function run_monkey(i, item) {
    while (!empty(items[i])) {
        item = apply(i, pop(items[i]))
        if (item % test[i] == 0)
            push(items[true[i]], item)
        else
            push(items[false[i]], item)
        ++seen[i]
    }
}

END {
    for (round = 0; round < 10000; ++round)
        for (j = 0; j <= m; ++j)
            run_monkey(j)
    asort(seen, seen, "@val_num_desc")
    print seen[1] * seen[2]
}
