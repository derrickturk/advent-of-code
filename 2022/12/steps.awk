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

# local parts, this
function push_neighbors(q, ix, parts, this) {
    split(ix, parts, SUBSEP)
    this = parts[1] SUBSEP parts[2]
    left = (parts[1] - 1) SUBSEP parts[2]
    right = (parts[1] + 1) SUBSEP parts[2]
    up = parts[1] SUBSEP (parts[2] - 1)
    down = parts[1] SUBSEP (parts[2] + 1)
    if (map[left] && map[left] - map[this] <= 1)
        push(q, left SUBSEP parts[3] + 1)
    if (map[right] && map[right] - map[this] <= 1)
        push(q, right SUBSEP parts[3] + 1)
    if (map[up] && map[up] - map[this] <= 1)
        push(q, up SUBSEP parts[3] + 1)
    if (map[down] && map[down] - map[this] <= 1)
        push(q, down SUBSEP parts[3] + 1)
}

# local q, s, s_pos, parts, seen
function min_cost(q, s, s_pos, parts, seen) {
    delete q
    delete seen
    push(q, start SUBSEP 0)
    while (!empty(q)) {
        s = pop(q)
        split(s, parts, SUBSEP)
        s_pos = parts[1] SUBSEP parts[2]
        if (s_pos == end)
            return parts[3]
        if (seen[s_pos])
            continue
        seen[s_pos] = 1
        push_neighbors(q, s)
    }
}

BEGIN {
    for (i = 0; i < 26; ++i)
        elev[sprintf("%c", i + 97)] = i + 1
    elev["S"] = 1
    elev["E"] = 26
    FS = ""
}

{
    for (i = 1; i <= NF; ++i) {
        map[i - 1, NR - 1] = elev[$i]
        if ($i == "S")
            start = (i - 1) SUBSEP (NR - 1)
        if ($i == "E")
            end = (i - 1) SUBSEP (NR - 1)
    }
}

END {
    print min_cost()
}
