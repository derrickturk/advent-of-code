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
