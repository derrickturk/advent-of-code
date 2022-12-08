# local tree, viz
function visible_any(trees, i, j, tree, viz) {
    tree = trees[i, j]

    viz = 1
    for (i_check = 1; i_check < i; ++i_check)
        if (trees[i_check, j] >= tree) {
            viz = 0
            break
        }
    if (viz != 0)
        return 1

    viz = 1
    for (i_check = i + 1; i_check <= NF; ++i_check)
        if (trees[i_check, j] >= tree) {
            viz = 0
            break
        }
    if (viz != 0)
        return 1

    viz = 1
    for (j_check = 1; j_check < j; ++j_check)
        if (trees[i, j_check] >= tree) {
            viz = 0
            break
        }
    if (viz != 0)
        return 1

    viz = 1
    for (j_check = j + 1; j_check <= NR; ++j_check)
        if (trees[i, j_check] >= tree) {
            viz = 0
            break
        }

    return viz
}

# local tree, east, west, north, south
function scenic_score(trees, i, j, tree, east, west, north, south) {
    tree = trees[i, j]

    for (i_check = i - 1; i_check >= 1; --i_check) {
        ++west
        if (trees[i_check, j] >= tree)
            break;
    }

    for (i_check = i + 1; i_check <= NF; ++i_check) {
        ++east
        if (trees[i_check, j] >= tree)
            break;
    }

    for (j_check = j - 1; j_check >= 1; --j_check) {
        ++north
        if (trees[i, j_check] >= tree)
            break;
    }

    for (j_check = j + 1; j_check <= NR; ++j_check) {
        ++south
        if (trees[i, j_check] >= tree)
            break;
    }

    return east * west * north * south
}

BEGIN { FS = "" }
{
    for (i = 1; i <= NF; ++i)
        trees[i, NR] = $i
}

END {
    max_ss = 0
    for (i = 1; i <= NF; ++i) {
        for (j = 1; j <= NR; ++j) {
            if (visible_any(trees, i, j))
                ++total
            ss = scenic_score(trees, i, j)
            if (ss > max_ss)
                max_ss = ss
        }
    }
    print total
    print max_ss
}
