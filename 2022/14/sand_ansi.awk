BEGIN { FS = " -> |,"; at_rest = 1; bottom = 0 }

# local sand_x, sand_y
function drop_sand(sand_x, sand_y) {
    sand_x = 500
    sand_y = 0

    while (1) {
        if (rock[sand_x, sand_y + 1] != 1 && sand[sand_x, sand_y + 1] != 1) {
            ++sand_y
        } else if (rock[sand_x - 1, sand_y + 1] != 1 && sand[sand_x - 1, sand_y + 1] != 1) {
            --sand_x
            ++sand_y
        } else if (rock[sand_x + 1, sand_y + 1] != 1 && sand[sand_x + 1, sand_y + 1] != 1) {
            ++sand_x
            ++sand_y
        } else {
            break
        }

        if (sand_y > bottom)
            return 0
    }

    sand[sand_x, sand_y] = 1

    return 1
}

# local parts
function bounds(parts) {
    min_x = 500
    max_x = 500
    min_y = 0
    max_y = 0
    for (v in rock) {
        split(v, parts, SUBSEP)
        min_x = parts[1] < min_x ? parts[1] : min_x
        max_x = parts[1] > max_x ? parts[1] : max_x
        min_y = parts[2] < min_y ? parts[2] : min_y
        max_y = parts[2] > max_y ? parts[2] : max_y
    }
    for (v in sand) {
        split(v, parts, SUBSEP)
        min_x = parts[1] < min_x ? parts[1] : min_x
        max_x = parts[1] > max_x ? parts[1] : max_x
        min_y = parts[2] < min_y ? parts[2] : min_y
        max_y = parts[2] > max_y ? parts[2] : max_y
    }
}

function render(min_x, max_x, min_y, max_y, clr) {
    printf "%s%s", "\033[?25l\033[;H", clr == 1 ? "\033[J" : ""
    for (j = min_y; j <= max_y; ++j) {
        for (i = min_x; i <= max_x; ++i) {
            if (rock[i, j] == 1) {
                printf "\033[48;5;245m\033[38;5;235m#\033[39m\033[49m"
            } else if (sand[i, j] == 1) {
                printf "\033[49m\033[38;5;220mo\033[39m\033[49m"
            } else {
                printf " "
            }
        }
        printf "\n"
    }
    printf "\033[?25h"
}

# local sand_x, sand_y
function drop_sand2(sand_x, sand_y) {
    sand_x = 500
    sand_y = 0

    while (1) {
        if (sand_y + 1 == bottom + 2)
            break
        if (rock[sand_x, sand_y + 1] != 1 && sand2[sand_x, sand_y + 1] != 1) {
            ++sand_y
        } else if (rock[sand_x - 1, sand_y + 1] != 1 && sand2[sand_x - 1, sand_y + 1] != 1) {
            --sand_x
            ++sand_y
        } else if (rock[sand_x + 1, sand_y + 1] != 1 && sand2[sand_x + 1, sand_y + 1] != 1) {
            ++sand_x
            ++sand_y
        } else {
            break
        }
    }

    sand2[sand_x, sand_y] = 1

    if (sand_x == 500 && sand_y == 0)
        return 0

    return 1
}

{
    for (i = 3; i < NF; i += 2) {
        if ($i == $(i - 2)) {
            y_min = $(i - 1) < $(i + 1) ? $(i - 1) : $(i + 1)
            y_max = $(i - 1) > $(i + 1) ? $(i - 1) : $(i + 1)
            for (y = y_min; y <= y_max; ++y)
                rock[$i, y] = 1
        } else if ($(i + 1) == $(i - 1)) {
            x_min = $(i - 2) < $i ? $(i - 2) : $i
            x_max = $(i - 2) > $i ? $(i - 2) : $i
            for (x = x_min; x <= x_max; ++x)
                rock[x, $(i + 1)] = 1
        }
        bottom = $(i + 1) > bottom ? $(i + 1) : bottom
    }
}

END {
    bounds()
    render(min_x, max_x, min_y, max_y, 1)
    while (drop_sand() == 1)
        render(min_x, max_x, min_y, max_y, 0)
    for (cell in sand)
        if (sand[cell] == 1)
            ++count
    print count

    while (drop_sand2() == 1);
    for (cell in sand2)
        if (sand2[cell] == 1)
            ++count2
    print count2
}
