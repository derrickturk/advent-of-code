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
    while (drop_sand() == 1);
    for (cell in sand)
        if (sand[cell] == 1)
            ++count
    print count
}
