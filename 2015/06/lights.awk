BEGIN { FS = "[ ,]" }

/^turn on/ {
    for (i = $3; i <= $6; ++i) {
        for (j = $4; j <= $7; ++j) {
            lights[i, j] = 1
            lights2[i, j] += 1
        }
    }
}

/^toggle/ {
    for (i = $2; i <= $5; ++i) {
        for (j = $3; j <= $6; ++j) {
            lights[i, j] = !lights[i, j]
            lights2[i, j] += 2
        }
    }
}

/^turn off/ {
    for (i = $3; i <= $6; ++i) {
        for (j = $4; j <= $7; ++j) {
            lights[i, j] = 0
            lights2[i, j] = lights2[i, j] == 0 ? 0 : lights2[i, j] - 1
        }
    }
}

END {
    for (cell in lights) {
        on += lights[cell]
        on2 += lights2[cell]
    }
    print on
    print on2
}
