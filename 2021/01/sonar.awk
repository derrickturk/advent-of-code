BEGIN { last = "+inf" + 0 }

{
    if ($0 > last)
        ++increases
    last = $0

    last_threesum = threesum
    threesum -= buf[0]
    buf[0] = buf[1]
    buf[1] = buf[2]
    buf[2] = $0
    threesum += $0
}

NR > 3 {
    if (threesum > last_threesum)
        ++threesum_increases
}

END { print increases; print threesum_increases }
