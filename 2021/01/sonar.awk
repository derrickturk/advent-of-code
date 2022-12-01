BEGIN { last = "+inf" + 0 }

{
    if ($0 > last)
        ++increases
    last = $0
}

END { print increases }
