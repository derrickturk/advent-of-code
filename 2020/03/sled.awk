# set dx and dy with -v
BEGIN { x = 1; y = 1; FPAT = "[#.]" }
y == NR && $x == "#" { ++wrecks }
y == NR { y += dy; x += dx; x = (x - 1) % NF + 1 }
END { print wrecks }
