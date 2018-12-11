use std::env;

type Matrix = Vec<Vec<i32>>;

#[inline]
fn hundreds(x: i32) -> i32 {
    x % 1000 / 100
}

fn cell_power(serial: i32, x: usize, y: usize) -> i32 {
    let rack_id = x as i32 + 10;
    hundreds((rack_id * y as i32 + serial) * rack_id) - 5
}

#[inline]
fn xy_to_ij(x: usize, y: usize) -> (usize, usize) {
    (y - 1, x - 1)
}

#[inline]
fn ij_to_xy(i: usize, j: usize) -> (usize, usize) {
    (j + 1, i + 1)
}

fn make_grid(size: usize, serial: i32) -> Matrix {
    let mut grid = Vec::with_capacity(size);
    for i in 0..size {
        let mut row = Vec::with_capacity(size);
        for j in 0..size {
            let (x, y) = ij_to_xy(i, j);
            row.push(cell_power(serial, x, y));
        }
        grid.push(row);
    }
    grid
}

fn summed_area(grid: &Matrix) -> Matrix {
    let mut sat = grid.clone();
    for i in 0..grid.len() {
        for j in 0..grid[i].len() {
            if j > 0 {
                sat[i][j] += sat[i][j - 1]
            }

            if i > 0 {
                sat[i][j] += sat[i - 1][j]
            }

            if i > 0 && j > 0 {
                sat[i][j] -= sat[i - 1][j - 1]
            }
        }
    }
    sat
}

#[inline]
fn rectangle_sum(sat: &Matrix,
                 i_min: usize, i_max: usize,
                 j_min: usize, j_max: usize) -> i32 {
    let d = sat[i_max][j_max];

    let a = if i_min == 0 || j_min == 0 {
        0
    } else {
        sat[i_min - 1][j_min - 1]
    };

    let b = if i_min == 0 {
        0
    } else {
        sat[i_min - 1][j_max]
    };

    let c = if j_min == 0 {
        0
    } else {
        sat[i_max][j_min - 1]
    };

    d + a - b - c
}

#[inline]
fn cell_square_sum(grid: &Matrix, sat: &Matrix,
                   x: usize, y: usize, w: usize) -> i32 {
    let (i, j) = xy_to_ij(x, y);
    if w == 1 {
        grid[i][j]
    } else {
        rectangle_sum(sat, i, i + w - 1, j, j + w - 1)
    }
}

fn max_power_square_sized(grid: &Matrix, sat: &Matrix,
                          w: usize) -> (usize, usize, i32) {
    if w > grid.len() {
        panic!("square too large for grid!"); 
    }

    let (mut x_max, mut y_max, mut max) = (0, 0, i32::min_value());
    for x in 1..=grid.len() - w + 1 {
        for y in 1..=grid.len() - w + 1 {
            let p = cell_square_sum(grid, sat, x, y, w);
            if p > max {
                x_max = x;
                y_max = y;
                max = p;
            }
        }
    }

    (x_max, y_max, max)
}

fn max_power_square(grid: &Matrix, sat: &Matrix) -> (usize, usize, usize, i32) {
    let (mut x_max, mut y_max, mut w_max, mut max) =
      (0, 0, 0, i32::min_value());
    for w in 1..=grid.len() {
        let (x, y, p) = max_power_square_sized(grid, sat, w);
        if p > max {
            x_max = x;
            y_max = y;
            w_max = w;
            max = p;
        }
    }

    (x_max, y_max, w_max, max)
}

#[allow(dead_code)]
fn print_grid(grid: &Matrix) {
    for row in grid.iter() {
        for elem in row.iter() {
            print!("{}, ", elem);
        }
        print!("\n");
    }
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: {} size serial",
          args.first().unwrap_or(&String::from("grid")));
        return;
    }

    let (size, serial) = match (args[1].parse(), args[2].parse()) {
        (Ok(sz), Ok(ser)) => (sz, ser),
        (Err(_), _) => {
            eprintln!("Invalid size \"{}\"", args[1]);
            return;
        },
        (_, Err(_)) => {
            eprintln!("Invalid serial number \"{}\"", args[2]);
            return;
        },
    };

    let grid = Box::new(make_grid(size, serial));
    let sat = Box::new(summed_area(&grid));

    let (x, y, p) = max_power_square_sized(&grid, &sat, 3);
    println!("{},{} (power {})", x, y, p);

    let (x, y, w, p) = max_power_square(&grid, &sat);
    println!("{},{},{} (power {})", x, y, w, p);
}
