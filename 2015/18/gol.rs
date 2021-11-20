// a game of lights

use std::{
    error::Error,
    io,
};

mod gridworld;
use gridworld::{Cell, Grid};

fn main() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let mut grid = Grid::parse(&mut stdin).ok_or("couldn't parse grid")?;
    let mut grid2 = grid.clone();

    for _ in 0..4 {
        grid = grid.update(|gr| {
            let live =
              gr.neighbor_values_diagonal().filter(|&v| v == Cell::On).count();
            match gr.read() {
                Cell::Off => {
                    if live == 3 {
                        Cell::On
                    } else {
                        Cell::Off
                    }
                },
                Cell::On => {
                    if live == 2 || live == 3 {
                        Cell::On
                    } else {
                        Cell::Off
                    }
                },
            }
        });
    }

    // gahh
    let last_row = grid2.rows() - 1;
    let last_col = grid2.cols() - 1;
    grid2[(0, 0)] = Cell::On;
    grid2[(0, last_col)] = Cell::On;
    grid2[(last_row, 0)] = Cell::On;
    grid2[(last_row, last_col)] = Cell::On;

    for _ in 0..100 {
        grid2 = grid2.update(|gr| {
            if gr.is_corner() {
                return Cell::On;
            }
            let live =
              gr.neighbor_values_diagonal().filter(|&v| v == Cell::On).count();
            match gr.read() {
                Cell::Off => {
                    if live == 3 {
                        Cell::On
                    } else {
                        Cell::Off
                    }
                },
                Cell::On => {
                    if live == 2 || live == 3 {
                        Cell::On
                    } else {
                        Cell::Off
                    }
                },
            }
        });
    }

    println!("{}", grid);
    dbg!(grid.count_on());

    println!("{}", grid2);
    dbg!(grid2.count_on());

    Ok(())
}
