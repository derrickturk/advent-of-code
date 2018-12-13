use std::io;
use std::io::BufRead;
use std::collections::HashMap;

use TrackPiece::*;
use Direction::*;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum TrackPiece {
    Horizontal,
    Vertical,
    Intersection,
    CornerF,
    CornerB,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct Cart {
    i: usize,
    j: usize,
    dir: Direction,
    n_int: usize,
}

type Track = HashMap<(usize, usize), TrackPiece>;
type CartGroup = Vec<Cart>;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
struct Crash(usize, usize);

fn build_row(track: &mut Track, carts: &mut CartGroup,
             i: usize, text: &str) -> bool {
    for (j, c) in text.chars().enumerate() {
        let (piece, cart) = match c {
            '-' => (Horizontal, None),
            '|' => (Vertical, None),
            '+' => (Intersection, None),
            '/' => (CornerF, None),
            '\\' => (CornerB, None),
            '^' => (Vertical, Some(Up)),
            'v' => (Vertical, Some(Down)),
            '>' => (Horizontal, Some(Right)),
            '<' => (Horizontal, Some(Left)),
            ' ' => continue,
            _ => return false,
        };
        track.insert((i, j), piece);
        if let Some(dir) = cart {
            carts.push(Cart { i, j, dir, n_int: 0 });
        }
    }
    true
}

fn turn_left(dir: Direction) -> Direction {
    match dir {
        Up => Left,
        Down => Right,
        Left => Down,
        Right => Up,
    }
}

fn turn_right(dir: Direction) -> Direction {
    match dir {
        Up => Right,
        Down => Left,
        Left => Up,
        Right => Down,
    }
}

fn move_cart(track: &Track, cart: &mut Cart) {
    let (new_i, new_j) = match cart.dir {
        Up => (cart.i - 1, cart.j),
        Down => (cart.i + 1, cart.j),
        Left => (cart.i, cart.j - 1),
        Right => (cart.i, cart.j + 1),
    };
    cart.i = new_i;
    cart.j = new_j;

    cart.dir = match track[&(new_i, new_j)] {
        Vertical => cart.dir,
        Horizontal => cart.dir,
        CornerF => match cart.dir {
            Up => Right,
            Down => Left,
            Left => Down,
            Right => Up,
        },
        CornerB => match cart.dir {
            Down => Right,
            Up => Left,
            Left => Up,
            Right => Down,
        },
        Intersection => {
            let dir = match cart.n_int % 3 {
                0 => turn_left(cart.dir),
                1 => cart.dir,
                2 => turn_right(cart.dir),
                _ => panic!("math broke"),
            };
            cart.n_int += 1;
            dir
        },
    };
}

fn move_carts(track: &Track, carts: &mut CartGroup) -> Option<Crash> {
    // i'd like a priority queue / ordered map
    carts.sort();
    for i in 0..carts.len() {
        move_cart(track, &mut carts[i]);
        if let Some(_) = carts.iter().enumerate().find(
              |&(o, c)| o != i && c.i == carts[i].i && c.j == carts[i].j) {
            return Some(Crash(carts[i].i, carts[i].j));
        }
    }
    None
}

fn main() -> io::Result<()> {
    let mut track = Track::new();
    let mut carts = CartGroup::new();

    let stdin = io::stdin();
    for (i, line) in stdin.lock().lines().into_iter().enumerate() {
        if !build_row(&mut track, &mut carts, i, &line?) {
            eprintln!("invalid row #{}", i);
            return Ok(())
        }
    }

    loop {
        if let Some(Crash(i, j)) = move_carts(&mut track, &mut carts) {
            // remember, they use X,Y rather than i, j
            println!("crash @ X,Y = {},{}", j, i);
            break;
        }
    }

    Ok(())
}
