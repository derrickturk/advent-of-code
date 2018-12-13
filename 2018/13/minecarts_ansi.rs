use std::io::{self, Write, BufRead, BufWriter};
use std::collections::HashMap;
use std::thread;
use std::time;

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

fn move_carts<F: FnMut(Crash) -> ()>(track: &Track, carts: &mut CartGroup,
                                     mut crash_fn: F) {
    carts.sort();

    let mut i = 0;
    while i < carts.len() {
        move_cart(track, &mut carts[i]);

        let mut crashed = false;
        let mut j = 0;
        while i < carts.len() && j < carts.len() {
            if i != j && carts[i].i == carts[j].i && carts[i].j == carts[j].j {
                crash_fn(Crash(carts[i].i, carts[i].j));
                crashed = true;
                if i < j {
                    carts.remove(j);
                    carts.remove(i);
                    j -= 1;
                } else {
                    carts.remove(i);
                    carts.remove(j);
                    i -= 1;
                }
            } else {
                j += 1;
            }
        }

        if !crashed {
            i += 1;
        }
    }
}

fn render_track(track: &Track) -> Vec<Vec<u8>> {
    let mut piece_locs: Vec<_> = track.keys().collect();
    piece_locs.sort();

    let mut rows = Vec::new();
    let mut cols = Vec::new();
    for &(i, j) in piece_locs {
        while i > rows.len() {
            cols.push(b'\n');
            rows.push(cols);
            cols = Vec::new();
        }

        while j > cols.len() {
            cols.push(b' ');
        }

        let track_char = match track[&(i, j)] {
            Horizontal => b'-',
            Vertical => b'|',
            Intersection => b'+',
            CornerF => b'/',
            CornerB => b'\\',
        };
        cols.push(track_char);
    }

    if !cols.is_empty() {
        cols.push(b'\n');
        rows.push(cols);
    }

    rows
}

fn render_track_with_carts<W: Write>(stream: &mut BufWriter<W>,
                                     track_chars: &[Vec<u8>],
                                     carts: &CartGroup) -> io::Result<()> {
    for (i, chars) in track_chars.iter().enumerate() {
        for (j, c) in chars.iter().enumerate() {
            let c = match carts.iter().find(|k| k.i == i && k.j == j) {
                Some(Cart { dir, .. }) => match dir {
                    Up => b'^',
                    Down => b'v',
                    Left => b'<',
                    Right => b'>',
                },
                None => *c,
            };
            stream.write(&[c])?;
        }
    }
    Ok(())
}

fn reset_cursor<W: Write>(w: &mut W, rows: usize) -> io::Result<()> {
    let up_rows = rows.to_string();
    let mut code = Vec::new();
    code.push(27u8);
    code.push(b'[');
    code.extend(up_rows.as_bytes().iter());
    code.push(b'A');
    w.write_all(&code)?;
    Ok(())
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

    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    let track_chars = render_track(&track);
    for line in &track_chars {
        stdout.write_all(&line)?;
    }

    let mut stdout_buf = BufWriter::new(stdout);

    // part 2
    loop {
        if carts.len() == 0 {
            println!("hmm, all carts are gone."); 
            break;
        }

        if carts.len() == 1 {
            println!("final cart @ X,Y = {},{}",
              carts[0].j, carts[0].i);
            break;
        }

        thread::sleep(time::Duration::from_millis(500));
        move_carts(&mut track, &mut carts, |_| {});
        reset_cursor(&mut stdout_buf, track_chars.len())?;
        render_track_with_carts(&mut stdout_buf, &track_chars, &carts)?;
        stdout_buf.flush()?;
    }

    Ok(())
}
