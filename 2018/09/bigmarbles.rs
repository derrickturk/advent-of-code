// H E L L M A R B L E S
//  E  D  I  T  I  O  N

// for up to 2 ** 32 marbles

use std::io;
use std::fmt;

#[derive(Copy, Clone, Debug)]
struct Marble {
    next: u32,
    prev: u32,
}

#[derive(Clone, Debug)]
struct Board {
    marbles: Vec<Marble>,
    head: u32,
}

impl Board {
    #[inline]
    fn with_marbles(marbles: u32) -> Board {
        let mut marbles = Vec::with_capacity(marbles as usize);
        marbles.push(Marble { next: 0, prev: 0 });
        Board { marbles, head: 0 }
    }

    #[inline]
    fn rotate_cw(&mut self, n: u32) {
        for _ in 0..n {
            self.head = self.marbles[self.head as usize].next;
        }
    }

    #[inline]
    fn rotate_ccw(&mut self, n: u32) {
        for _ in 0..n {
            self.head = self.marbles[self.head as usize].prev;
        }
    }

    #[inline]
    fn insert_head(&mut self) {
        let next = self.marbles.len();
        let head = self.head;
        let old_prev = self.marbles[head as usize].prev;
        self.marbles.push(
            Marble {
                next: head,
                prev: old_prev,
            });
        self.marbles[old_prev as usize].next = next as u32;
        self.marbles[head as usize].prev = next as u32;
        self.head = next as u32;
    }

    #[inline]
    fn remove_head(&mut self) -> u32 {
        let old_head = self.head;
        let old_prev = self.marbles[old_head as usize].prev;
        self.head = self.marbles[self.head as usize].next;
        self.marbles[self.head as usize].prev = old_prev;
        self.marbles[old_prev as usize].next = self.head;
        old_head
    }

    #[inline]
    fn ignore_marble(&mut self) -> u32 {
        let next = self.marbles.len() as u32;
        self.marbles.push(
            Marble {
                next: next,
                prev: next,
            });
        next
    }

    fn place_marble(&mut self) -> u32 {
        let next = self.marbles.len();
        if next % 23 == 0 {
            self.rotate_ccw(7);
            let m = self.remove_head();
            let n = self.ignore_marble();
            m + n
        } else {
            self.rotate_cw(2);
            self.insert_head();
            0
        }
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({})", self.head)?;
        let mut next = self.marbles[self.head as usize].next;
        while next != self.head {
            write!(f, " {}", next)?;
            next = self.marbles[next as usize].next;
        }
        Ok(())
    }
}

fn parse_description(s: &str) -> Option<(usize, u32)> {
    let words: Vec<_> = s.split_whitespace().collect();
    if words.len() != 8 {
        None
    } else {
        Some((words[0].parse().ok()?, words[6].parse().ok()?))
    }
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    match parse_description(&input) {
        Some((players, marbles)) => {
            let mut board = Board::with_marbles(marbles);
            let mut scores = vec![0; players];
            for i in 0..marbles as usize {
                scores[i % players] += board.place_marble() as u64;
            }
            println!("max score: {}", scores.iter().max().unwrap());
        },
        None => eprintln!("invalid input"),
    };
    Ok(())
}
