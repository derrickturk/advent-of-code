use std::{
    io::{self, BufRead},
    error::Error,
};

trait Bit {
    fn as_bit(&self) -> u8;
}

#[derive(Debug, Copy, Clone)]
enum RowDirection {
    Front = 0,
    Back = 1,
}

impl Bit for RowDirection {
    fn as_bit(&self) -> u8 { *self as u8 }
}

#[derive(Debug, Copy, Clone)]
enum SeatDirection {
    Left = 0,
    Right = 1,
}

impl Bit for SeatDirection {
    fn as_bit(&self) -> u8 { *self as u8 }
}

struct SeatPos {
    row: [RowDirection; 7],
    seat: [SeatDirection; 3],
}

impl SeatPos {
    fn from(text: &str) -> Option<Self> {
        if text.len() != 10 {
            return None
        }

        let mut row = [RowDirection::Front; 7];
        let mut seat = [SeatDirection::Left; 3];

        let (row_chars, seat_chars) = text.split_at(7);
        for (i, c) in row_chars.chars().enumerate() {
            match c {
                'F' => row[i] = RowDirection::Front,
                'B' => row[i] = RowDirection::Back,
                _ => return None,
            };
        }
        for (i, c) in seat_chars.chars().enumerate() {
            match c {
                'L' => seat[i] = SeatDirection::Left,
                'R' => seat[i] = SeatDirection::Right,
                _ => return None,
            };
        }

        Some(Self { row, seat })
    }

    fn row_number(&self) -> u8 {
        bisect(&self.row[..])
    }

    fn seat_number(&self) -> u8 {
        bisect(&self.seat[..])
    }

    fn seat_id(&self) -> u16 {
        /* bisect could do this if I hadn't made RowDirection and SeatDirection
         *   different types...
         */
        (self.row_number() as u16) << 3 | self.seat_number() as u16
    }
}

fn bisect<T: Bit>(bits: &[T]) -> u8 {
    let mut i = bits.len();
    let mut res = 0u8;
    for b in bits {
        i -= 1;
        res |= b.as_bit() << i;
    }
    res
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut seats = Vec::new();
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let seat_pos = SeatPos::from(line?.as_str())
          .ok_or("Invalid seat descriptor.")?;
        seats.push(seat_pos);
    }

    let max_id_seat = seats.iter().max_by_key(|s| s.seat_id())
      .ok_or("No seat descriptors provided.")?;
    println!("row {}, seat {}, id {}",
      max_id_seat.row_number(), max_id_seat.seat_number(),
      max_id_seat.seat_id());

    Ok(())
}
