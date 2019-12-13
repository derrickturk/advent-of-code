use std::{
    io::{self, BufWriter, Write},
    collections::hash_map::HashMap,
    thread,
    time::Duration,
};

use futures::{
    executor::{block_on, ThreadPool},
    channel::mpsc,
    stream::{StreamExt},
    sink::{SinkExt},
    task::{SpawnExt},
};

use winapi::{
    um::winuser::{GetAsyncKeyState, VK_LEFT, VK_RIGHT},
};

use intcode::*;

const IO_BUF_SZ: usize = 512;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Tile {
    Empty,
    Wall,
    Block,
    Paddle,
    Ball,
}

struct WorldState {
    tiles: HashMap<(i64, i64), Tile>,
    segment: i64,
}

impl WorldState {
    fn new() -> Self {
        Self { tiles: HashMap::new(), segment: 0, }
    }

    fn tile(&self, pos: (i64, i64)) -> Tile {
        *self.tiles.get(&pos).unwrap_or(&Tile::Empty)
    }

    fn set_tile(&mut self, pos: (i64, i64), tile: Tile) {
        self.tiles.insert(pos, tile);
    }

    fn bounds(&self) -> ((i64, i64), (i64, i64)) {
        let mut x_min = std::i64::MAX;
        let mut x_max = std::i64::MIN;
        let mut y_min = std::i64::MAX;
        let mut y_max = std::i64::MIN;

        if self.tiles.is_empty() {
            return ((0, 0), (0, 0));
        }

        for &(x, y) in self.tiles.keys() {
            if x < x_min {
                x_min = x;
            }

            if x > x_max {
                x_max = x;
            }

            if y < y_min {
                y_min = y;
            }

            if y > y_max {
                y_max = y;
            }
        }

        ((x_min, y_min), (x_max, y_max))
    }
}

async fn problem2(pool: &ThreadPool, program: Vec<i64>
      ) -> Result<(), IntCodeError> {
    let mut state = ProgramState::<ExpandoVec>::from(program);
    let (mut input_send, mut input_recv) = mpsc::channel(IO_BUF_SZ);
    let (mut output_send, mut output_recv) = mpsc::channel(IO_BUF_SZ);

    let mut world = WorldState::new();

    let exec_handle = pool.spawn_with_handle(async move {
        let res = execute(&mut state, &mut input_recv, &mut output_send).await;
        drop(output_send);
        res
    }).expect("failed to spawn task on ThreadPool");

    let joy_handle = pool.spawn_with_handle(async move {
        loop {
            let msg = unsafe {
                if GetAsyncKeyState(VK_LEFT) as u16 & 0x8001 != 0 {
                    -1
                } else if GetAsyncKeyState(VK_RIGHT) as u16 & 0x8001 != 0 {
                    1
                } else {
                    0
                }
            };
            if let Err(_) = input_send.send(msg).await { break; }
            thread::sleep(Duration::from_millis(1500));
        }
    }).expect("failed to spawn task on ThreadPool");

    let mut rows = 0usize;
    let stdout = io::stdout();
    let mut stdout = BufWriter::new(stdout.lock());
    loop {
        let x;
        if let Some(msg) = output_recv.next().await {
            x = msg;
        } else {
            break;
        }

        let y;
        if let Some(msg) = output_recv.next().await {
            y = msg;
        } else {
            panic!("no y message after x message");
        }

        if let Some(msg) = output_recv.next().await {
            if (x, y) == (-1, 0) {
                world.segment = msg;
            } else {
                let tile = match msg {
                    0 => Tile::Empty,
                    1 => Tile::Wall,
                    2 => Tile::Block,
                    3 => Tile::Paddle,
                    4 => Tile::Ball,
                    _ => panic!("unexpected tile message from arcade: {}", msg),
                };
                world.set_tile((x, y), tile);
            }
        } else {
            panic!("no tile message after coord messages");
        }

        reset_cursor(&mut io::stdout(), rows).unwrap();
        rows = 1;
        rows += print_map(&mut stdout, &world).unwrap();
        writeln!(&mut stdout, "Score: {}", world.segment).unwrap();
        stdout.flush().unwrap();
    }

    exec_handle.await?;

    Ok(())
}

fn print_map(w: &mut impl Write, world: &WorldState) -> io::Result<usize> {
    let ((xmin, ymin), (xmax, ymax)) = world.bounds();
    let mut rows = 0;
    let mut buf = Vec::new();
    for y in ymin..=ymax {
        for x in xmin..=xmax {
            let c = match world.tile((x, y)) {
                Tile::Empty => b' ',
                Tile::Wall => b'|',
                Tile::Block => b'#',
                Tile::Paddle => b'Q',
                Tile::Ball => b'o',
            };
            buf.push(c)
        }
        buf.push(b'\n');
        rows += 1;
    }
    w.write_all(&buf[..])?;
    Ok(rows)
}

fn reset_cursor(w: &mut impl Write, rows: usize) -> io::Result<()> {
    let up_rows = rows.to_string();
    let mut code = Vec::new();
    code.push(27u8);
    code.push(b'[');
    code.extend(up_rows.as_bytes().iter());
    code.push(b'A');
    w.write_all(&code)?;
    Ok(())
}

fn main() -> Result<(), IntCodeError> {
    let mut program = String::new();
    io::stdin().read_line(&mut program)
        .expect("failed to read program from stdin");
    let pool = ThreadPool::new().expect("failed to launch ThreadPool");
    let program: Vec<i64> = program.trim_end().split(',')
        .map(|word| word.parse().map_err(|_| IntCodeError::ParseError))
        .collect::<Result<_, _>>()?;
    block_on(problem2(&pool, program))
}
