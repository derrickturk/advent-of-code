use std::{
    io,
    collections::hash_map::HashMap,
};

use futures::{
    executor::{block_on, ThreadPool},
    channel::mpsc,
    stream::{StreamExt, empty},
    task::{SpawnExt},
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
}

impl WorldState {
    fn new() -> Self {
        Self { tiles: HashMap::new() }
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

async fn problem1(pool: &ThreadPool, program: Vec<i64>
      ) -> Result<(), IntCodeError> {
    let mut state = ProgramState::<ExpandoVec>::from(program);
    // let (mut input_send, mut input_recv) = mpsc::channel(IO_BUF_SZ);
    let (mut output_send, mut output_recv) = mpsc::channel(IO_BUF_SZ);

    let mut world = WorldState::new();

    let exec_handle = pool.spawn_with_handle(async move {
        let res = execute(&mut state, &mut empty(), &mut output_send).await;
        drop(output_send);
        res
    }).expect("failed to spawn task on ThreadPool");

    loop {
        /*
        if let Err(_) = input_send.send(msg).await {
            break;
        }
        */

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
            let tile = match msg {
                0 => Tile::Empty,
                1 => Tile::Wall,
                2 => Tile::Block,
                3 => Tile::Paddle,
                4 => Tile::Ball,
                _ => panic!("unexpected tile message from arcade: {}", msg),
            };
            world.set_tile((x, y), tile);
        } else {
            panic!("no tile message after coord messages");
        }
    }

    // drop(input_send);
    exec_handle.await?;

    print_map(&world);

    println!("drew {} blocks",
        world.tiles.values().filter(|&&t| t == Tile::Block).count());

    Ok(())
}

fn print_map(world: &WorldState) {
    let ((xmin, ymin), (xmax, ymax)) = world.bounds();

    for y in ymin..=ymax {
        for x in xmin..=xmax {
            let c = match world.tile((x, y)) {
                Tile::Empty => ' ',
                Tile::Wall => '\u{2588}',
                Tile::Block => '#',
                Tile::Paddle => 'Q',
                Tile::Ball => 'o',
            };
            print!("{}", c);
        }
        println!();
    }
}

fn main() -> Result<(), IntCodeError> {
    let mut program = String::new();
    io::stdin().read_line(&mut program)
        .expect("failed to read program from stdin");
    let pool = ThreadPool::new().expect("failed to launch ThreadPool");
    let program: Vec<i64> = program.trim_end().split(',')
        .map(|word| word.parse().map_err(|_| IntCodeError::ParseError))
        .collect::<Result<_, _>>()?;
    block_on(problem1(&pool, program))
}
