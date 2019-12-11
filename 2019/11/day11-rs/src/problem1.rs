use std::{
    io,
    collections::hash_map::HashMap,
};

use futures::{
    executor::{block_on, ThreadPool},
    channel::mpsc,
    stream::StreamExt,
    sink::SinkExt,
    task::{SpawnExt},
};

use intcode::*;

const IO_BUF_SZ: usize = 512;

#[derive(Debug, Copy, Clone)]
enum Direction { N, S, E, W }

#[derive(Debug, Copy, Clone)]
struct RobotState {
    pos: (i32, i32),
    facing: Direction,
}

impl RobotState {
    fn new() -> Self {
        Self { pos: (0, 0), facing: Direction::N, }
    }

    fn turn_left(&mut self) {
        self.facing = match self.facing {
            Direction::N => Direction::W,
            Direction::S => Direction::E,
            Direction::E => Direction::N,
            Direction::W => Direction::S,
        };
    }

    fn turn_right(&mut self) {
        self.facing = match self.facing {
            Direction::N => Direction::E,
            Direction::S => Direction::W,
            Direction::E => Direction::S,
            Direction::W => Direction::N,
        };
    }

    fn step(&mut self) {
        match self.facing {
            Direction::N => self.pos.1 += 1,
            Direction::S => self.pos.1 -= 1,
            Direction::E => self.pos.0 += 1,
            Direction::W => self.pos.0 -= 1,
        };
    }
}

#[derive(Debug, Copy, Clone)]
enum Color { Black, White }

struct WorldState {
    colors: HashMap<(i32, i32), Color>,
}

impl WorldState {
    fn new() -> Self {
        Self { colors: HashMap::new() }
    }

    fn color(&self, pos: (i32, i32)) -> Color {
        *self.colors.get(&pos).unwrap_or(&Color::Black)
    }

    fn paint(&mut self, pos: (i32, i32), color: Color) {
        self.colors.insert(pos, color);
    }
}

async fn problem1(pool: &ThreadPool, program: Vec<i64>
      ) -> Result<(), IntCodeError> {
    let mut state = ProgramState::<ExpandoVec>::from(program);
    let (mut input_send, mut input_recv) = mpsc::channel(IO_BUF_SZ);
    let (mut output_send, mut output_recv) = mpsc::channel(IO_BUF_SZ);

    let mut robot = RobotState::new();
    let mut world = WorldState::new();

    let exec_handle = pool.spawn_with_handle(async move {
        let res = execute(&mut state, &mut input_recv, &mut output_send).await;
        drop(output_send);
        res
    }).expect("failed to spawn task on ThreadPool");

    loop {
        let msg = match world.color(robot.pos) {
            Color::Black => 0,
            Color::White => 1,
        };

        if let Err(_) = input_send.send(msg).await {
            break;
        }

        if let Some(msg) = output_recv.next().await {
            let color = match msg {
                0 => Color::Black,
                1 => Color::White,
                _ => panic!("unexpected color message from robot: {}", msg),
            };
            world.paint(robot.pos, color);
        } else {
            break;
        }

        if let Some(msg) = output_recv.next().await {
            if msg == 0 {
                robot.turn_left();
            } else if msg == 1 {
                robot.turn_right();
            } else {
                panic!("unexpected turn message from robot: {}", msg);
            }
            robot.step();
        } else {
            panic!("no turn message after color message");
        }
    }

    drop(input_send);
    exec_handle.await?;

    println!("painted {} panels", world.colors.len());

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
    block_on(problem1(&pool, program))
}
