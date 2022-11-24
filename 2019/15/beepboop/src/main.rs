use std::{
    cmp::Ordering,
    io,
    hash::{Hash, Hasher},
};

use intcode::{*, blocking::*};
use aocrs::dijkstra::cost_to_win;

#[derive(Copy, Clone, Debug)]
enum Dir { N, S, W, E }

impl Dir {
    fn step(&self, x: i32, y: i32) -> (i32, i32) {
        match *self {
            Dir::N => (x, y + 1),
            Dir::S => (x, y - 1),
            Dir::W => (x - 1, y),
            Dir::E => (x + 1, y),
        }
    }

    fn command(&self) -> i64 {
        match *self {
            Dir::N => 1,
            Dir::S => 2,
            Dir::W => 3,
            Dir::E => 4,
        }
    }
}

#[derive(Clone, Debug)]
struct State {
    x: i32,
    y: i32,
    oxygen: bool,
    vm: Unblocked<ExpandoVec>,
}

impl State {
    fn from(program: Vec<i64>) -> Self {
        State {
            x: 0,
            y: 0,
            oxygen: false,
            vm: Unblocked(ProgramState::from(program)),
        }
    }

    fn try_move(&self, dir: &Dir) -> Option<Self> {
        let vm = self.vm.clone();
        match io_pair(vm, dir.command()) {
            Some((0, _)) => None,
            Some((code, new_vm)) => {
                let (x, y) = dir.step(self.x, self.y);
                Some(State {
                    x,
                    y,
                    oxygen: code == 2,
                    vm: new_vm,
                })
            },
            None => None,
        }
    }

    fn moves(&self) -> impl Iterator<Item=(usize, State)> {
        let mut res = Vec::new();
        for dir in [Dir::N, Dir::S, Dir::W, Dir::E] {
            if let Some(s) = self.try_move(&dir) {
                res.push(s);
            }
        }
        res.into_iter().map(|s| (1, s))
    }
}

impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        (self.x, self.y).eq(&(other.x, other.y))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        (self.x, self.y).partial_cmp(&(other.x, other.y))
    }
}

impl Eq for State { }

impl Hash for State {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.x, self.y).hash(state);
    }
}

fn io_pair<T: ExpandoMemory>(mut state: Unblocked<T>, input: i64
  ) -> Option<(i64, Unblocked<T>)> {
    loop {
        match state.step().ok()? {
            BlockingProgramState::Unblocked(new_state) => {
                state = new_state;
            },

            BlockingProgramState::WaitInput(w) => {
                state = w.step(input).ok()?;
                break;
            },

            BlockingProgramState::WaitOutput(_) => {
                return None;
            },

            BlockingProgramState::Halted(_) => {
                return None;
            },
        }
    }

    loop {
        match state.step().ok()? {
            BlockingProgramState::Unblocked(new_state) => {
                state = new_state;
            },

            BlockingProgramState::WaitInput(_) => {
                return None;
            },

            BlockingProgramState::WaitOutput(o) => {
                return Some((o.output(), o.step()));
            },

            BlockingProgramState::Halted(_) => {
                return None;
            },
        }
    }
}

fn main() -> Result<(), IntCodeError> {
    let mut program = String::new();
    io::stdin().read_line(&mut program)
        .expect("failed to read program from stdin");
    let program: Vec<i64> = program.trim_end().split(',')
        .map(|word| word.parse().map_err(|_| IntCodeError::ParseError))
        .collect::<Result<_, _>>()?;
    let initial = State::from(program);
    if let Some(moves) = cost_to_win(initial, |s| s.moves(), |s| s.oxygen) {
        println!("part 1 in {} moves", moves);
    } else {
        eprintln!("part 1 fail");
    }
    Ok(())
}
