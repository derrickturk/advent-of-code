use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashSet},
    error::Error,
    io::{BufRead, self},
};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
enum Bug { A, B, C, D }
type Room = Vec<Option<Bug>>;
type Rooms = [Room; 4];
type Hallway = [Option<Bug>; 11];
type World = (Rooms, Hallway);

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
struct State {
    estimate: u64,
    cost: u64,
    length: usize,
    world: World,
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
struct TraceState {
    estimate: u64,
    cost: u64,
    length: usize,
    world: World,
    trace: Vec<(u64, u64, World)>,
}

impl Bug {
    fn from_char(c: char) -> Option<Self> {
        match c {
            'A' => Some(Bug::A),
            'B' => Some(Bug::B),
            'C' => Some(Bug::C),
            'D' => Some(Bug::D),
            _ => None,
        }
    }

    fn for_room(i: usize) -> Option<Self> {
        match i {
            0 => Some(Bug::A),
            1 => Some(Bug::B),
            2 => Some(Bug::C),
            3 => Some(Bug::D),
            _ => None,
        }
    }

    fn energy(&self) -> u64 {
        match self {
            Bug::A => 1,
            Bug::B => 10,
            Bug::C => 100,
            Bug::D => 1000,
        }
    }

    fn room(&self) -> usize {
        match self {
            Bug::A => 0,
            Bug::B => 1,
            Bug::C => 2,
            Bug::D => 3,
        }
    }

    fn to_char(&self) -> char {
        match self {
            Bug::A => 'A',
            Bug::B => 'B',
            Bug::C => 'C',
            Bug::D => 'D',
        }
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other.estimate.cmp(&self.estimate)
          .then_with(|| other.cost.cmp(&self.cost))
          .then_with(|| self.world.cmp(&other.world))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TraceState {
    fn cmp(&self, other: &Self) -> Ordering {
        other.estimate.cmp(&self.estimate)
          .then_with(|| other.cost.cmp(&self.cost))
          .then_with(|| self.world.cmp(&other.world))
    }
}

impl PartialOrd for TraceState {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn read_world() -> Result<World, Box<dyn Error>> {
    let stdin = io::stdin();
    let stdin = stdin.lock();

    let mut rooms = [vec![], vec![], vec![], vec![]];
    let mut hallway = [None; 11];

    let mut seen_hall = false;
    for l in stdin.lines() {
        let l = l?;
        if l.chars().all(|c| c == '#' || c == ' ') {
            continue;
        }

        if !seen_hall {
            for (i, c) in l[1..l.len() - 1].chars().enumerate() {
                hallway[i] = Bug::from_char(c);
            }
            seen_hall = true;
        } else {
            for (i, c) in l.chars().filter(|&c| c != '#' && c != ' ').enumerate() {
                rooms[i].push(Bug::from_char(c));
            }
        }
    }

    Ok((rooms, hallway))
}

fn valid_moves(world: &World) -> Vec<(u64, World)> {
    let mut moves = Vec::new();
    room_to_room_moves(world, &mut moves);
    room_to_hall_moves(world, &mut moves);
    hall_to_room_moves(world, &mut moves);
    moves
}

fn room_sources(rooms: &Rooms) -> Vec<(usize, usize, Bug)> {
    let mut result = vec![];
    for (i, r) in rooms.iter().enumerate() {
        'room: for (j, bug) in r.iter().enumerate() {
            if let Some(bug) = bug {
                let mut may_leave = bug.room() != i;
                if !may_leave {
                    for other in r[j + 1..].iter() {
                        if let Some(other) = other {
                            if other.room() != i {
                                may_leave = true;
                                break;
                            }
                        }
                    }
                }
                if may_leave {
                    result.push((i, j, *bug));
                }
                break 'room;
            }
        }
    }
    result
}

fn room_target(bug: Bug, rooms: &Rooms) -> Option<(usize, usize)> {
    let i = bug.room();
    let mut j = None;

    for (k, b) in rooms[i].iter().enumerate() {
        if b.is_none() {
            j = Some(k);
        } else {
            break;
        }
    }

    let j = j?;
    for b in rooms[i][j + 1..].iter() {
        if b.unwrap() != bug {
            return None;
        }
    }

    Some((i, j))
}

fn hall_targets(hall: &Hallway, i: usize) -> Vec<usize> {
    let i = room_to_hall(i);
    let mut result = vec![];
    match hall[i] {
        Some(_) => { },
        None => {
            let mut j = i - 1;
            while j > 0 && j < hall.len() && hall[j].is_none() {
                if !is_foyer(j) {
                    result.push(j);
                }
                j -= 1;
            }
            let mut j = i + 1;
            while j > 0 && j < hall.len() && hall[j].is_none() {
                if !is_foyer(j) {
                    result.push(j);
                }
                j += 1;
            }
        },
    }
    result
}

fn room_to_hall(i: usize) -> usize {
    2 * (i + 1)
}

fn hall_to_room(i: usize) -> Option<usize> {
    match i {
        2 => Some(0),
        4 => Some(1),
        6 => Some(2),
        8 => Some(3),
        _ => None,
    }
}

fn is_foyer(i: usize) -> bool {
    hall_to_room(i).is_some()
}

fn clear_between(i: usize, j: usize, hall: &Hallway) -> bool {
    if i < j {
        for k in i..=j {
            if let Some(_) = hall[k] {
                return false;
            }
        }
    } else {
        for k in j..=i {
            if let Some(_) = hall[k] {
                return false;
            }
        }
    }
    true
}

fn clear_between1(i: usize, j: usize, hall: &Hallway) -> bool {
    if i < j {
        for k in i + 1..=j {
            if let Some(_) = hall[k] {
                return false;
            }
        }
    } else {
        for k in j..=i - 1 {
            if let Some(_) = hall[k] {
                return false;
            }
        }
    }
    true
}

fn diff(i: usize, j: usize) -> usize {
    if i < j {
        j - i
    } else {
        i - j
    }
}

fn room_to_room_moves((rooms, hall): &World, moves: &mut Vec<(u64, World)>) {
    for &(i, j, bug) in room_sources(rooms).iter() {
        if let Some((new_i, new_j)) = room_target(bug, rooms) {
            if i != new_i &&
              clear_between(room_to_hall(i), room_to_hall(new_i), hall) {
                let steps = 2 + j + new_j
                  + diff(room_to_hall(new_i), room_to_hall(i));
                let mut new_rooms = rooms.clone();
                new_rooms[i][j] = None;
                new_rooms[new_i][new_j] = Some(bug);
                moves.push((bug.energy() * steps as u64, (new_rooms, hall.clone())));
            }
        }
    }
}

fn room_to_hall_moves((rooms, hall): &World, moves: &mut Vec<(u64, World)>) {
    for &(i, j, bug) in room_sources(rooms).iter() {
        for &k in hall_targets(hall, i).iter() {
            let steps = 1 + j + diff(room_to_hall(i), k);
            let mut new_rooms = rooms.clone();
            let mut new_hall = hall.clone();
            new_rooms[i][j] = None;
            new_hall[k] = Some(bug);
            moves.push((bug.energy() * steps as u64, (new_rooms, new_hall)));
        }
    }
}

fn hall_to_room_moves((rooms, hall): &World, moves: &mut Vec<(u64, World)>) {
    for (k, bug) in hall.iter().enumerate() {
        if let &Some(bug) = bug {
            if let Some((i, j)) = room_target(bug, rooms) {
                if !clear_between1(k, room_to_hall(i), hall) {
                    continue;
                }
                let steps = 1 + j + diff(room_to_hall(i), k);
                let mut new_rooms = rooms.clone();
                let mut new_hall = hall.clone();
                new_rooms[i][j] = Some(bug);
                new_hall[k] = None;
                moves.push((bug.energy() * steps as u64, (new_rooms, new_hall)));
            }
        }
    }
}

fn won((rooms, _): &World) -> bool {
    rooms.iter().enumerate().all(
      |(i, r)| r.iter().all(|&b| b == Bug::for_room(i)))
}

fn heuristic((rooms, hall): &World) -> u64 {
    let room_cost: u64 = rooms.iter().enumerate().flat_map(|(i, r)| {
        r.iter().enumerate().map(move |(j, b)| {
            match b {
                Some(bug) if bug.room() != i => {
                    let steps =
                      2 + j + diff(room_to_hall(i), room_to_hall(bug.room()));
                    bug.energy() * steps as u64
                },
                _ => 0,
            }
        })
    }).sum();

    let hall_cost: u64 = hall.iter().enumerate().map(|(i, b)| {
        match b {
            Some(bug) => {
                let steps = 1 + diff(i, room_to_hall(bug.room()));
                bug.energy() * steps as u64
            },
            _ => 0,
        }
    }).sum();

    room_cost + hall_cost
}

fn solve_cost(world: &World) -> Option<u64> {
    let mut q = BinaryHeap::new();
    q.push(State {
        estimate: 0,
        cost: 0,
        length: 0,
        world: world.clone(),
    });

    let mut seen = HashSet::new();
    while let Some(s) = q.pop() {
        if won(&s.world) {
            dbg!(s.length);
            return Some(s.cost)
        }

        seen.insert(s.world.clone());

        let next = valid_moves(&s.world);
        for (cost, new_w) in next {
            if seen.contains(&new_w) {
                continue;
            }

            q.push(State {
                estimate: cost + s.cost + heuristic(&new_w),
                cost: cost + s.cost,
                length: s.length + 1,
                world: new_w,
            });
        }
    }

    None
}

fn solve_steps(world: &World) -> Option<Vec<(u64, u64, World)>> {
    let mut q = BinaryHeap::new();
    q.push(TraceState {
        estimate: 0,
        cost: 0,
        length: 0,
        world: world.clone(),
        trace: vec![],
    });

    let mut seen = HashSet::new();
    while let Some(s) = q.pop() {
        if won(&s.world) {
            dbg!(s.length);
            return Some(s.trace)
        }

        seen.insert(s.world.clone());

        let next = valid_moves(&s.world);
        for (cost, new_w) in next {
            if seen.contains(&new_w) {
                continue;
            }

            let mut trace = s.trace.clone();
            let estimate = cost + s.cost + heuristic(&new_w);
            let cost = cost + s.cost;
            trace.push((estimate, cost, new_w.clone()));

            q.push(TraceState {
                estimate,
                cost,
                length: s.length + 1,
                world: new_w,
                trace,
            });
        }
    }

    None
}

fn print((rooms, hall): &World) {
    println!("#############");
    print!("#");
    for b in hall {
        match b {
            Some(b) => print!("{}", b.to_char()),
            None => print!("."),
        };
    }
    println!("#");

    for i in 0..rooms[0].len() {
        if i == 0 {
            print!("###");
        } else {
            print!("  #");
        }

        for j in 0..4 {
            match rooms[j][i] {
                Some(b) => print!("{}", b.to_char()),
                None => print!("."),
            };
            print!("#");
        }

        if i == 0 {
            println!("##");
        } else {
            println!("  ");
        }
    }

    println!("  #########  ");
    println!();
}

fn main() -> Result<(), Box<dyn Error>> {
    let world = read_world()?;

    println!("START");
    print(&world);

    println!("TRACK");
    dbg!(solve_cost(&world));
    if let Some(trace) = solve_steps(&world) {
        for (h, c, w) in trace {
            println!("{} (est {})", c, h);
            print(&w);
        }
    }

    println!("AVAILABLE");
    for (c, w) in valid_moves(&world) {
        println!("{} (est {})", c, c + heuristic(&w));
        print(&w);
    }

    Ok(())
}
