use std::{
    collections::HashSet,
    hash::Hash,
    ops::Add,
    rc::Rc,
};

use crate::priority_queue::*;

pub fn cost_to_win<C, S, MF, WF, I>(initial: S, moves: MF, win: WF) -> Option<C>
  where C: Add<C, Output=C> + Copy + PartialEq + PartialOrd + Default,
        S: PartialEq + Eq + Hash, // C needs default() == 0
        MF: for<'a> Fn(&'a S) -> I,
        WF: for<'a> Fn(&'a S) -> bool,
        I: Iterator<Item=(C, S)>, {
    let mut q = PriorityQueue::new();
    let mut seen = HashSet::new();
    q.insert(C::default(), initial);
    while let Some((cost, next)) = q.pop_minimum() {
        if win(&next) {
            return Some(cost);
        }

        if seen.contains(&next) {
            continue;
        }

        for (step_cost, step) in moves(&next) {
            q.insert(cost + step_cost, step);
        }

        seen.insert(next);
    }

    None
}

pub fn states_to_win<C, S, MF, WF, I>(initial: S, moves: MF, win: WF
  ) -> Option<(C, Vec<Rc<S>>)>
  where C: Add<C, Output=C> + Copy + PartialEq + PartialOrd + Default,
        S: PartialEq + Eq + Hash + Clone, // C needs default() == 0
        MF: for<'a> Fn(&'a S) -> I,
        WF: for<'a> Fn(&'a S) -> bool,
        I: Iterator<Item=(C, S)>, {
    let mut q = PriorityQueue::new();
    let mut seen = HashSet::new();
    q.insert(C::default(), (initial, vec![]));
    while let Some((cost, (next, path))) = q.pop_minimum() {
        let next = Rc::new(next);

        if win(&next) {
            let mut path = path.clone();
            path.push(next);
            return Some((cost, path));
        }

        if seen.contains(&next) {
            continue;
        }

        let mut path = path.clone();
        path.push(Rc::clone(&next));
        for (step_cost, step) in moves(&next) {
            q.insert(cost + step_cost, (step, path.clone()));
        }

        seen.insert(next);
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mathy_cost() {
        assert_eq!(cost_to_win(1i32, |x| {
            [x * 2, x + 1].into_iter().map(|y| (1, y))
        }, |x| *x == 17), Some(5));
    }


    #[test]
    fn math_path() {
        let res = states_to_win(1i32, |x| {
            [x * 2, x + 1].into_iter().map(|y| (1, y))
        }, |x| *x == 17);

        assert!(res.is_some());

        let (cost, path) = res.unwrap();
        assert_eq!(cost, 5);
        assert_eq!(path.len(), 6);
        assert_eq!(*path[0], 1);
        assert_eq!(*path[1], 2);
        assert_eq!(*path[2], 4);
        assert_eq!(*path[3], 8);
        assert_eq!(*path[4], 16);
        assert_eq!(*path[5], 17);
    }
}
