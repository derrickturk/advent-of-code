use std::cmp::Ordering;

use crate::binary_minheap::BinaryMinHeap;

struct WithPriority<P, V>(P, V);

impl<P, V> PartialEq for WithPriority<P, V> where P: PartialEq {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<P, V> PartialOrd for WithPriority<P, V> where P: PartialOrd {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

pub struct PriorityQueue<P, V>(BinaryMinHeap<WithPriority<P, V>>);

impl<P, V> PriorityQueue<P, V> where P: PartialOrd {
    pub fn new() -> Self {
        PriorityQueue(BinaryMinHeap::new())
    }

    pub fn insert(&mut self, cost: P, value: V) {
        self.0.insert(WithPriority(cost, value));
    }

    pub fn minimum(&self) -> Option<(&P, &V)> {
        self.0.minimum().map(|WithPriority(cost, value)| (cost, value))
    }

    pub fn pop_minimum(&mut self) -> Option<(P, V)> {
        self.0.pop_minimum().map(|WithPriority(cost, value)| (cost, value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_minimum() {
        let mut q = PriorityQueue::new();
        assert_eq!(q.minimum(), None);
        q.insert(3, "bobby");
        q.insert(2, "sally");
        assert_eq!(q.minimum(), Some((&2, &"sally")));
    }

    #[test]
    fn pop_in_order() {
        let vals = [
            (7, "sally"),
            (3, "bobby"),
            (4, "joe"),
            (2, "kathy"),
            (8, "alice"),
        ];

        let mut q = PriorityQueue::new();
        for (cost, value) in vals {
            q.insert(cost, value);
        }

        assert_eq!(q.pop_minimum(), Some((2, "kathy")));
        assert_eq!(q.pop_minimum(), Some((3, "bobby")));
        assert_eq!(q.pop_minimum(), Some((4, "joe")));
        assert_eq!(q.pop_minimum(), Some((7, "sally")));
        assert_eq!(q.pop_minimum(), Some((8, "alice")));
        assert_eq!(q.pop_minimum(), None);
    }
}
