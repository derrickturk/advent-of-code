pub struct BinaryMinHeap<T>(Vec<T>);

impl<T> BinaryMinHeap<T> where T: PartialOrd {
    pub fn new() -> Self {
        BinaryMinHeap(Vec::new())
    }

    pub fn insert(&mut self, val: T) {
        self.0.push(val);
        self.upheap(self.0.len() - 1);
    }

    pub fn minimum(&self) -> Option<&T> {
        if !self.0.is_empty() {
            Some(&self.0[0])
        } else {
            None
        }
    }

    pub fn pop_minimum(&mut self) -> Option<T> {
        if self.0.is_empty() { return None; }
        let last = self.0.len() - 1;
        self.0.swap(0, last);
        let min = self.0.pop().unwrap(); // must be Some; we returned if empty
        self.downheap(0);
        Some(min)
    }

    fn upheap(&mut self, mut i: usize) {
        loop {
            if i == 0 { break; }
            let parent = (i - 1) / 2;
            if self.0[i] >= self.0[parent] { break; }
            self.0.swap(i, parent);
            i = parent;
        }
    }

    fn downheap(&mut self, mut i: usize) {
        let mut smaller = i;
        loop {
            let lefty = i * 2 + 1;
            let righty = i * 2 + 2;

            if lefty < self.0.len() && self.0[lefty] < self.0[smaller] {
                smaller = lefty;
            }

            if righty < self.0.len() && self.0[righty] < self.0[smaller] {
                smaller = righty;
            }

            if smaller == i { break; }
            self.0.swap(i, smaller);
            i = smaller;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_minimum() {
        let mut h = BinaryMinHeap::<i32>::new();
        assert_eq!(h.minimum(), None);
        h.insert(2);
        h.insert(3);
        h.insert(1);
        h.insert(4);
        assert_eq!(h.minimum(), Some(&1));
    }

    #[test]
    fn pop_in_order() {
        let vals: [i32; 9] = [3, 7, 1, 9, 5, 2, 4, 2, 8];
        let mut h = BinaryMinHeap::new();
        for i in vals {
            h.insert(i);
        }

        assert_eq!(h.pop_minimum(), Some(1));
        assert_eq!(h.pop_minimum(), Some(2));
        assert_eq!(h.pop_minimum(), Some(2));
        assert_eq!(h.pop_minimum(), Some(3));
        assert_eq!(h.pop_minimum(), Some(4));
        assert_eq!(h.pop_minimum(), Some(5));
        assert_eq!(h.pop_minimum(), Some(7));
        assert_eq!(h.pop_minimum(), Some(8));
        assert_eq!(h.pop_minimum(), Some(9));
        assert_eq!(h.pop_minimum(), None);
        assert_eq!(h.pop_minimum(), None);
    }
}
