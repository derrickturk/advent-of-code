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

    fn upheap(&mut self, mut i: usize) {
        loop {
            if i == 0 { break; }
            let parent = (i - 1) / 2;
            if self.0[i] >= self.0[parent] { break; }
            self.0.swap(i, parent);
            i = parent;
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
}
