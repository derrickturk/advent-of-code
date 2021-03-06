#![feature(const_generics)]

pub struct ValidPasswordIterator<const N: usize> {
    current: [u8; N],
    max: [u8; N],
}

impl<const N: usize> ValidPasswordIterator<{N}> {
    #[inline]
    pub fn new(min: [u8; N], max: [u8; N]) -> Self {
        ValidPasswordIterator { current: min, max }
    }

    #[inline]
    fn incr(&mut self) {
        if N == 0 { return; }

        self.current[N - 1] += 1;
        if self.current[N - 1] > b'9' {
            self.current[N - 1] = b'0';
            for i in (0..N - 1).rev() {
                self.current[i] += 1;
                if self.current[i] <= b'9' {
                    break;
                }
                self.current[i] = b'0';
            }
        }
    }

    #[inline]
    fn has_duplicate(&self) -> bool {
        if N < 2 { return false; }

        let mut this_run = 1;

        for i in 1..N {
            if self.current[i] == self.current[i - 1] {
                this_run += 1;
            } else {
                if this_run == 2 {
                    return true;
                }
                this_run = 1;
            }
        }

        this_run == 2
    }

    #[inline]
    fn valid(&self) -> bool {
        for i in 1..N {
            if self.current[i] < self.current[i - 1] {
                return false;
            }
        }

        if !self.has_duplicate() {
            return false;
        }

        true
    }
}

impl<const N: usize> Iterator for ValidPasswordIterator<{N}> {
    type Item = [u8; N];

    fn next(&mut self) -> Option<[u8; N]> {
        while !self.valid() {
            self.incr();
        }

        if &self.current[..] > &self.max[..] {
            None
        } else {
            let val = self.current.clone();
            self.incr();
            Some(val)
        }
    }
}

fn main() {
    for pwd in ValidPasswordIterator::new(*b"264793", *b"803935") {
        println!("{}", unsafe { std::str::from_utf8_unchecked(&pwd) });
    }
}
