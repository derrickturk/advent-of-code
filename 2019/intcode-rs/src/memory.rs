use std::{
    borrow::Cow,
    collections::hash_map::HashMap,
    fmt::Debug,
};

pub trait ExpandoMemory: Debug {
    fn from(memory: Vec<i64>) -> Self;
    fn get(&mut self, index: usize) -> &i64;
    fn get_mut(&mut self, index: usize) -> &mut i64;
    fn image(&self) -> Cow<[i64]>;
}

#[derive(Debug, Clone)]
pub struct ExpandoVec(pub Vec<i64>);

impl ExpandoMemory for ExpandoVec {
    #[inline]
    fn from(vec: Vec<i64>) -> Self {
        Self(vec)
    }

    #[inline]
    fn get(&mut self, index: usize) -> &i64 {
        if index >= self.0.len() {
            self.0.resize(index + 1, 0);
        }
        unsafe { self.0.get_unchecked(index) }
    }

    #[inline]
    fn get_mut(&mut self, index: usize) -> &mut i64 {
        if index >= self.0.len() {
            self.0.resize(index + 1, 0);
        }
        unsafe { self.0.get_unchecked_mut(index) }
    }

    #[inline]
    fn image(&self) -> Cow<[i64]> {
        Cow::from(&self.0[..])
    }
}

#[derive(Debug, Clone)]
pub struct ExpandoSparse {
    base: Vec<i64>,
    extended: HashMap<usize, i64>,
}

impl ExpandoMemory for ExpandoSparse {
    #[inline]
    fn from(vec: Vec<i64>) -> Self {
        Self {
            base: vec,
            extended: HashMap::new(),
        }
    }

    #[inline]
    fn get(&mut self, index: usize) -> &i64 {
        if let Some(x) = self.base.get(index) {
            x
        } else {
            self.extended.entry(index).or_insert(0)
        }
    }

    #[inline]
    fn get_mut(&mut self, index: usize) -> &mut i64 {
        if let Some(x) = self.base.get_mut(index) {
            x
        } else {
            self.extended.entry(index).or_insert(0)
        }
    }

    #[inline]
    fn image(&self) -> Cow<[i64]> {
        if self.extended.is_empty() {
            Cow::from(&self.base[..])
        } else {
            let mut buf = self.base.clone();
            let mut ext: Vec<_> = self.extended.iter()
                .map(|(addr, val)| (*addr, *val)).collect();
            ext.sort_by(|a, b| a.0.cmp(&b.0));
            let (max_addr, _) = ext.last().unwrap();
            buf.resize(max_addr + 1, 0);
            for (addr, val) in ext.iter() {
                buf[*addr] = *val;
            }
            Cow::from(buf)
        }
    }
}

#[derive(Debug, Clone)]
pub struct ProgramState<T: ExpandoMemory> {
    pub memory: T,
    pub ip: usize,
    pub relative_base: i64,
}

impl<T: ExpandoMemory> ProgramState<T> {
    #[inline]
    pub fn from(memory: Vec<i64>) -> Self {
        ProgramState { memory: T::from(memory), ip: 0, relative_base: 0, }
    }

    #[inline]
    pub fn memory(&self) -> &T {
        &self.memory
    }

    #[inline]
    pub fn ip(&self) -> usize {
        self.ip
    }

    #[inline]
    pub fn relative_base(&self) -> i64 {
        self.relative_base
    }
}
