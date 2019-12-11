use std::{
    collections::hash_map::HashMap,
    fmt::Debug,
};

pub trait ExpandoMemory: Debug {
    fn from(memory: Vec<i64>) -> Self;
    fn get(&mut self, index: usize) -> &i64;
    fn get_mut(&mut self, index: usize) -> &mut i64;
}

#[derive(Debug, Clone)]
pub struct ExpandoVec(pub Vec<i64>);

impl ExpandoMemory for ExpandoVec {
    fn from(vec: Vec<i64>) -> Self {
        Self(vec)
    }

    fn get(&mut self, index: usize) -> &i64 {
        if index >= self.0.len() {
            self.0.resize(index + 1, 0);
        }
        unsafe { self.0.get_unchecked(index) }
    }

    fn get_mut(&mut self, index: usize) -> &mut i64 {
        if index >= self.0.len() {
            self.0.resize(index + 1, 0);
        }
        unsafe { self.0.get_unchecked_mut(index) }
    }
}

#[derive(Debug, Clone)]
pub struct ExpandoSparse {
    base: Vec<i64>,
    extended: HashMap<usize, i64>,
}

impl ExpandoMemory for ExpandoSparse {
    fn from(vec: Vec<i64>) -> Self {
        Self {
            base: vec,
            extended: HashMap::new(),
        }
    }

    fn get(&mut self, index: usize) -> &i64 {
        if let Some(x) = self.base.get(index) {
            x
        } else {
            self.extended.entry(index).or_insert(0)
        }
    }

    fn get_mut(&mut self, index: usize) -> &mut i64 {
        if let Some(x) = self.base.get_mut(index) {
            x
        } else {
            self.extended.entry(index).or_insert(0)
        }
    }
}

#[derive(Debug, Clone)]
pub struct ProgramState<T: ExpandoMemory> {
    pub(super) memory: T,
    pub(super) ip: usize,
    pub(super) relative_base: i64,
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
