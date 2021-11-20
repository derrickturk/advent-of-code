use std::{
    fmt::{self, Display, Formatter},
    io::BufRead,
    ops::{Index, IndexMut},
};

#[derive(PartialOrd, Ord, PartialEq, Eq, Copy, Clone, Debug)]
pub enum Cell {
    Off,
    On,
}

#[derive(Clone, Debug)]
pub struct Grid {
    grid: Vec<Vec<Cell>>,
}

impl Grid {
    #[inline]
    pub fn new(rows: usize, cols: usize, initial: Cell) -> Self {
        Self {
            grid: vec![vec![initial; cols]; rows]
        }
    }

    pub fn parse<B: BufRead>(buf: &mut B) -> Option<Self> {
        let mut rows = Vec::new();
        for l in buf.lines() {
            rows.push(parse_line(l.ok()?.as_str())?);
        }
        if rows.len() == 0 {
            Some(Self { grid: rows })
        } else {
            let cols = rows[0].len();
            if rows.iter().all(|r| r.len() == cols) {
                Some(Self { grid: rows })
            } else {
                None
            }
        }
    }

    #[inline]
    pub fn rows(&self) -> usize {
        self.grid.len()
    }

    #[inline]
    pub fn cols(&self) -> usize {
        self.grid.first().map_or(0, |row| row.len())
    }

    #[inline]
    pub fn grid(&self) -> &[Vec<Cell>] {
        &self.grid
    }

    #[inline]
    pub fn grid_ref(&self, i: usize, j: usize) -> Option<GridRef> {
        if i < self.rows() && j < self.cols() {
            Some(unsafe { self.grid_ref_unsafe(i, j) })
        } else {
            None
        }
    }

    #[inline]
    pub unsafe fn grid_ref_unsafe(&self, i: usize, j: usize) -> GridRef {
        GridRef {
            i,
            j,
            rows: self.rows(),
            cols: self.cols(),
            grid: self,
        }
    }

    #[inline]
    pub fn update<F: Fn(GridRef) -> Cell>(&self, f: F) -> Grid {
        let mut new_grid = self.grid.clone();
        for gr in self.grid_refs() {
            gr.write(f(gr), &mut new_grid);
        }
        Grid {
            grid: new_grid,
        }
    }

    #[inline]
    fn grid_refs(&self) -> impl Iterator<Item=GridRef> {
        (0..self.rows()).flat_map(
          move |i| (0..self.cols()).map(
            move |j| unsafe { self.grid_ref_unsafe(i, j) }))
    }

    #[inline]
    pub fn count_on(&self) -> usize {
        self.grid.iter()
          .map(|row| row.iter().filter(|&&c| c == Cell::On).count())
          .sum()
    }

    #[inline]
    pub fn count_off(&self) -> usize {
        self.grid.iter()
          .map(|row| row.iter().filter(|&&c| c == Cell::Off).count())
          .sum()
    }
}

impl Index<(usize, usize)> for Grid {
    type Output = Cell;

    #[inline]
    fn index(&self, (i, j): (usize, usize)) -> &Self::Output {
        &self.grid[i][j]
    }
}

impl IndexMut<(usize, usize)> for Grid {
    #[inline]
    fn index_mut(&mut self, (i, j): (usize, usize)) -> &mut Self::Output {
        &mut self.grid[i][j]
    }
}

impl Display for Grid {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for row in self.grid.iter() {
            for cell in row {
                let c = match cell {
                    Cell::Off => '.',
                    Cell::On => '#',
                };
                write!(f, "{}", c)?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

#[inline]
fn parse_line(line: &str) -> Option<Vec<Cell>> {
    line.chars().map(|c| {
        match c {
            '.' => Some(Cell::Off),
            '#' => Some(Cell::On),
            _ => None,
        }
    }).collect()
}

#[derive(Copy, Clone, Debug)]
pub struct GridRef<'a> {
    i: usize,
    j: usize,
    rows: usize,
    cols: usize,
    grid: &'a Grid,
}

impl<'a> GridRef<'a> {
    #[inline]
    pub fn read(&self) -> Cell {
        self.grid[(self.i, self.j)]
    }

    #[inline]
    pub fn i(&self) -> usize {
        self.i
    }

    #[inline]
    pub fn j(&self) -> usize {
        self.j
    }

    #[inline]
    pub fn is_corner(&self) -> bool {
        (self.i == 0 || self.i == self.rows - 1)
          && (self.j == 0 || self.j == self.cols - 1)
    }

    #[inline]
    pub fn neighbors_diagonal(&self) -> impl Iterator<Item=GridRef> {
        self.neighbors_diagonal_ixes().map(move |(i, j)|
            GridRef {
                i,
                j,
                rows: self.rows,
                cols: self.cols,
                grid: self.grid,
            })
    }

    #[inline]
    pub fn neighbor_values_diagonal(&self) -> impl Iterator<Item=Cell> + '_ {
        self.neighbors_diagonal_ixes().map(move |(i, j)| self.grid[(i, j)])
    }

    #[inline]
    fn write(&self, value: Cell, target: &mut Vec<Vec<Cell>>) {
        target[self.i][self.j] = value;
    }

    #[inline]
    fn neighbors_diagonal_ixes(&self)
      -> impl Iterator<Item=(usize, usize)> + '_ {
        (self.i.saturating_sub(1)..=(self.i + 1).min(self.rows.saturating_sub(1)))
          .flat_map(move |i|
            (self.j.saturating_sub(1)..=(self.j + 1).min(self.cols.saturating_sub(1)))
              .map(move |j| (i, j)))
        .filter(move |&(i, j)| i != self.i || j != self.j)
    }
}
