use std::{
    cmp::Ordering,
    ops::{Add, Sub, AddAssign, SubAssign},
};

#[derive(Debug, Copy, Clone)]
struct Vec3 {
    x: i64,
    y: i64,
    z: i64,
}

impl Vec3 {
    fn compare(self, other: Self) -> Self {
        Self {
            x: ordering_to_i64(self.x.cmp(&other.x)),
            y: ordering_to_i64(self.y.cmp(&other.y)),
            z: ordering_to_i64(self.z.cmp(&other.z)),
        }
    }
}

impl Add for Vec3 {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.y + other.z,
        }
    }
}

impl AddAssign for Vec3 {
    fn add_assign(&mut self, other: Self) {
        self.x += other.x;
        self.y += other.y;
        self.z += other.z;
    }
}

impl Sub for Vec3 {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            x: self.x - other.x,
            y: self.y - other.y,
            z: self.y - other.z,
        }
    }
}

impl SubAssign for Vec3 {
    fn sub_assign(&mut self, other: Self) {
        self.x -= other.x;
        self.y -= other.y;
        self.z -= other.z;
    }
}

#[derive(Debug, Copy, Clone)]
struct Moon {
    position: Vec3,
    velocity: Vec3,
}

impl Moon {
    fn at(x: i64, y: i64, z: i64) -> Self {
        Self {
            position: Vec3 { x, y, z, },
            velocity: Vec3 { x: 0, y: 0, z: 0, },
        }
    }

    // only apply to self! (loop is easier)
    #[inline]
    fn apply_gravity(&mut self, moon2: Moon) {
        let cmp = self.position.compare(moon2.position);
        self.velocity -= cmp;
    }

    #[inline]
    fn step(&mut self) {
        self.position += self.velocity;
    }

    #[inline]
    fn potential(&self) -> i64 {
        self.position.x.abs() + self.position.y.abs() + self.position.z.abs()
    }

    #[inline]
    fn kinetic(&self) -> i64 {
        self.velocity.x.abs() + self.velocity.y.abs() + self.velocity.z.abs()
    }

    #[inline]
    fn total(&self) -> i64 {
        self.potential() * self.kinetic()
    }
}

#[inline]
fn ordering_to_i64(ord: Ordering) -> i64 {
    match ord {
        Ordering::Less => -1,
        Ordering::Equal => 0,
        Ordering::Greater => 1,
    }
}

fn main() {
    let mut moons = [
        Moon::at(-7, 17, -11),
        Moon::at(9, 12, 5),
        Moon::at(-9, 0, -4),
        Moon::at(4, 6, 0),
    ];

    for _ in 0..1000 {
        for i in 0..moons.len() {
            for j in 0..moons.len() {
                moons[i].apply_gravity(moons[j]);
            }
        }

        for m in &mut moons {
            m.step();
        }
    }

    let mut total = 0;
    for m in &moons {
        total += m.total();
    }
    println!("total energy: {}", total);
}
