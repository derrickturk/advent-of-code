mod maze;
use maze::*;

fn main() {
    let mut ks = KeySet::new();
    ks.insert('a');
    ks.insert('c');
    ks.insert('z');
    println!("{}", ks);
    dbg!(ks.contains('C'));
}
