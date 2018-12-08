use std::io::{self, Read};

#[derive(Debug)]
struct Node {
    children: Vec<Node>,
    metadata: Vec<i32>,
}

fn parse_node(xs: &[i32]) -> Option<(Node, &[i32])> {
    let (n_children, xs) = xs.split_first()?;
    let (n_meta, xs) = xs.split_first()?;
    let mut children = Vec::new();
    let mut xs = xs;
    for _ in 0..*n_children {
        let (child, xs_next) = parse_node(xs)?;
        children.push(child);
        xs = xs_next;
    }
    let (meta, xs) = xs.split_at(*n_meta as usize);
    Some((Node { children, metadata: meta.to_owned() }, xs))
}

fn sum(n: &Node) -> i32 {
    let m_sum: i32 = n.metadata.iter().sum();
    let c_sum: i32 = n.children.iter().map(|c| sum(c)).sum();
    m_sum + c_sum
}

fn value(n: &Node) -> i32 {
    if n.children.is_empty() {
        n.metadata.iter().sum()
    } else {
        n.metadata.iter().map(|i| {
            let i = *i as usize;
            if i == 0 || i > n.children.len() {
                0
            } else {
                value(&n.children[i - 1])
            }
        }).sum()
    }
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    let node = input.split_whitespace()
        .map(|w| w.parse().ok())
        .collect::<Option<Vec<_>>>()
        .and_then(|xs| parse_node(&xs).map(|(n, _)| n));
    match node {
        None => println!("invalid input"),
        Some(node) => {
            println!("{}", sum(&node));
            println!("{}", value(&node));
        }
    }
    Ok(())
}
