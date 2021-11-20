const SIZES: [usize; 20] = [
    11,
    30,
    47,
    31,
    32,
    36,
    3,
    1,
    5,
    3,
    32,
    36,
    15,
    11,
    46,
    26,
    28,
    1,
    19,
    3,
];

const TARGET: usize = 150;

fn subset_sum_count(sizes: &[usize], target: usize) -> isize {
    let mut dp_table = vec![vec![0; target + 1]; sizes.len() + 1];
    dp_table[0][0] = 1;
    for i in 1..=sizes.len() {
        dp_table[i][0] = 1;
    }
    for i in 1..=sizes.len() {
        for j in 1..=target {
            if sizes[i - 1] > j {
                dp_table[i][j] = dp_table[i - 1][j];
            } else {
                dp_table[i][j] =
                  dp_table[i - 1][j] + dp_table[i - 1][j - sizes[i - 1]];
            }
        }
    }
    dp_table[sizes.len()][target]
}

fn main() {
    dbg!(subset_sum_count(&[20, 15, 10, 5, 5], 25));
    dbg!(subset_sum_count(&SIZES, TARGET));
}
