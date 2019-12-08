use std::env;
use std::io::{self, Read};

fn main() {
    let args: Vec<_> = env::args().collect();

    let (width, height) = match &args[..] {
        [_, width, height] => {
            let w_parsed = width.parse::<usize>();
            let h_parsed = height.parse::<usize>();
            match (w_parsed, h_parsed) {
                (Ok(width), Ok(height)) => (width, height),
                _ => {
                    eprintln!("Invalid dimensions: {} x {}", width, height);
                    return;
                },
            }
        },

        _ => {
            eprintln!("Usage: {} width height <input.txt",
                args.first().map(|s| s.as_str()).unwrap_or("sif"));
            return;
        }
    };

    let mut buf = Vec::new();
    let mut stdin = io::stdin();
    if let Ok(mut bytes) = stdin.read_to_end(&mut buf) {
        while buf.last().map(u8::is_ascii_whitespace).unwrap_or(false) {
            buf.pop();
            bytes -= 1;
        }

        if bytes % (width * height) != 0 {
            eprintln!("Invalid image size ({} bytes) for dimensions {} x {}",
              bytes, width, height);
            return;
        }
    } else {
        eprintln!("Error reading from stdin.");
        return;
    }

    #[derive(Debug, Copy, Clone)]
    struct DigitCount {
        zero: usize,
        one: usize,
        two: usize,
    }

    let mut counts = Vec::new();

    for layer in buf.chunks(width * height) {
        let mut count = DigitCount { zero: 0, one: 0, two: 0 };
        for digit in layer {
            match digit {
                b'0' => count.zero += 1,
                b'1' => count.one += 1,
                b'2' => count.two += 1,
                _ => {},
            };
        }
        counts.push(count);
    }

    if let Some(c) = counts.iter().min_by_key(|c| c.zero) {
        println!("{} x {} = {}", c.one, c.two, c.one * c.two);
    } else {
        eprintln!("Empty buffer.");
    }
}
