use std::env;
use std::io;

mod sif;
use sif::SIFImage;

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

    let img = match SIFImage::read(width, height, io::stdin()) {
        Ok(img) => img,
        Err(e) => {
            eprintln!("error: {:?}", e);
            return;
        }
    };

    #[derive(Debug, Copy, Clone)]
    struct DigitCount {
        zero: usize,
        one: usize,
        two: usize,
    }

    let mut counts = Vec::new();

    for layer in img.layers() {
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
