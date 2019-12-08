use std::env;
use std::io;

mod sif;
use sif::SIFImage;

fn main() {
    let args: Vec<_> = env::args().collect();

    let (width, height, ascii) = match &args[..] {
        [_, width, height, mode] => {
            let w_parsed = width.parse::<usize>();
            let h_parsed = height.parse::<usize>();
            match (w_parsed, h_parsed) {
                (Ok(width), Ok(height)) => (width, height, mode == "ascii"),
                _ => {
                    eprintln!("Invalid dimensions: {} x {}", width, height);
                    return;
                },
            }
        },

        [_, width, height] => {
            let w_parsed = width.parse::<usize>();
            let h_parsed = height.parse::<usize>();
            match (w_parsed, h_parsed) {
                (Ok(width), Ok(height)) => (width, height, false),
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

    let img = match SIFImage::read(width, height, &mut io::stdin()) {
        Ok(img) => img,
        Err(e) => {
            eprintln!("error: {:?}", e);
            return;
        }
    };

    if ascii {
        match img.write_ascii(&mut io::stdout()) {
            Ok(_) => { },
            Err(_) => eprintln!("error writing ASCII"),
        };
    } else {
        match img.write_netpbm(&mut io::stdout()) {
            Ok(_) => { },
            Err(_) => eprintln!("error writing NetPBM"),
        };
    }
}
