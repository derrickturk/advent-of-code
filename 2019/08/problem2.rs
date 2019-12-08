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

    let img = match SIFImage::read(width, height, &mut io::stdin()) {
        Ok(img) => img,
        Err(e) => {
            eprintln!("error: {:?}", e);
            return;
        }
    };

    match img.write_netpbm(&mut io::stdout()) {
        Ok(_) => { },
        Err(_) => eprintln!("error writing NetPBM"),
    };
}
