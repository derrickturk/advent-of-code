use std::io::{self, Read, Write};

#[derive(Debug)]
pub enum SIFError {
    WrongImageSize { bytes: usize, width: usize, height: usize },
    BadPixel { value: u8 },
    IOError { error: io::Error },
}

pub struct SIFImage {
    width: usize,
    height: usize,
    buf: Vec<u8>,
}

impl SIFImage {
    #[inline]
    pub fn read(width: usize, height: usize, src: &mut impl Read
          ) -> Result<Self, SIFError> {
        let mut buf = Vec::new();

        match src.read_to_end(&mut buf) {
            Ok(mut bytes) => {
                while buf.last().map(u8::is_ascii_whitespace).unwrap_or(false) {
                    buf.pop();
                    bytes -= 1;
                }

                for b in &buf {
                    if *b != b'0' && *b != b'1' && *b != b'2' {
                        return Err(SIFError::BadPixel { value: *b });
                    }
                }

                if bytes % (width * height) == 0 {
                    Ok(Self { width, height, buf, })
                } else {
                    Err(SIFError::WrongImageSize { bytes, width, height })
                }
            },

            Err(error) => Err(SIFError::IOError { error }),
        }
    }

    #[inline]
    pub fn buf(&self) -> &[u8] {
        &self.buf[..]
    }

    #[inline]
    pub fn width(&self) -> usize {
        self.width
    }

    #[inline]
    pub fn height(&self) -> usize {
        self.height
    }

    // front to back
    #[inline]
    pub fn layers(&self) -> impl Iterator<Item = &[u8]> {
        self.buf.chunks(self.width * self.height)
    }

    // back to front
    #[inline]
    pub fn layers_rev(&self) -> impl Iterator<Item = &[u8]> {
        self.buf.rchunks(self.width * self.height)
    }

    #[inline]
    pub fn flatten(&self) -> Vec<u8> {
        let mut flat = vec![0u8; self.width * self.height];
        for layer in self.layers_rev() {
            flatten1(&mut flat[..], layer);
        }
        flat
    }

    pub fn write_netpbm(&self, write: &mut impl Write) -> io::Result<()> {
        let flat = self.flatten();
        writeln!(write, "P1")?;
        writeln!(write, "{} {}", self.width, self.height)?;
        for row in flat.chunks(self.width) {
            for (i, pix) in row.iter().enumerate() {
                if i != 0 {
                    write!(write, " ")?;
                }
                write!(write, "{}", if *pix == b'0' { 1 } else { 0 })?;
            }
            writeln!(write)?;
        }
        Ok(())
    }
}

fn flatten1(dst: &mut [u8], src: &[u8]) {
    for (i, pix) in src.iter().enumerate() {
        if *pix != b'2' {
            dst[i] = *pix;
        }
    }
}
