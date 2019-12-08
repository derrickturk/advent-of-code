use std::io::{self, Read};

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
    pub fn read(width: usize, height: usize, mut src: impl Read
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

    #[inline]
    pub fn layers(&self) -> impl Iterator<Item = &[u8]> {
        self.buf.chunks(self.width * self.height)
    }
}
