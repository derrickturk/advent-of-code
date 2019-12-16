use std::{
    io::{self, BufRead, Write},
};

use regex::{
    Regex,
};

use lazy_static::lazy_static;

use super::{asm, disasm};

#[derive(Debug)]
pub enum MapFileError {
    ParseError(usize),
    IOError(io::Error),
}

impl From<io::Error> for MapFileError {
    fn from(other: io::Error) -> Self {
        MapFileError::IOError(other)
    }
}

pub fn read_map(read: &mut impl BufRead
      ) -> Result<disasm::LabelMap, MapFileError> {
    let mut map = disasm::LabelMap::new();
    for (i, line) in read.lines().enumerate() {
        let line = line?;
        let captures = LINE.captures(line.as_str())
            .ok_or(MapFileError::ParseError(i + 1))?;
        let ptr = captures.get(1)
            .ok_or(MapFileError::ParseError(i + 1))?
            .as_str()
            .parse::<usize>()
            .map_err(|_| MapFileError::ParseError(i + 1))?;
        let lbl = captures.get(2)
            .ok_or(MapFileError::ParseError(i + 1))?;
        map.insert(ptr, lbl.as_str().to_string());
    }
    Ok(map)
}

pub fn write_map(write: &mut impl Write, map: &asm::LabelMap
      ) -> Result<(), io::Error> {
    for (lbl, ptr) in map.iter() {
        writeln!(write, "{}\t{}", ptr, lbl)?;
    }
    Ok(())
}

pub fn to_output_map(input_map: &disasm::LabelMap) -> asm::LabelMap {
    let mut out_map = asm::LabelMap::new();
    for (ptr, lbl) in input_map.iter() {
        out_map.insert(lbl.as_str(), *ptr);
    }
    out_map
}

lazy_static! {
    static ref LINE: Regex = Regex::new(
        r"^\s*([0-9]+)\s+([A-Za-z_][A-Za-z0-9_]*)\s*$").unwrap();
}
