use std::{
    fmt,
    io::{self, Read},
    error::Error,
    collections::HashMap,
};

const REQUIRED_KEYS: [&'static str; 7] = [
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid",
    // "cid",
];

#[derive(Debug)]
struct Passport<'a>(HashMap<&'a str, &'a str>);

impl<'a> Passport<'a> {
    fn valid(&self) -> bool {
        for k in REQUIRED_KEYS.iter() {
            if !self.0.contains_key(k) {
                return false;
            }
        }
        true
    }
}

#[derive(Debug)]
enum ParseError {
    InvalidEntry(String),
    MissingValue(String),
    DuplicateKey(String),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for ParseError { }

fn parse_passports(file: &str) -> Result<Vec<Passport>, ParseError> {
    let mut passports = Vec::new();
    let mut entries = HashMap::new();
    for line in file.lines() {
        if line == "" {
            passports.push(Passport(entries));
            entries = HashMap::new();
        }

        for entry in line.split_whitespace() {
            match entry.find(':') {
                Some(ix) => {
                    let (k, v) = entry.split_at(ix);

                    if v.len() == 1 {
                        return Err(ParseError::MissingValue(k.to_owned()));
                    }

                    if let Some(_) = entries.insert(k, &v[1..]) {
                        return Err(ParseError::DuplicateKey(k.to_owned()));
                    }
                },

                None => return Err(ParseError::InvalidEntry(entry.to_owned())),
            };
        }
    }

    if !entries.is_empty() {
        passports.push(Passport(entries));
    }

    Ok(passports)
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    let passports = parse_passports(&input)?;
    let valid_count = passports.iter().filter(|p| p.valid()).count();
    dbg!(passports);
    println!("{} valid passports", valid_count);
    Ok(())
}
