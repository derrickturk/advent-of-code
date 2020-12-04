use std::{
    fmt,
    io::{self, Read},
    error::Error,
    collections::HashMap,
};

const REQUIRED_KEYS: [(&'static str, fn (&str) -> bool); 7] = [
    ("byr", valid_byr),
    ("iyr", valid_iyr),
    ("eyr", valid_eyr),
    ("hgt", valid_hgt),
    ("hcl", valid_hcl),
    ("ecl", valid_ecl),
    ("pid", valid_pid),
    // "cid",
];

#[derive(Debug)]
struct Passport<'a>(HashMap<&'a str, &'a str>);

impl<'a> Passport<'a> {
    fn valid(&self) -> bool {
        for (k, _) in REQUIRED_KEYS.iter() {
            if !self.0.contains_key(k) {
                return false;
            }
        }
        true
    }

    fn valid2(&self) -> bool {
        for (k, validator) in REQUIRED_KEYS.iter() {
            if let Some(v) = self.0.get(k) {
                if !validator(v) {
                    return false;
                }
            } else {
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

fn valid_byr(val: &str) -> bool {
    if val.len() != 4 {
        return false;
    }

    match val.parse::<u16>() {
        Ok(year) => 1920 <= year && year <= 2002,
        _ => false,
    }
}

fn valid_iyr(val: &str) -> bool {
    if val.len() != 4 {
        return false;
    }

    match val.parse::<u16>() {
        Ok(year) => 2010 <= year && year <= 2020,
        _ => false,
    }
}

fn valid_eyr(val: &str) -> bool {
    if val.len() != 4 {
        return false;
    }

    match val.parse::<u16>() {
        Ok(year) => 2020 <= year && year <= 2030,
        _ => false,
    }
}

fn valid_hgt(val: &str) -> bool {
    match val.find(char::is_lowercase) {
        Some(unit_start) => {
            let (num, unit) = val.split_at(unit_start);
            if let Ok(num) = num.parse::<u8>() {
                match unit {
                    "cm" => 150 <= num && num <= 193,
                    "in" => 59 <= num && num <= 76,
                    _ => false,
                }
            } else {
                false
            }
        },
        _ => false,
    }
}

fn valid_hcl(val: &str) -> bool {
    if val.len() != 7 {
        return false;
    }

    let (hash, num) = val.split_at(1);
    hash == "#" && num.chars().all(
      |c| (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))
}

fn valid_ecl(val: &str) -> bool {
    match val {
        "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => true,
        _ => false
    }
}

fn valid_pid(val: &str) -> bool {
    val.len() == 9 && val.chars().all(|c| c.is_ascii_digit())
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    let passports = parse_passports(&input)?;
    let valid_count = passports.iter().filter(|p| p.valid()).count();
    let valid_count2 = passports.iter().filter(|p| p.valid2()).count();
    println!("{} valid passports", valid_count);
    println!("{} valid2 passports", valid_count2);
    Ok(())
}
