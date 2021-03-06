use std::{
    borrow::Borrow,
    convert::TryInto,
    collections::{HashSet, hash_map::Entry},
    io::{self, Read, BufRead, BufReader, Write},
    fs::File,
    path::PathBuf,
};

use futures::{
    executor::block_on,
    channel::mpsc,
    stream,
};

use structopt::StructOpt;

use intcode::*;
use intcode::disasm::{LabelMap, DisAsm};

#[cfg(windows)]
use winapi::um::{
    handleapi::INVALID_HANDLE_VALUE,
    winbase::{STD_OUTPUT_HANDLE},
    wincon::{ENABLE_VIRTUAL_TERMINAL_PROCESSING},
    processenv::GetStdHandle,
    consoleapi::{GetConsoleMode, SetConsoleMode},
};

const BEGIN_RED: &'static str = "\u{1b}[1;31m";
const BEGIN_GREEN: &'static str = "\u{1b}[1;32m";
const BEGIN_BLUE: &'static str = "\u{1b}[1;34m";
const BEGIN_YELLOW: &'static str = "\u{1b}[1;33m";
const CLEAR_COLOR: &'static str = "\u{1b}[0m";

#[derive(StructOpt, Debug)]
struct Options {
    #[structopt(short, long, default_value="512")]
    buf_size: usize,

    #[structopt(short, long)]
    text: bool,

    #[structopt(short="s", long)]
    start_input: Option<Vec<i64>>,

    #[structopt(short="f", long, parse(from_os_str))]
    start_input_file: Option<PathBuf>,

    #[structopt(short, long, parse(from_os_str))]
    map_file: Option<PathBuf>,

    #[structopt(name="FILE", parse(from_os_str))]
    input_file: PathBuf,
}

#[derive(Debug)]
enum Error {
    IntCodeError(IntCodeError),
    IOError(io::Error),
    MapFileError(map_file::MapFileError),
    UnknownCommand(String),
    UnknownLabel(String),
}

impl From<IntCodeError> for Error {
    fn from(other: IntCodeError) -> Self {
        Error::IntCodeError(other)
    }
}

impl From<io::Error> for Error {
    fn from(other: io::Error) -> Self {
        Error::IOError(other)
    }
}

impl From<map_file::MapFileError> for Error {
    fn from(other: map_file::MapFileError) -> Self {
        Error::MapFileError(other)
    }
}

struct TracerState {
    labels: LabelMap,
    breakpoints: HashSet<usize>,
    continue_til: Option<usize>,
}

impl TracerState {
    fn new() -> Self {
        Self {
            labels: LabelMap::new(),
            breakpoints: HashSet::new(),
            continue_til: None,
        }
    }

    fn with_labels(labels: LabelMap) -> Self {
        Self {
            labels,
            breakpoints: HashSet::new(),
            continue_til: None,
        }
    }

    #[inline]
    fn set_label(&mut self, ptr: usize, label: String) {
        self.labels.insert(ptr, label);
    }

    #[inline]
    fn clear_label(&mut self, label: &str) {
        let ptr = self.labels.iter()
            .find(|(_, v)| v.as_str() == label)
            .map(|(ptr, _)| *ptr);
        match ptr {
            Some(ptr) => { self.labels.remove(&ptr); },
            None => { },
        };
    }

    #[inline]
    fn set_breakpoint(&mut self, ptr: usize) {
        self.breakpoints.insert(ptr);
    }

    #[inline]
    fn clear_breakpoint(&mut self, ptr: usize) {
        self.breakpoints.remove(&ptr);
    }

    #[inline]
    fn set_continue(&mut self, ptr: usize) {
        self.continue_til = Some(ptr);
    }

    #[inline]
    fn should_continue(&mut self, ip: usize) -> bool {
        if self.breakpoints.contains(&ip) {
            self.continue_til = None;
            false
        } else if let Some(til) = self.continue_til {
            if ip >= til {
                self.continue_til = None;
                false
            } else {
                true
            }
        } else {
            false
        }
    }
}

enum TracerCommandResult {
    Step,
    Jump,
    WaitCommands,
    Restart,
    Quit,
}

enum TracerCommand {
    Step,
    Examine(Option<usize>, usize, bool),
    DisAsm(Option<usize>, usize, bool),
    SetLabel(usize, String),
    ClearLabel(String),
    SetBreakpoint(usize),
    ClearBreakpoint(usize),
    Jump(usize),
    SetRelBase(i64),
    Continue(Option<usize>),
    Write(usize, i64),
    Asm(Option<usize>),
    SaveLabels(String),
    SaveImage(String),
    Help,
    Restart,
    Quit,
}

impl TracerCommand {
    fn get_command(labels: &LabelMap) -> Result<TracerCommand, Error> {
        let mut line = String::new();
        print!("ictrace> ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut line)?;

        let cmd = line.trim();

        if cmd.is_empty() {
            return Ok(TracerCommand::Step);
        }

        let mut cmd = cmd.split_whitespace();
        let cmd_word = cmd.next()
            .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
        let res = match cmd_word {
            "s" | "step" => {
                TracerCommand::Step
            },

            "x" | "examine" | "xn" | "examine-nums" => {
                let ptr = cmd.next()
                    .map(|ptr| ptr_or_label(ptr, labels))
                    .transpose()?;
                let len = cmd.next()
                    .map(|len| len.parse::<usize>()
                        .map_err(|_| IntCodeError::ParseError))
                    .transpose()?;
                TracerCommand::Examine(ptr, len.unwrap_or(1),
                    cmd_word == "xn" || cmd_word == "examine-nums")
            },

            "d" | "disassemble" | "dn" | "disassemble-nums" | "dis" => {
                let ptr = cmd.next()
                    .map(|ptr| ptr_or_label(ptr, labels))
                    .transpose()?;
                let len = cmd.next()
                    .map(|len| len.parse::<usize>()
                        .map_err(|_| IntCodeError::ParseError))
                    .transpose()?;
                TracerCommand::DisAsm(ptr, len.unwrap_or(1),
                    cmd_word == "dn" || cmd_word == "disassemble-nums")
            },

            "l" | "label" | "lbl" => {
                let ptr = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let ptr = ptr_or_label(ptr, labels)?;
                let lbl = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                TracerCommand::SetLabel(ptr, lbl.to_string())
            },

            "u" | "unlabel" => {
                let label = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                TracerCommand::ClearLabel(label.to_string())
            },

            "r" | "clear" => {
                let ptr = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let ptr = ptr_or_label(ptr, labels)?;
                TracerCommand::ClearBreakpoint(ptr)
            },

            "b" | "break" => {
                let ptr = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let ptr = ptr_or_label(ptr, labels)?;
                TracerCommand::SetBreakpoint(ptr)
            },

            "j" | "jump" => {
                let ptr = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let ptr = ptr_or_label(ptr, labels)?;
                TracerCommand::Jump(ptr)
            },

            "t" | "relative" | "rel" => {
                let offset = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let offset = offset.parse::<i64>()
                    .map_err(|_| IntCodeError::ParseError)?;
                TracerCommand::SetRelBase(offset)
            },

            "c" | "continue" => {
                let ptr = cmd.next()
                    .map(|ptr| ptr_or_label(ptr, labels))
                    .transpose()?;
                TracerCommand::Continue(ptr)
            },

            "w" | "write" => {
                let ptr = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let ptr = ptr_or_label(ptr, labels)?;
                let num = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let num = num.parse::<i64>()
                    .map_err(|_| IntCodeError::ParseError)?;
                TracerCommand::Write(ptr, num)
            },

            "a" | "assemble" | "asm" => {
                let ptr = cmd.next()
                    .map(|ptr| ptr_or_label(ptr, labels))
                    .transpose()?;
                TracerCommand::Asm(ptr)
            },

            "v" | "savelabels" => {
                let path = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                TracerCommand::SaveLabels(path.to_string())
            },

            "i" | "saveimage" => {
                let path = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                TracerCommand::SaveImage(path.to_string())
            },

            "h" | "help" => TracerCommand::Help,

            "restart" => TracerCommand::Restart,

            "q" | "quit" => TracerCommand::Quit,

            _ => return Err(Error::UnknownCommand(line.clone())),
        };

        Ok(res)
    }

    async fn exec<T>(self, program: &mut ProgramState<T>,
          tracer: &mut TracerState) -> TracerCommandResult
          where T: ExpandoMemory {
        match self {
            TracerCommand::Step => TracerCommandResult::Step,

            TracerCommand::Examine(ptr, len, nums) => {
                let ptr = ptr.unwrap_or(program.ip);
                if nums {
                    for p in ptr..ptr + len {
                        println!("{}{}{}\t{}", BEGIN_YELLOW, p, CLEAR_COLOR,
                            *program.memory.get(p));
                    }
                } else {
                    let mut sep = "";
                    for p in ptr..ptr + len {
                        print!("{}{}", sep, *program.memory.get(p));
                        sep = ",";
                    }
                    println!();
                }
                TracerCommandResult::WaitCommands
            },

            TracerCommand::DisAsm(ptr, len, nums) => {
                let orig_ip = program.ip;

                let ptr = ptr.unwrap_or(program.ip);
                program.ip = ptr;

                while program.ip < ptr + len {
                    if nums {
                        print!("{}{}{}\t", BEGIN_YELLOW, program.ip,
                            CLEAR_COLOR);
                    }
                    if let Ok(opcode) = OpCode::parse_next(program) {
                        match opcode.disassemble_at(&mut io::stdout(),
                              &tracer.labels, program.ip) {
                            Ok(_) => { println!() },
                            Err(e) => {
                                if nums { println!() };
                                eprintln!("{}disassembly failed: {:?}{}",
                                    BEGIN_RED, e, CLEAR_COLOR);
                                break;
                            },
                        };
                        program.ip += opcode.width();
                    } else {
                        if let Some(lbl) = tracer.labels.get(&program.ip) {
                            print!("{}: ", lbl);
                        }
                        println!("${}", *program.memory.get(program.ip));
                        program.ip += 1;
                    }
                }

                program.ip = orig_ip;
                TracerCommandResult::WaitCommands
            },

            TracerCommand::SetLabel(ptr, lbl) => {
                tracer.set_label(ptr, lbl);
                TracerCommandResult::WaitCommands
            },

            TracerCommand::ClearLabel(lbl) => {
                tracer.clear_label(&lbl);
                TracerCommandResult::WaitCommands
            },

            TracerCommand::SetBreakpoint(ptr) => {
                tracer.set_breakpoint(ptr);
                TracerCommandResult::WaitCommands
            },

            TracerCommand::ClearBreakpoint(ptr) => {
                tracer.clear_breakpoint(ptr);
                TracerCommandResult::WaitCommands
            },

            TracerCommand::Jump(ip) => {
                program.ip = ip;
                TracerCommandResult::Jump
            },

            TracerCommand::SetRelBase(rb) => {
                program.relative_base = rb;
                TracerCommandResult::WaitCommands
            },

            TracerCommand::Continue(ip) => {
                tracer.set_continue(ip.unwrap_or(std::usize::MAX));
                TracerCommandResult::Step
            },

            TracerCommand::Write(ptr, val) => {
                *program.memory.get_mut(ptr) = val;
                // we might have written over the current instruction!
                // trigger a "jump" to re-parse it
                TracerCommandResult::Jump
            },

            TracerCommand::Asm(ptr) => {
                let ptr = ptr.unwrap_or(program.ip);
                let mut lines = Vec::new();
                loop {
                    let mut line = String::new();
                    print!("{}asm>{} ", BEGIN_YELLOW, CLEAR_COLOR);
                    if let Err(e) = io::stdout().flush() {
                        eprintln!("{}IO error: {:?}{}", BEGIN_RED, e,
                            CLEAR_COLOR);
                    }
                    if let Err(e) = io::stdin().read_line(&mut line) {
                        eprintln!("{}IO error: {:?}{}", BEGIN_RED, e,
                            CLEAR_COLOR);
                    }

                    if line.trim().is_empty() {
                        break;
                    }

                    lines.push(line);
                }

                let stmts = asm_parser::parse(lines.iter().map(|s| s.as_str()));
                let words = match stmts {
                    Ok(stmts) => {
                        let mut labels = map_file::to_output_map(
                            &tracer.labels);

                        match asm::extract_labels(&stmts[..]) {
                            Ok(new_labels) => {
                                for (k, v) in new_labels.iter() {
                                    match labels.entry(k) {
                                        Entry::Occupied(_) => {
                                            eprintln!(
                                                "{}label already defined: {}{}",
                                                BEGIN_RED, k, CLEAR_COLOR);
                                        },
                                        Entry::Vacant(place) => {
                                            place.insert(*v);
                                        },
                                    };
                                }
                            },
                            Err(e) => {
                                eprintln!("{}assembler error: {:?}{}",
                                    BEGIN_RED, e, CLEAR_COLOR);
                            },
                        };

                        match asm::assemble_with_labels(&stmts[..], &labels) {
                            Ok(items) => {
                                let mut new_map = map_file::to_input_map(
                                    &labels);
                                for (ptr, lbl) in new_map.drain() {
                                    /* insert any new labels (as defined by
                                     * previously unlabeled addrs) into
                                     * the label map */
                                    match tracer.labels.entry(ptr) {
                                        Entry::Occupied(_) => { },
                                        Entry::Vacant(place) => {
                                            place.insert(lbl);
                                        },
                                    }
                                }

                                Some(asm::program_values(&items[..]))
                            },
                            Err(e) => {
                                eprintln!("{}assembler error: {:?}{}",
                                    BEGIN_RED, e, CLEAR_COLOR);
                                None
                            },
                        }
                    },
                    Err(e) => {
                        eprintln!("{}parse error: {:?}{}", BEGIN_RED, e,
                            CLEAR_COLOR);
                        None
                    },
                };

                if let Some(words) = words {
                    for (i, val) in words.iter().enumerate() {
                        *program.memory.get_mut(ptr + i) = *val;
                    }
                }

                // we might have assembled over the current instruction!
                // trigger a "jump" to re-parse it
                TracerCommandResult::Jump
            },

            TracerCommand::SaveLabels(path) => {
                let out_labels = map_file::to_output_map(&tracer.labels);
                if let Ok(mut file) = File::create(path) {
                    match map_file::write_map(&mut file, &out_labels) {
                        Ok(_) => { },
                        Err(e) => {
                            eprintln!("{}unable to write: {:?}{}",
                                BEGIN_RED, e, CLEAR_COLOR);
                        },
                    }
                } else {
                    eprintln!("{}unable to open file{}",
                        BEGIN_RED, CLEAR_COLOR);
                }
                TracerCommandResult::WaitCommands
            },

            TracerCommand::SaveImage(path) => {
                let image = program.memory.image();
                if let Ok(mut file) = File::create(path) {
                    match write_image(&mut file, &image) {
                        Ok(_) => { },
                        Err(e) => {
                            eprintln!("{}unable to write: {:?}{}",
                                BEGIN_RED, e, CLEAR_COLOR);
                        },
                    }
                } else {
                    eprintln!("{}unable to open file{}",
                        BEGIN_RED, CLEAR_COLOR);
                }
                TracerCommandResult::WaitCommands
            },

            TracerCommand::Help => {
                println!("{}ictrace - tracer commands:", BEGIN_YELLOW);
                println!("  (s)tep");
                println!("  e(x)amine <ptr> <len> [nums]");
                println!("  (xn)/examine-nums <ptr> <len>");
                println!("  (d)isassemble <ptr> <len>");
                println!("  (dn)/disassemble-nums <ptr> <len>");
                println!("  (l)abel <ptr> <lbl>");
                println!("  (u)nlabel <ptr>");
                println!("  clea(r) <breakpoint>");
                println!("  (b)reak <ptr>");
                println!("  (j)ump <ptr>");
                println!("  rela(t)ive <ptr>");
                println!("  (c)ontinue <ptr>");
                println!("  (w)rite <ptr> <num>");
                println!("  (a)ssemble <ptr>");
                println!("  sa(v)elabels <file>");
                println!("  save(i)mage <file>");
                println!("  (h)elp");
                println!("  restart");
                println!("  (q)uit{}", CLEAR_COLOR);
                println!();
                TracerCommandResult::WaitCommands
            },

            TracerCommand::Restart => {
                TracerCommandResult::Restart
            },

            TracerCommand::Quit => {
                println!("{}bye!{}", BEGIN_YELLOW, CLEAR_COLOR);
                TracerCommandResult::Quit
            },
        }
    }
}

#[inline]
fn ptr_or_label(input: &str, labels: &LabelMap) -> Result<usize, Error> {
    if let Ok(ptr) = input.parse::<usize>() {
        return Ok(ptr);
    }
    if let Some(ptr) = labels.iter()
          .find(|(_, v)| v.as_str() == input).map(|(k, _)| *k) {
        Ok(ptr)
    } else {
        Err(Error::UnknownLabel(input.to_string()))
    }
}

#[inline]
fn write_image(writer: &mut impl Write, image: &impl Borrow<[i64]>)
      -> Result<(), Error> {
    let mut sep = "";
    for val in image.borrow().iter() {
        write!(writer, "{}{}", sep, val)?;
        sep = ",";
    }
    Ok(())
}

struct InputIter {
    text: bool,
    line: String,
}

impl InputIter {
    #[inline]
    fn new(text: bool) -> Self {
        Self { text, line: String::new(), }
    }
}

impl Iterator for InputIter {
    type Item = i64;

    fn next(&mut self) -> Option<i64> {
        loop {
            if self.text {
                if self.line.is_empty() {
                    print!("{}input>{} ", BEGIN_GREEN, CLEAR_COLOR);
                    io::stdout().flush().ok()?;
                    self.line.clear();
                    io::stdin().read_line(&mut self.line).ok()?;
                }
                if let Some(first) = self.line.drain(..1).next() {
                    if first == '\r' {
                        continue;
                    }
                    return Some(first as i64);
                }
            } else {
                print!("{}input>{} ", BEGIN_GREEN, CLEAR_COLOR);
                io::stdout().flush().ok()?;
                self.line.clear();
                io::stdin().read_line(&mut self.line).ok()?;
                if let Ok(num) = self.line.trim().parse::<i64>() {
                    return Some(num);
                }
            }
        }
    }
}

async fn run_vm(image: Vec<i64>, options: &Options) -> Result<(), Error> {
    let (mut output_send, mut output_recv) = mpsc::channel(options.buf_size);

    let mut start_input = Vec::new();

    if let Some(input) = &options.start_input {
        for i in input {
            start_input.push(*i);
        }
    }

    if let Some(input_file) = &options.start_input_file {
        let file = File::open(input_file)?;
        if options.text {
            for line in BufReader::new(file).lines() {
                for c in line?.chars() {
                    start_input.push(c as i64);
                }
                start_input.push('\n' as i64);
            }
        } else {
            for line in BufReader::new(file).lines() {
                let i = line?.trim_end().parse::<i64>()
                    .map_err(|_| IntCodeError::ParseError)?;
                start_input.push(i);
            }
        }
    }

    let mut program = ProgramState::<ExpandoSparse>::from(image.clone());
    let mut tracer = if let Some(map_file) = &options.map_file {
        TracerState::with_labels(map_file::read_map(
                &mut BufReader::new(File::open(map_file)?))?)
    } else {
        TracerState::new()
    };

    let mut input = stream::iter(start_input.drain(..)
        .chain(InputIter::new(options.text)));

    loop {
        let opcode = OpCode::parse_next(&mut program)?;
        println!("{}ip: {}, rb: {}{}", BEGIN_BLUE, program.ip,
            program.relative_base, CLEAR_COLOR);

        print!("{}", BEGIN_BLUE);
        match opcode.disassemble_at(&mut io::stdout(),
              &tracer.labels, program.ip) {
            Ok(_) => {
                println!("{}", CLEAR_COLOR);
            },
            Err(e) => {
                println!("{}", CLEAR_COLOR);
                eprintln!("{}disassembly failed: {:?}{}",
                    BEGIN_RED, e, CLEAR_COLOR);
            },
        };

        if !tracer.should_continue(program.ip) {
            let mut should_jump = false;
            loop {
                let cmd = match TracerCommand::get_command(&tracer.labels) {
                    Ok(cmd) => cmd,
                    Err(e) => {
                        eprintln!("{}invalid command: {:?}{}",
                            BEGIN_RED, e, CLEAR_COLOR);
                        continue;
                    },
                };

                match cmd.exec(&mut program, &mut tracer).await {
                    TracerCommandResult::Step => break,
                    TracerCommandResult::Jump => { should_jump = true; break; },
                    TracerCommandResult::WaitCommands => continue,
                    TracerCommandResult::Restart => {
                        program = ProgramState::<ExpandoSparse>::from(
                            image.clone());
                        should_jump = true;
                        break;
                    },
                    TracerCommandResult::Quit => return Ok(()),
                };
            }
            if should_jump { continue; }
        }

        match opcode {
            OpCode::Halt => return Ok(()),
            _ => opcode.eval(&mut program, &mut input, &mut output_send).await?,
        }

        if let Ok(msg) = output_recv.try_next() {
            match msg {
                Some(msg) => {
                    if options.text {
                        println!("{}output> {}{}", BEGIN_GREEN,
                            (msg as u32).try_into()
                              .unwrap_or(std::char::REPLACEMENT_CHARACTER),
                            CLEAR_COLOR);
                    } else {
                        println!("{}output> {}{}", BEGIN_GREEN, msg, CLEAR_COLOR);
                    }
                },
                None => break,
            };
        }
    }

    Ok(())
}

#[cfg(windows)]
fn set_ansi_console() {
    unsafe {
        let out = GetStdHandle(STD_OUTPUT_HANDLE);
        if out == INVALID_HANDLE_VALUE {
            return;
        }

        let mut mode = 0;
        if GetConsoleMode(out, &mut mode) == 0 {
            return;
        }

        SetConsoleMode(out, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
    }
}

fn main() -> Result<(), Error> {
    if cfg!(windows) {
        set_ansi_console();
    }

    let options = Options::from_args();

    let mut program = String::new();
    File::open(&options.input_file)?.read_to_string(&mut program)?;

    let program: Vec<i64> = program.trim_end().split(',')
        .map(|word| word.parse().map_err(|_| IntCodeError::ParseError))
        .collect::<Result<_, _>>()?;

    block_on(run_vm(program, &options))
}
