use std::{
    collections::HashSet,
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

const BEGIN_RED: &'static str = "\u{1b}[1;31m";
const BEGIN_GREEN: &'static str = "\u{1b}[1;32m";
const BEGIN_BLUE: &'static str = "\u{1b}[1;34m";
const BEGIN_YELLOW: &'static str = "\u{1b}[1;33m";
const CLEAR_COLOR: &'static str = "\u{1b}[0m";

#[derive(StructOpt, Debug)]
struct Options {
    #[structopt(short, long, default_value="512")]
    buf_size: usize,

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

/* "nice to have" / TODO list
 * interpret label as address for x, d, w, b
 * implicit arguments: ptr = ip, len = 1
 */

enum TracerCommand {
    Step,
    Examine(Option<usize>, usize, bool),
    DisAsm(usize, usize, bool),
    SetLabel(usize, String),
    ClearLabel(String),
    SetBreakpoint(usize),
    ClearBreakpoint(usize),
    Jump(usize),
    SetRelBase(i64),
    Continue(usize),
    Write(usize, i64),
    Asm(usize),
    SaveLabels(String),
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

            "d" | "disassemble" | "dn" | "disassemble-nums" => {
                let ptr = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let ptr = ptr.parse::<usize>()
                    .map_err(|_| IntCodeError::ParseError)?;
                let len = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let len = len.parse::<usize>()
                    .map_err(|_| IntCodeError::ParseError)?;
                TracerCommand::DisAsm(ptr, len,
                    cmd_word == "dn" || cmd_word == "disassemble-nums")
            },

            "l" | "label" => {
                let ptr = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let ptr = ptr.parse::<usize>()
                    .map_err(|_| IntCodeError::ParseError)?;
                let lbl = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                TracerCommand::SetLabel(ptr, lbl.to_string())
            },

            "r" | "clear" => {
                let ptr_or_label = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                if let Ok(ptr) = ptr_or_label.parse::<usize>() {
                    TracerCommand::ClearBreakpoint(ptr)
                } else {
                    TracerCommand::ClearLabel(ptr_or_label.to_string())
                }
            },

            "b" | "break" => {
                let ptr = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let ptr = ptr.parse::<usize>()
                    .map_err(|_| IntCodeError::ParseError)?;
                TracerCommand::SetBreakpoint(ptr)
            },

            "j" | "jump" => {
                let ptr = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let ptr = ptr.parse::<usize>()
                    .map_err(|_| IntCodeError::ParseError)?;
                TracerCommand::Jump(ptr)
            },

            "t" | "relative" => {
                let offset = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let offset = offset.parse::<i64>()
                    .map_err(|_| IntCodeError::ParseError)?;
                TracerCommand::SetRelBase(offset)
            },

            "c" | "continue" => {
                let ptr = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let ptr = ptr.parse::<usize>()
                    .map_err(|_| IntCodeError::ParseError)?;
                TracerCommand::Continue(ptr)
            },

            "w" | "write" => {
                let ptr = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let ptr = ptr.parse::<usize>()
                    .map_err(|_| IntCodeError::ParseError)?;
                let num = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let num = num.parse::<i64>()
                    .map_err(|_| IntCodeError::ParseError)?;
                TracerCommand::Write(ptr, num)
            },

            "a" | "assemble" => {
                let ptr = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let ptr = ptr.parse::<usize>()
                    .map_err(|_| IntCodeError::ParseError)?;
                TracerCommand::Asm(ptr)
            },

            "v" | "savelabels" => {
                let path = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                TracerCommand::SaveLabels(path.to_string())
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
                tracer.set_continue(ip);
                TracerCommandResult::Step
            },

            TracerCommand::Write(ptr, val) => {
                *program.memory.get_mut(ptr) = val;
                TracerCommandResult::WaitCommands
            },

            TracerCommand::Asm(ptr) => {
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
                        match asm::assemble(&stmts[..]) {
                            Ok(items) => {
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

                TracerCommandResult::WaitCommands
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

            TracerCommand::Help => {
                println!("{}ictrace - tracer commands:", BEGIN_YELLOW);
                println!("(s)tep");
                println!("e(x)amine <ptr> <len> [nums]");
                println!("(xn)/examine-nums <ptr> <len>");
                println!("(d)isassemble <ptr> <len>");
                println!("(dn)/disassemble-nums <ptr> <len>");
                println!("(l)abel <ptr> <lbl>");
                println!("clea(r) <lbl>|<breakpoint>");
                println!("(b)reak <ptr>");
                println!("(j)ump <ptr>");
                println!("rela(t)ive <ptr>");
                println!("(c)ontinue <ptr>");
                println!("(w)rite <ptr> <num>");
                println!("(a)ssemble <ptr>");
                println!("sa(v)elabels <file>");
                println!("(h)elp");
                println!("restart");
                println!("(q)uit{}", CLEAR_COLOR);
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

struct InputIter { }

impl Iterator for InputIter {
    type Item = i64;

    fn next(&mut self) -> Option<i64> {
        loop {
            let mut line = String::new();
            print!("{}input>{} ", BEGIN_GREEN, CLEAR_COLOR);
            io::stdout().flush().ok()?;
            io::stdin().read_line(&mut line).ok()?;
            if let Ok(num) = line.trim().parse::<i64>() {
                return Some(num);
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
        for line in BufReader::new(file).lines() {
            let i = line?.trim_end().parse::<i64>()
                .map_err(|_| IntCodeError::ParseError)?;
            start_input.push(i);
        }
    }

    let mut program = ProgramState::<ExpandoSparse>::from(image.clone());
    let mut tracer = if let Some(map_file) = &options.map_file {
        TracerState::with_labels(map_file::read_map(
                &mut BufReader::new(File::open(map_file)?))?)
    } else {
        TracerState::new()
    };

    let mut input = stream::iter(start_input.drain(..).chain(InputIter { }));

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
                    println!("{}output> {}{}", BEGIN_GREEN, msg, CLEAR_COLOR);
                },
                None => break,
            };
        }
    }

    Ok(())
}

fn main() -> Result<(), Error> {
    let options = Options::from_args();

    let mut program = String::new();
    File::open(&options.input_file)?.read_to_string(&mut program)?;

    let program: Vec<i64> = program.trim_end().split(',')
        .map(|word| word.parse().map_err(|_| IntCodeError::ParseError))
        .collect::<Result<_, _>>()?;

    block_on(run_vm(program, &options))
}
