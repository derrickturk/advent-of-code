use std::{
    collections::HashSet,
    io::{self, Read, BufRead, BufReader},
    fs::File,
    marker::Unpin,
    path::PathBuf,
};

use futures::{
    executor::block_on,
    channel::mpsc,
    sink::{Sink, SinkExt},
};

use structopt::StructOpt;

use intcode::*;
use intcode::disasm::{LabelMap, DisAsm};

#[derive(StructOpt, Debug)]
struct Options {
    #[structopt(short, long, default_value="512")]
    buf_size: usize,

    #[structopt(short="s", long)]
    start_input: Option<Vec<i64>>,

    #[structopt(short="f", long, parse(from_os_str))]
    start_input_file: Option<PathBuf>,

    #[structopt(name="FILE", parse(from_os_str))]
    input_file: PathBuf,
}

#[derive(Debug)]
enum Error {
    IntCodeError(IntCodeError),
    IOError(io::Error),
    UnknownCommand(String),
    StartInputError,
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
            false
        } else if let Some(til) = self.continue_til {
            if ip == til {
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
}

enum TracerCommand {
    Step,
    Input(i64),
    Examine(usize, usize),
    DisAsm(usize),
    SetLabel(usize, String),
    ClearLabel(String),
    SetBreakpoint(usize),
    ClearBreakpoint(usize),
    Jump(usize),
    Continue(usize),
    Write(usize, i64),
    Help,
}

impl TracerCommand {
    fn get_command() -> Result<TracerCommand, Error> {
        let mut line = String::new();
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

            "i" | "input" => {
                let num = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let num = num.parse::<i64>()
                    .map_err(|_| IntCodeError::ParseError)?;
                TracerCommand::Input(num)
            },

            "x" | "examine" => {
                let ptr = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let ptr = ptr.parse::<usize>()
                    .map_err(|_| IntCodeError::ParseError)?;
                let len = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let len = len.parse::<usize>()
                    .map_err(|_| IntCodeError::ParseError)?;
                TracerCommand::Examine(ptr, len)
            },

            "d" | "disassemble" => {
                let ptr = cmd.next()
                    .ok_or_else(|| Error::UnknownCommand(line.clone()))?;
                let ptr = ptr.parse::<usize>()
                    .map_err(|_| IntCodeError::ParseError)?;
                TracerCommand::DisAsm(ptr)
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

            "h" | "help" => TracerCommand::Help,

            _ => return Err(Error::UnknownCommand(line.clone())),
        };

        Ok(res)

        /*
        eprintln!("ictrace - tracer commands:");
        eprintln!("(s)tep");
        eprintln!("(i)nput <num>");
        eprintln!("e(x)amine <ptr> <len>");
        eprintln!("(d)isassemble <ptr>");
        eprintln!("(l)abel <ptr> <lbl>");
        eprintln!("clea(r) <lbl>|<breakpoint>");
        eprintln!("(b)reak <ptr>");
        eprintln!("(j)ump <ptr>");
        eprintln!("(c)ontinue <ptr>");
        eprintln!("(w)rite <ptr> <num>");
        eprintln!("(h)elp");
        */
    }

    async fn exec<T, I>(self, program: &mut ProgramState<T>,
          tracer: &mut TracerState, input_send: &mut I) -> TracerCommandResult
          where T: ExpandoMemory
              , I: Sink<i64> + Unpin {
        match self {
            TracerCommand::Step => TracerCommandResult::Step,

            TracerCommand::Input(n) => {
                match input_send.send(n).await {
                    Ok(_) => { },
                    Err(_) => { eprintln!("failed to send!") },
                };
                TracerCommandResult::WaitCommands
            },

            TracerCommand::Examine(ptr, len) => {
                let mut sep = "";
                for p in ptr..ptr + len {
                    eprint!("{}{}", sep, *program.memory.get(p));
                    sep = ",";
                }
                eprintln!();
                TracerCommandResult::WaitCommands
            },

            // TODO: disassemble range?
            TracerCommand::DisAsm(ptr) => {
                let orig_ip = program.ip;

                program.ip = ptr;

                if let Ok(opcode) = OpCode::parse_next(program) {
                    match opcode.disassemble_at(&mut io::stderr(),
                          &tracer.labels, program.ip) {
                        Ok(_) => { eprintln!() },
                        Err(e) => { eprintln!("disassembly failed: {:?}", e) },
                    };
                } else {
                    eprintln!("${}", *program.memory.get(ptr));
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

            TracerCommand::Continue(ip) => {
                tracer.set_continue(ip);
                TracerCommandResult::Step
            },

            TracerCommand::Write(ptr, val) => {
                *program.memory.get_mut(ptr) = val;
                TracerCommandResult::WaitCommands
            },

            TracerCommand::Help => {
                eprintln!("ictrace - tracer commands:");
                eprintln!("(s)tep");
                eprintln!("(i)nput <num>");
                eprintln!("e(x)amine <ptr> <len>");
                eprintln!("(d)isassemble <ptr>");
                eprintln!("(l)abel <ptr> <lbl>");
                eprintln!("clea(r) <lbl>|<breakpoint>");
                eprintln!("(b)reak <ptr>");
                eprintln!("(j)ump <ptr>");
                eprintln!("(c)ontinue <ptr>");
                eprintln!("(w)rite <ptr> <num>");
                eprintln!("(h)elp");
                eprintln!();
                TracerCommandResult::WaitCommands
            },
        }
    }
}

async fn run_vm(program: Vec<i64>, options: &Options) -> Result<(), Error> {
    let (mut input_send, mut input_recv) = mpsc::channel(options.buf_size);
    let (mut output_send, mut output_recv) = mpsc::channel(options.buf_size);

    if let Some(input) = &options.start_input {
        for i in input {
            input_send.send(*i).await.map_err(|_| Error::StartInputError)?;
        }
    }

    if let Some(input_file) = &options.start_input_file {
        let file = File::open(input_file)?;
        for line in BufReader::new(file).lines() {
            for c in line?.chars() {
                input_send.send(c as i64)
                    .await.map_err(|_| Error::StartInputError)?;
            }
        }
    }

    let mut program = ProgramState::<ExpandoSparse>::from(program);
    let mut tracer = TracerState::new();

    loop {
        let opcode = OpCode::parse_next(&mut program)?;
        eprintln!("ip: {}, rb: {}", program.ip, program.relative_base);

        match opcode.disassemble_at(&mut io::stderr(),
              &tracer.labels, program.ip) {
            Ok(_) => { eprintln!() },
            Err(e) => { eprintln!("disassembly failed: {:?}", e) },
        };

        if !tracer.should_continue(program.ip) {
            let mut should_jump = false;
            loop {
                let cmd = match TracerCommand::get_command() {
                    Ok(cmd) => cmd,
                    Err(e) => {
                        eprintln!("invalid command: {:?}", e);
                        continue;
                    },
                };

                match cmd.exec(&mut program, &mut tracer, &mut input_send).await {
                    TracerCommandResult::Step => break,
                    TracerCommandResult::Jump => { should_jump = true; break; },
                    TracerCommandResult::WaitCommands => continue,
                };
            }
            if should_jump { continue; }
        }

        match opcode {
            OpCode::Halt => return Ok(()),
            _ => opcode.eval(&mut program, &mut input_recv,
                &mut output_send).await?,
        }

        if let Ok(msg) = output_recv.try_next() {
            match msg {
                Some(msg) => println!("output: {}", msg),
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
