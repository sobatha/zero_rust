use crate::helper::DynError;

use nix::{
    libc,
    sys::{
        signal::{killpg, signal, SigHandler, Signal},
        wait::{waitpid, WaitPidFlag, WaitStatus},
    },
    unistd::{self, dup2, execvp, fork, pipe, setpgid, tcgetpgrp, tcsetpgrp, ForkResult, Pid},
};
use rustyline::{error::ReadlineError, line_buffer::WordAction, Editor};
use signal_hook::{consts::*, iterator::Signals};
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    ffi::CString,
    hash::Hash,
    i32,
    mem::replace,
    path::PathBuf,
    process::exit,
    sync::mpsc::{channel, sync_channel, Receiver, Sender, SyncSender},
    thread,
};

enum WorkerMsg {
    Signal(i32),
    Cmd(String),
}

enum ShellMsg {
    Continue(i32),
    Quit(i32),
}

#[derive(Debug)]
pub struct Shell {
    logfile: String,
}
#[derive(Debug, PartialEq, Eq, Clone)]
enum ProcState {
    Run,
    Stop,
}

#[derive(Debug, Clone)]
struct ProcInfo {
    state: ProcState, // Process state
    pgid: Pid,        // Process group id
}

struct CleanUp<F>
where
    F: Fn(),
{
    f: F,
}

impl<F> Drop for CleanUp<F>
where
    F: Fn(),
{
    fn drop(&mut self) {
        (self.f)()
    }
}

#[derive(Debug)]
struct Worker {
    exit_val: i32,
    fg: Option<Pid>,                      // foreground's process group id
    jobs: BTreeMap<usize, (Pid, String)>, // Job id to (Pid, execution command)
    pgid_to_pids: HashMap<Pid, (usize, HashSet<Pid>)>, // process group id to (job id, process id)
    pid_to_info: HashMap<Pid, ProcInfo>,  // Process id to Process group id and state
    shell_pgid: Pid,                      //shell's process pid
}

type CmdResult<'a> = Result<Vec<(&'a str, Vec<&'a str>)>, DynError>;

impl Worker {
    pub fn new() -> Self {
        Worker {
            exit_val: 0,
            fg: None,
            jobs: BTreeMap::new(),
            pgid_to_pids: HashMap::new(),
            pid_to_info: HashMap::new(),
            shell_pgid: tcgetpgrp(libc::STDIN_FILENO).unwrap(),
        }
    }

    pub fn spawn(mut self, worker_rx: Receiver<WorkerMsg>, shell_tx: SyncSender<ShellMsg>) {
        thread::spawn(move || {
            for msg in worker_rx.iter() {
                match msg {
                    WorkerMsg::Cmd(line) => match parse_cmd(&line) {
                        Ok(cmd) => {
                            if self.build_in_cmd(&cmd, &shell_tx) {
                                continue;
                            }
                            if !self.spawn_child(&line, &cmd) {
                                shell_tx.send(ShellMsg::Continue(self.exit_val)).unwrap();
                            }
                        }
                        Err(e) => {
                            eprintln!("Zerosh: {e}");
                            shell_tx.send(ShellMsg::Continue(self.exit_val)).unwrap();
                        }
                    },
                    WorkerMsg::Signal(SIGCHLD) => {
                        self.wait_child(&shell_tx);
                    }
                    _ => (),
                }
            }
        });
    }

    fn build_in_cmd(&mut self, cmd: &[(&str, Vec<&str>)], shell_tx: &SyncSender<ShellMsg>) -> bool {
        if cmd.len() > 1 {
            return false;
        }
        match cmd[0].0 {
            "exit" => self.run_exit(&cmd[0].1, shell_tx),
            "jobs" => self.run_jobs(shell_tx),
            "fg" => self.run_fg(&cmd[0].1, shell_tx),
            "cd" => self.run_cd(&cmd[0].1, shell_tx),
            _ => false,
        }
    }

    fn run_exit(&mut self, args: &[&str], shell_tx: &SyncSender<ShellMsg>) -> bool {
        if !self.jobs.is_empty() {
            eprintln!("job is executing, not able to exit");
            self.exit_val = 1;
            shell_tx.send(ShellMsg::Continue(self.exit_val)).unwrap();
            return true;
        }

        let exit_val = match args.get(1) {
            Some(&s) => match (*s).parse::<i32>() {
                Ok(n) => n,
                _ => {
                    eprint!("invalid argument: {s}");
                    self.exit_val = 1;
                    shell_tx.send(ShellMsg::Continue(self.exit_val)).unwrap();
                    return true;
                }
            },
            None => self.exit_val,
        };

        shell_tx.send(ShellMsg::Quit(exit_val)).unwrap();

        true
    }

    fn run_cd(&mut self, args: &[&str], shell_tx: &SyncSender<ShellMsg>) -> bool {
        let path = if args.len() == 0 {
            dirs::home_dir()
                .or_else(|| Some(PathBuf::from("/")))
                .unwrap()
        } else {
            PathBuf::from(args[0])
        };

        self.exit_val = match std::env::set_current_dir(&path) {
            Err(e) => {
                eprintln!("cdに失敗: {e}");
                1
            }
            Ok(_) => 0,
        };

        shell_tx.send(ShellMsg::Continue(self.exit_val)).unwrap();
        true
    }

    fn run_fg(&mut self, args: &[&str], shell_tx: &SyncSender<ShellMsg>) -> bool {
        self.exit_val = 1;

        if args.len() < 2 {
            report_err(shell_tx, "usage: fg <number>", self.exit_val);
            return true;
        }

        if let Ok(n) = args[1].parse::<usize>() {
            if let Some((pgid, cmd)) = self.jobs.get(&n) {
                eprintln!("[{n}] restart\t{cmd}");

                self.fg = Some(*pgid);
                tcsetpgrp(libc::STDIN_FILENO, *pgid).unwrap();

                killpg(*pgid, Signal::SIGCONT).unwrap();
                return true;
            }
        }
        report_err(
            shell_tx,
            "cannot find the job name: {args[1]}",
            self.exit_val,
        );
        true
    }

    fn run_jobs(&mut self, shell_tx: &SyncSender<ShellMsg>) -> bool {
        for (job_id, (pgid, cmd)) in &self.jobs {
            let state = if self.is_group_stop(*pgid).unwrap() {
                "stopped"
            } else {
                "executing"
            };
            println!("[{job_id}] {state}\t{cmd}")
        }
        self.exit_val = 0;
        shell_tx.send(ShellMsg::Continue(self.exit_val)).unwrap();
        true
    }

    fn spawn_child(&mut self, line: &str, cmd: &[(&str, Vec<&str>)]) -> bool {
        assert_ne!(cmd.len(), 0);

        let job_id = if let Some(id) = self.get_new_job_id() {
            id
        } else {
            eprint!("Zerosh: job number exceeds the maximum");
            return false;
        };

        if cmd.len() > 2 {
            eprintln!("Zerosh: Pipe with more than three command is not supported.");
            return false;
        }

        let mut input = None;
        let mut output = None;

        if cmd.len() == 2 {
            let p = pipe().unwrap();
            input = Some(p.0);
            output = Some(p.1);
        }

        let cleanup_pipe = CleanUp {
            f: || {
                if let Some(fd) = input {
                    syscall(|| unistd::close(fd)).unwrap();
                }
                if let Some(fd) = output {
                    syscall(|| unistd::close(fd)).unwrap();
                }
            },
        };

        let pgid;

        match fork_exec(Pid::from_raw(0), cmd[0].0, &cmd[0].1, None, output) {
            Ok(child) => {
                pgid = child;
            }
            Err(e) => {
                eprintln!("Zerosh: Process creation error");

                return false;
            }
        }

        let info = ProcInfo {
            state: ProcState::Run,
            pgid,
        };

        let mut pids = HashMap::new();
        pids.insert(pgid, info.clone());

        if cmd.len() == 2 {
            match fork_exec(pgid, cmd[1].0, &cmd[1].1, input, None) {
                Ok(child) => {
                    pids.insert(child, info);
                }
                Err(e) => {
                    eprintln!("Zeshrc: process creation error");
                    return false;
                }
            }
        }

        std::mem::drop(cleanup_pipe);

        self.fg = Some(pgid);
        self.insert_job(job_id, pgid, pids, line);
        tcsetpgrp(libc::STDIN_FILENO, pgid).unwrap();

        true
    }

    fn wait_child(&mut self, shell_tx: &SyncSender<ShellMsg>) {
        let flag = Some(WaitPidFlag::WUNTRACED | WaitPidFlag::WNOHANG | WaitPidFlag::WCONTINUED);

        loop {
            match syscall(|| waitpid(Pid::from_raw(-1), flag)) {
                Ok(WaitStatus::Exited(pid, status)) => {
                    self.exit_val = status;
                    self.process_term(pid, shell_tx);
                }
                Ok(WaitStatus::Signaled(pid, sig, core)) => {
                    eprintln!(
                        "\nZerosh: child process has been terminated by signal {}: pid={pid}, signal={sig}", 
                        if core {"(core dump)"} else { "" }
                    );
                    self.exit_val = sig as i32 + 128;
                    self.process_term(pid, shell_tx);
                }
                Ok(WaitStatus::Stopped(pid, _sig)) => {
                    self.process_stop(pid, shell_tx);
                }
                Ok(WaitStatus::Continued(pid)) => self.process_continue(pid),
                Ok(WaitStatus::StillAlive) => return,
                Err(nix::Error::ECHILD) => return,
                Err(e) => {
                    eprintln!("\nZerosh: failed wait: {e}");
                    exit(1);
                }
                #[cfg(any(target_os = "linux", target_os = "android"))]
                Ok(WaitStatus::PrtaceEvent(pid, _, _) | WaitStatus::PtraceSyscall(pid)) => {
                    self.process_stop(pid, shell_tx)
                }
            }
        }
    }

    fn is_group_empty(&self, pgid: Pid) -> bool {
        self.pgid_to_pids.get(&pgid).unwrap().1.is_empty()
    }

    fn is_group_stop(&self, pgid: Pid) -> Option<bool> {
        for pid in self.pgid_to_pids.get(&pgid)?.1.iter() {
            if self.pid_to_info.get(pid)?.state == ProcState::Run {
                return Some(false);
            }
        }
        Some(true)
    }

    fn get_new_job_id(&self) -> Option<usize> {
        for i in 0..=usize::MAX {
            if !self.jobs.contains_key(&i) {
                return Some(i);
            }
        }
        None
    }

    fn insert_job(&mut self, job_id: usize, pgid: Pid, pids: HashMap<Pid, ProcInfo>, line: &str) {
        assert!(!self.jobs.contains_key(&job_id));
        self.jobs.insert(job_id, (pgid, line.to_string()));

        let mut procs = HashSet::new();
        for (pid, info) in pids {
            procs.insert(pid);

            assert!(!self.pid_to_info.contains_key(&pid));
            self.pid_to_info.insert(pid, info);
        }

        assert!(!self.pgid_to_pids.contains_key(&pgid));
        self.pgid_to_pids.insert(pgid, (job_id, procs));
    }

    fn process_term(&mut self, pid: Pid, shell_tx: &SyncSender<ShellMsg>) {
        if let Some((job_id, pgid)) = self.remove_pid(pid) {
            self.manage_job(job_id, pgid, shell_tx)
        }
    }

    fn process_stop(&mut self, pid: Pid, shell_tx: &SyncSender<ShellMsg>) {
        self.set_pid_state(pid, ProcState::Stop);
        let pgid = self.pid_to_info.get(&pid).unwrap().pgid;
        let job_id = self.pgid_to_pids.get(&pgid).unwrap().0;
        self.manage_job(job_id, pgid, shell_tx);
    }

    fn process_continue(&mut self, pid: Pid) {
        self.set_pid_state(pid, ProcState::Run);
    }

    fn manage_job(&mut self, job_id: usize, pgid: Pid, shell_tx: &SyncSender<ShellMsg>) {
        let is_fg = self.fg.map_or(false, |x| pgid == x);
        let line = &self.jobs.get(&job_id).unwrap();
        if is_fg {
            if self.is_group_empty(pgid) {
                eprintln!("[{job_id}] ternminated\t{:?}", line);
                self.remove_job(job_id);
                self.set_shell_fg(shell_tx);
            } else if self.is_group_stop(pgid).unwrap() {
                eprintln!("\n[{job_id}] stopped\t{:?}", line);
                self.set_shell_fg(shell_tx);
            }
        } else {
            if self.is_group_empty(pgid) {
                eprintln!("`n[{job_id}] teminated\t{:?}", line);
                self.remove_job(job_id);
            }
        }
    }

    fn set_pid_state(&mut self, pid: Pid, state: ProcState) -> Option<ProcState> {
        let info = self.pid_to_info.get_mut(&pid)?;
        Some(replace(&mut info.state, state))
    }

    fn remove_pid(&mut self, pid: Pid) -> Option<(usize, Pid)> {
        let pgid = self.pid_to_info.get(&pid)?.pgid;
        let it = self.pgid_to_pids.get_mut(&pgid)?;
        it.1.remove(&pgid);
        let job_id = it.0;
        Some((job_id, pgid))
    }

    fn remove_job(&mut self, job_id: usize) {
        if let Some((pgid, _)) = self.jobs.remove(&job_id) {
            if let Some((_, pids)) = self.pgid_to_pids.remove(&pgid) {
                assert!(pids.is_empty());
            }
        }
    }

    fn set_shell_fg(&mut self, shell_tx: &SyncSender<ShellMsg>) {
        self.fg = None;
        tcsetpgrp(libc::STDIN_FILENO, self.shell_pgid).unwrap();
        shell_tx.send(ShellMsg::Continue(self.exit_val)).unwrap();
    }
}

fn fork_exec(
    pgid: Pid,
    filename: &str,
    args: &[&str],
    input: Option<i32>,
    output: Option<i32>,
) -> Result<Pid, DynError> {
    let filename = CString::new(filename).unwrap();
    let args: Vec<CString> = args.iter().map(|s| CString::new(*s).unwrap()).collect();

    match syscall(|| unsafe { fork() })? {
        ForkResult::Parent { child, .. } => {
            setpgid(child, pgid).unwrap();
            Ok(child)
        }
        ForkResult::Child => {
            setpgid(Pid::from_raw(0), pgid).unwrap();

            if let Some(infd) = input {
                syscall(|| dup2(infd, libc::STDIN_FILENO)).unwrap();
            }
            if let Some(outfd) = output {
                syscall(|| dup2(outfd, libc::STDOUT_FILENO)).unwrap();
            }

            for i in 3..6 {
                let _ = syscall(|| unistd::close(i)).unwrap();
            }

            match execvp(&filename, &args) {
                Err(_) => {
                    unistd::write(libc::STDERR_FILENO, "unknown command exectution".as_bytes())
                        .ok();
                    exit(1);
                }
                Ok(_) => unreachable!(),
            }
        }
    }
}

impl Shell {
    pub fn new(logfile: &str) -> Self {
        Shell {
            logfile: logfile.to_string(),
        }
    }

    pub fn run(&self) -> Result<(), DynError> {
        unsafe { signal(Signal::SIGTTOU, SigHandler::SigIgn).unwrap() };
        let mut rl = Editor::<()>::new()?;
        if let Err(e) = rl.load_history(&self.logfile) {
            eprintln!("Zerosh: Failed to load history file: {e}")
        }

        let (worker_tx, worker_rx) = channel();
        let (shell_tx, shell_rx) = sync_channel(0);
        spawn_sig_handler(worker_tx.clone())?;
        Worker::new().spawn(worker_rx, shell_tx);

        let exit_val;
        let mut prev = 0;

        loop {
            let face = if prev == 0 { '\u{1F642}' } else { '\u{1F480}' };
            match rl.readline(&format!("Zerosh {face} % > ")) {
                Ok(line) => {
                    let line_trimmed = line.trim();
                    if line_trimmed.is_empty() {
                        continue;
                    } else {
                        rl.add_history_entry(line_trimmed);
                    }

                    worker_tx.send(WorkerMsg::Cmd(line)).unwrap();
                    match shell_rx.recv().unwrap() {
                        ShellMsg::Continue(n) => prev = n,
                        ShellMsg::Quit(n) => {
                            exit_val = n;
                            break;
                        }
                    }
                }
                Err(ReadlineError::Interrupted) => eprintln!("Zerosh: exit is Ctrl+d"),
                Err(ReadlineError::Eof) => {
                    worker_tx.send(WorkerMsg::Cmd("exit".to_string())).unwrap();
                    match shell_rx.recv().unwrap() {
                        ShellMsg::Quit(n) => {
                            exit_val = n;
                            break;
                        }
                        _ => {
                            panic!("failed to exit")
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Zerosh: reading error\n {e}");
                    exit_val = -1;
                    break;
                }
            }
        }
        if let Err(e) = rl.save_history(&self.logfile) {
            eprintln!("Zerosh: failed to write history file: {e}");
        }
        exit(exit_val);
    }
}

fn spawn_sig_handler(tx: Sender<WorkerMsg>) -> Result<(), DynError> {
    let mut signals = Signals::new(&[SIGINT, SIGTSTP, SIGCHLD])?;
    thread::spawn(move || {
        for sig in signals.forever() {
            tx.send(WorkerMsg::Signal(sig)).unwrap();
        }
    });
    Ok(())
}

fn syscall<F, T>(f: F) -> Result<T, nix::Error>
where
    F: Fn() -> Result<T, nix::Error>,
{
    loop {
        match f() {
            Err(nix::Error::EINTR) => (),
            result => return result,
        }
    }
}

fn parse_cmd(line: &str) -> CmdResult {
    let commands: Vec<&str> = line.split('|').filter(|x| !x.is_empty()).collect();
    if commands.is_empty() {
        return Err("blank command".into());
    }

    let mut result = Vec::new();

    for command in commands {
        let mut parts = command.split_whitespace();
        let filename = parts.next().ok_or("blank Command")?;
        let args: Vec<&str> = parts.collect();
        result.push((filename, args));
    }

    Ok(result)
}

fn report_err(shell_tx: &SyncSender<ShellMsg>, message: &str, exit_val: i32) {
    eprintln!("{message}");
    shell_tx.send(ShellMsg::Continue(exit_val));
}
