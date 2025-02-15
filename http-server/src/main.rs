use std::{
    io::{prelude::*, BufReader, BufWriter},
    net::{TcpListener, TcpStream},
    sync::{
        mpsc::{channel, Receiver, Sender},
        Arc, Mutex,
    },
    thread,
    time::Duration,
};

// Job's Type, holds an executable closure as box.
type Job = Box<dyn FnOnce() + Send + 'static>;

pub struct ThreadPool {
    workers: Vec<Worker>,
    sender: Sender<Job>,
}

impl ThreadPool {
    pub fn new(size: usize) -> ThreadPool {
        assert!(size > 0);

        let (sender, receiver) = channel();

        let mut workers = Vec::with_capacity(size);
        let receiver = Arc::new(Mutex::new(receiver));

        for id in 0..size {
            println!("id: {}", id);
            workers.push(Worker::new(id, Arc::clone(&receiver)));
        }

        ThreadPool { workers, sender }
    }

    pub fn execute<F>(&self, f: F)
    where
        F: FnOnce() + Send + 'static,
    {
        let job = Box::new(f);
        self.sender.send(job).unwrap();
    }
}

struct Worker {
    id: usize,
    thread: Option<thread::JoinHandle<()>>,
}

impl Worker {
    fn new(id: usize, receiver: Arc<Mutex<Receiver<Job>>>) -> Worker {
        let thread = thread::spawn(move || loop {
            // blocking thread and wait data arrival
            let lock = receiver.lock().unwrap().recv();
            match lock {
                Ok(job) => {
                    println!("Worker {}: executing job...", id);
                    job();
                }
                Err(_) => {
                    println!("Worker {}: channel has disconnected, quitting", id);
                    break;
                }
            }
        });

        Worker {
            id,
            thread: Some(thread),
        }
    }
}

fn main() {
    let listener = TcpListener::bind("127.0.0.1:10000").unwrap();

    let pool = ThreadPool::new(10);
    println!("Server listening on 127.0.0.1:10000");

    while let Ok((stream, _)) = listener.accept() {
        pool.execute(move || {
            handle_connection(stream);
        });
    }
}

fn handle_connection(stream: TcpStream) {
    let mut request_line = String::new();
    let mut reader = BufReader::new(&stream);
    reader.read_line(&mut request_line).unwrap();

    let stream1 = stream.try_clone().unwrap();
    let mut writer = BufWriter::new(&stream1);

    // some heavy process
    thread::sleep(Duration::from_secs(3));

    let body = "Hello";

    let response = format!(
        "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nContent-Type: text/plain\r\n\r\n{}",
        body.len(),
        body
    );

    writer.write_all(response.as_bytes()).unwrap();
    writer.flush().unwrap();
}
