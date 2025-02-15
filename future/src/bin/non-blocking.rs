use nix::fcntl::{fcntl, FcntlArg, OFlag};
use nix::sys::epoll::{
    epoll_create1, epoll_ctl, epoll_wait, EpollCreateFlags, EpollEvent, EpollFlags, EpollOp,
};
use std::collections::HashMap;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::os::unix::io::{AsRawFd, RawFd};

const BUFFER_SIZE: usize = 1024;
const ADDRESS: &str = "127.0.0.1:7878";

/// 指定したファイルディスクリプタを非ブロッキングモードに設定する
fn set_nonblocking(fd: RawFd) -> nix::Result<()> {
    let flags = fcntl(fd, FcntlArg::F_GETFL)?;
    let new_flags = OFlag::from_bits_truncate(flags) | OFlag::O_NONBLOCK;
    fcntl(fd, FcntlArg::F_SETFL(new_flags))?;
    Ok(())
}

/// 各接続の状態を表す
enum ConnectionState {
    /// クライアントからのデータを待っている状態
    Reading,
    /// クライアントから受け取ったメッセージに基づいてレスポンス送信中の状態
    Writing,
}

/// サーバー本体。listener と、接続ごとに TcpStream と状態を保持する HashMap を持つ
struct Server {
    listener: TcpListener,
    connections: HashMap<RawFd, (TcpStream, ConnectionState)>,
}

impl Server {
    /// 指定アドレスでサーバーを生成。listener を非ブロッキングに設定する
    fn new(addr: &str) -> std::io::Result<Self> {
        let listener = TcpListener::bind(addr)?;
        set_nonblocking(listener.as_raw_fd()).expect("failed to set nonblocking");
        Ok(Server {
            listener,
            connections: HashMap::new(),
        })
    }

    /// listener に対して accept を試み、接続があれば非ブロッキングに設定後、HashMap に登録し epoll に読み込みイベントを追加する
    fn accept_connection(&mut self, epoll_fd: RawFd) {
        loop {
            match self.listener.accept() {
                Ok((stream, addr)) => {
                    println!("Connected to {}", addr);
                    let fd = stream.as_raw_fd();
                    set_nonblocking(fd).expect("failed to set nonblocking");
                    self.connections.insert(fd, (stream, ConnectionState::Reading));
                    let mut event = EpollEvent::new(EpollFlags::EPOLLIN, fd as u64);
                    epoll_ctl(epoll_fd, EpollOp::EpollCtlAdd, fd, &mut event)
                        .expect("epoll_ctl add failed");
                }
                Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    // 受付可能な接続はこれ以上ない
                    break;
                }
                Err(e) => {
                    eprintln!("Accept error: {}", e);
                    break;
                }
            }
        }
    }

    /// 接続されたソケットから読み込みを行う。読み込み成功時は状態を Writing に変更し、epoll イベントを EPOLLOUT に切り替える
    fn handle_read(&mut self, fd: RawFd, epoll_fd: RawFd) {
        if let Some((stream, state)) = self.connections.get_mut(&fd) {
            let mut buf = [0u8; BUFFER_SIZE];
            match stream.read(&mut buf) {
                Ok(0) => {
                    println!("Connection closed: fd {}", fd);
                    epoll_ctl(epoll_fd, EpollOp::EpollCtlDel, fd, None)
                        .expect("epoll_ctl del failed");
                    self.connections.remove(&fd);
                }
                Ok(_) => {
                    println!("Received message on fd {}", fd);
                    *state = ConnectionState::Writing;
                    let mut event = EpollEvent::new(EpollFlags::EPOLLOUT, fd as u64);
                    epoll_ctl(epoll_fd, EpollOp::EpollCtlMod, fd, &mut event)
                        .expect("epoll_ctl mod failed");
                }
                Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    // 読み込み可能なデータがない
                }
                Err(e) => {
                    eprintln!("Read error on fd {}: {}", fd, e);
                    epoll_ctl(epoll_fd, EpollOp::EpollCtlDel, fd, None)
                        .expect("epoll_ctl del failed");
                    self.connections.remove(&fd);
                }
            }
        }
    }

    /// ソケットへの書き込みを行う。メッセージをパースしてレスポンス文字列を生成し、送信後は状態を Reading に戻して EPOLLIN イベントに切り替える
    fn handle_write(&mut self, fd: RawFd, epoll_fd: RawFd) {
        if let Some((stream, state)) = self.connections.get_mut(&fd) {
            if let ConnectionState::Writing = state {
                let body = "Hello";
                let response = format!(
                    "HTTP/1.1 200 OK\r\nContent-Length: {}\r\nContent-Type: text/plain\r\n\r\n{}",
                    body.len(),
                    body
                );
                println!("Sending response to fd {}: {}", fd, response.trim());
                match stream.write(response.as_bytes()) {
                    Ok(_) => {
                        *state = ConnectionState::Reading;
                        let mut event = EpollEvent::new(EpollFlags::EPOLLIN, fd as u64);
                        epoll_ctl(epoll_fd, EpollOp::EpollCtlMod, fd, &mut event)
                            .expect("epoll_ctl mod failed");
                    }
                    Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                        // 書き込み可能になるまで待つ
                    }
                    Err(e) => {
                        eprintln!("Write error on fd {}: {}", fd, e);
                        epoll_ctl(epoll_fd, EpollOp::EpollCtlDel, fd, None)
                            .expect("epoll_ctl del failed");
                        self.connections.remove(&fd);
                    }
                }
            }
        }
    }
}

fn main() {
    // サーバー作成
    let mut server = Server::new(ADDRESS).expect("Failed to create server");
    println!("Server listening on {}", ADDRESS);

    // epoll インスタンスの作成
    let epoll_fd = epoll_create1(EpollCreateFlags::empty()).expect("Failed to create epoll");
    // listener 用に EPOLLIN イベントを登録
    let listen_fd = server.listener.as_raw_fd();
    let mut event = EpollEvent::new(EpollFlags::EPOLLIN, listen_fd as u64);
    epoll_ctl(epoll_fd, EpollOp::EpollCtlAdd, listen_fd, &mut event)
        .expect("epoll_ctl add failed");

    let mut events = vec![EpollEvent::empty(); 1024];

    // メインのイベントループ
    loop {
        let nfds = epoll_wait(epoll_fd, &mut events, -1).expect("epoll_wait failed");
        for i in 0..nfds {
            let fd = events[i].data() as RawFd;
            if fd == listen_fd {
                // 新しい接続要求
                server.accept_connection(epoll_fd);
            } else {
                // 既存接続のイベント
                let event_flags = events[i].events();
                if event_flags.contains(EpollFlags::EPOLLIN) {
                    server.handle_read(fd, epoll_fd);
                }
                if event_flags.contains(EpollFlags::EPOLLOUT) {
                    server.handle_write(fd, epoll_fd);
                }
            }
        }
    }
}
