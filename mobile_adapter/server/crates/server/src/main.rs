use mobile_server::{serve_connection, TestFault};
use std::io::{self, Write};
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::net::TcpListener;
use tokio::sync::Semaphore;

fn main() -> io::Result<()> {
    let mut bind: SocketAddr = "127.0.0.1:17423".parse().unwrap();
    let mut fault = TestFault::None;
    let mut fault_count = u64::MAX;
    let mut args = std::env::args().skip(1);
    while let Some(option) = args.next() {
        let value = args
            .next()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "missing option value"))?;
        match option.as_str() {
            "--test-fault-count" => {
                fault_count = value.parse().map_err(|_| {
                    io::Error::new(io::ErrorKind::InvalidInput, "invalid fault count")
                })?
            }
            "--bind" => {
                bind = value.parse().map_err(|_| {
                    io::Error::new(io::ErrorKind::InvalidInput, "invalid bind address")
                })?
            }
            "--test-fault" => {
                fault = match value.as_str() {
                    "split" => TestFault::Split,
                    "bad-magic" => TestFault::BadMagic,
                    "silent" => TestFault::Silent,
                    "close" => TestFault::Close,
                    "oversize" => TestFault::Oversize,
                    "truncated" => TestFault::Truncated,
                    _ => {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidInput,
                            "invalid test fault",
                        ))
                    }
                }
            }
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "unknown option",
                ))
            }
        }
    }
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()?
        .block_on(async {
            let listener = TcpListener::bind(bind).await?;
            println!("READY {}", listener.local_addr()?);
            io::stdout().flush()?;
            let permits = Arc::new(Semaphore::new(16));
            let mut sequence = 0u64;
            loop {
                let (stream, _) = listener.accept().await?;
                let permit = match permits.clone().try_acquire_owned() {
                    Ok(permit) => permit,
                    Err(_) => continue,
                };
                stream.set_nodelay(true)?;
                sequence += 1;
                let session = sequence;
                let connection_fault = if session <= fault_count {
                    fault
                } else {
                    TestFault::None
                };
                tokio::spawn(async move {
                    let _permit = permit;
                    if let Err(error) = serve_connection(stream, session, connection_fault).await {
                        eprintln!("SESSION_ERROR session={session} error={error}");
                    }
                });
            }
        })
}
