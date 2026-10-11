use mobile_server::{serve_session, Service, TestFault};
use std::io::{self, Write};
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::net::TcpListener;
use tokio::sync::Semaphore;

fn main() -> io::Result<()> {
    let mut bind: SocketAddr = "127.0.0.1:17423".parse().unwrap();
    let mut fault = TestFault::None;
    let mut fault_count = u64::MAX;
    let mut test_delay_ms = 0u64;
    let mut args = std::env::args().skip(1);
    while let Some(option) = args.next() {
        let value = args
            .next()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "missing option value"))?;
        match option.as_str() {
            "--test-one-way-delay-ms" => {
                test_delay_ms = value.parse().map_err(|_| {
                    io::Error::new(io::ErrorKind::InvalidInput, "invalid test delay")
                })?;
                if test_delay_ms > 500 {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "test delay exceeds 500 ms",
                    ));
                }
            }
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
            let service = Arc::new(Service::with_test_delay(std::time::Duration::from_millis(
                test_delay_ms,
            )));
            let mut sequence = 0u64;
            loop {
                let (stream, peer) = listener.accept().await?;
                let permit = match permits.clone().try_acquire_owned() {
                    Ok(permit) => permit,
                    Err(_) => {
                        println!("CLIENT_REJECTED peer={peer} reason=capacity");
                        continue;
                    }
                };
                stream.set_nodelay(true)?;
                sequence += 1;
                let session = sequence;
                println!("CLIENT_CONNECTED session={session} peer={peer}");
                let connection_fault = if session <= fault_count {
                    fault
                } else {
                    TestFault::None
                };
                let service = service.clone();
                tokio::spawn(async move {
                    let _permit = permit;
                    match serve_session(stream, session, connection_fault, service).await {
                        Ok(()) => println!(
                            "CLIENT_DISCONNECTED session={session} peer={peer} reason=closed"
                        ),
                        Err(error) => {
                            eprintln!("SESSION_ERROR session={session} peer={peer} error={error}");
                            println!(
                                "CLIENT_DISCONNECTED session={session} peer={peer} reason=error"
                            );
                        }
                    }
                });
            }
        })
}
