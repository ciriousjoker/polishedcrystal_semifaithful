use std::{io, time::Duration};
fn main() -> io::Result<()> {
    let mut bind = "127.0.0.1:8787".to_owned();
    let mut delay = 0u64;
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        let value = args
            .next()
            .ok_or_else(|| io::Error::other("Missing option value"))?;
        match arg.as_str() {
            "--bind" => bind = value,
            "--test-one-way-delay-ms" => delay = value.parse().map_err(io::Error::other)?,
            _ => return Err(io::Error::other("Unknown option")),
        }
    }
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()?
        .block_on(async {
            let listener = tokio::net::TcpListener::bind(bind).await?;
            let (_stop, rx) = tokio::sync::watch::channel(false);
            mobile_bridge::hosted::serve(listener, Duration::from_millis(delay), rx).await
        })
}
