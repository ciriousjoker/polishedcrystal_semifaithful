use mobile_bridge::{Config, Mode, Status};
use std::{
    io,
    sync::{Arc, Mutex},
};

fn main() -> io::Result<()> {
    let mut config = Config {
        bind: "127.0.0.1:17423".parse().map_err(io::Error::other)?,
        lobby: 1,
        mode: Mode::Local,
    };
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        let value = args
            .next()
            .ok_or_else(|| io::Error::other("Missing option value"))?;
        match arg.as_str() {
            "--bind" => config.bind = value.parse().map_err(io::Error::other)?,
            "--lobby" => config.lobby = value.parse().map_err(io::Error::other)?,
            "--upstream" => config.mode = Mode::Internet { origin: value },
            _ => return Err(io::Error::other("Unknown option")),
        }
    }
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()?
        .block_on(async {
            let listener = tokio::net::TcpListener::bind(config.bind).await?;
            let (_stop, rx) = tokio::sync::watch::channel(false);
            mobile_bridge::serve(
                listener,
                config,
                Arc::new(Mutex::new(Status::default())),
                rx,
            )
            .await
        })
}
