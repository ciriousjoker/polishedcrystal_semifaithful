//! Small native ABI for the Flutter shell. No game memory crosses this boundary.
use crate::{Config, Mode, SharedStatus, Status};
use std::{
    ffi::CString,
    io,
    sync::{Mutex, OnceLock},
    thread::JoinHandle,
};
use tokio::sync::watch;

struct Running {
    stop: watch::Sender<bool>,
    thread: JoinHandle<()>,
    status: SharedStatus,
}
static RUNNING: OnceLock<Mutex<Option<Running>>> = OnceLock::new();
static SNAPSHOT: OnceLock<Mutex<CString>> = OnceLock::new();

fn state() -> &'static Mutex<Option<Running>> {
    RUNNING.get_or_init(|| Mutex::new(None))
}

fn start(port: u16, room: u16, mode: Mode) -> io::Result<()> {
    stop();
    if port == 0 || room == 0 {
        return Err(io::Error::other("Invalid port or room"));
    }
    let config = Config {
        // Development launchers may expose the household Bridge on a selected
        // LAN interface. Standalone app launches remain loopback-only.
        bind: (
            std::env::var("MOBILE_BRIDGE_BIND_ADDRESS")
                .unwrap_or_else(|_| "127.0.0.1".into())
                .parse::<std::net::Ipv4Addr>()
                .map_err(io::Error::other)?,
            port,
        )
            .into(),
        lobby: room,
        mode,
    };
    let status = std::sync::Arc::new(Mutex::new(Status::default()));
    let view = status.clone();
    let (tx, rx) = watch::channel(false);
    let (ready_tx, ready_rx) = std::sync::mpsc::sync_channel(1);
    let thread = std::thread::spawn(move || {
        let result = (|| -> io::Result<()> {
            let runtime = tokio::runtime::Builder::new_multi_thread()
                .worker_threads(2)
                .enable_all()
                .build()?;
            runtime.block_on(async {
                let listener = tokio::net::TcpListener::bind(config.bind).await;
                match listener {
                    Ok(listener) => {
                        let _ = ready_tx.send(Ok(()));
                        crate::serve(listener, config, view.clone(), rx).await
                    }
                    Err(error) => {
                        let _ = ready_tx.send(Err(error.to_string()));
                        Err(error)
                    }
                }
            })
        })();
        if let Err(error) = result {
            if let Ok(mut s) = view.lock() {
                s.error = Some(error.to_string());
            }
        }
    });
    match ready_rx.recv_timeout(std::time::Duration::from_secs(5)) {
        Ok(Ok(())) => {
            *state()
                .lock()
                .map_err(|_| io::Error::other("Bridge lock"))? = Some(Running {
                stop: tx,
                thread,
                status,
            });
            Ok(())
        }
        result => {
            let _ = tx.send(true);
            let _ = thread.join();
            Err(io::Error::other(format!("Bridge start failed: {result:?}")))
        }
    }
}

fn stop() {
    if let Ok(mut running) = state().lock() {
        if let Some(running) = running.take() {
            let _ = running.stop.send(true);
            let _ = running.thread.join();
        }
    }
}

#[no_mangle]
pub extern "C" fn mobile_bridge_start_local(port: u16, room: u16) -> i32 {
    if start(port, room, Mode::Local).is_ok() {
        0
    } else {
        1
    }
}

/// Caller passes a UTF-8 origin as a bounded byte slice. The slice is copied
/// synchronously; Rust never retains caller-owned memory.
///
/// # Safety
/// `origin` must point to `len` readable bytes for the duration of this call.
/// The caller must not mutate or free the slice until the call returns.
#[no_mangle]
pub unsafe extern "C" fn mobile_bridge_start_internet(
    port: u16,
    room: u16,
    origin: *const u8,
    len: usize,
) -> i32 {
    if origin.is_null() || len == 0 || len > 2048 {
        return 1;
    }
    let bytes = unsafe { std::slice::from_raw_parts(origin, len) };
    let Ok(origin) = std::str::from_utf8(bytes) else {
        return 1;
    };
    if crate::validate_origin(origin).is_err() {
        return 1;
    }
    if start(
        port,
        room,
        Mode::Internet {
            origin: origin.into(),
        },
    )
    .is_ok()
    {
        0
    } else {
        1
    }
}

#[no_mangle]
pub extern "C" fn mobile_bridge_stop() {
    stop();
}

/// Returned string remains valid until the next snapshot call. The UI copies
/// immediately on its single isolate. No allocation is transferred to Dart.
#[no_mangle]
pub extern "C" fn mobile_bridge_status() -> *const std::ffi::c_char {
    let status = state()
        .lock()
        .ok()
        .and_then(|r| {
            r.as_ref()
                .and_then(|r| r.status.lock().ok().map(|s| s.clone()))
        })
        .unwrap_or_default();
    let bytes = serde_json::to_string(&status).unwrap_or_else(|_| "{}".into());
    let cache = SNAPSHOT.get_or_init(|| Mutex::new(CString::new("{}").expect("Literal")));
    let Ok(mut cache) = cache.lock() else {
        return std::ptr::null();
    };
    *cache = CString::new(bytes).unwrap_or_else(|_| CString::new("{}").expect("Literal"));
    cache.as_ptr()
}
