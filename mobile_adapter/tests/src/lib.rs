//! Acceptance contracts execute production code; they do not model the renderer.
pub mod contracts;
pub fn assumptions() -> &'static serde_json::Value {
    static DECLARED: OnceLock<serde_json::Value> = OnceLock::new();
    DECLARED.get_or_init(|| {
        serde_json::from_str(include_str!("../assumptions.json"))
            .expect("Valid assumption manifest")
    })
}
use serde_json::Value;
use std::{
    fs,
    path::PathBuf,
    process::{Command, Stdio},
    sync::{Mutex, OnceLock},
    time::{Duration, Instant},
};

static EXECUTION: Mutex<()> = Mutex::new(());
static OUTPUT: OnceLock<PathBuf> = OnceLock::new();

pub fn output() -> &'static PathBuf {
    OUTPUT.get_or_init(|| {
        let path = std::env::var_os("MOBILE_ACCEPTANCE_OUTPUT")
            .map(PathBuf::from)
            .unwrap_or_else(|| {
                std::env::temp_dir().join(format!("mobile-acceptance-{}", std::process::id()))
            });
        fs::create_dir_all(&path).expect("Create acceptance artifact directory");
        path
    })
}

/// One process group per scenario; serialized host execution avoids CPU contention.
/// Results are immutable within one run, so separate contracts can inspect the
/// same expensive execution without depending on another test's mutation.
pub fn evidence(case: &str) -> Value {
    let _guard = EXECUTION.lock().unwrap_or_else(|p| p.into_inner());
    eprintln!("Acceptance scenario {case}: {}", output().display());
    let path = output().join(format!("{case}.json"));
    let error_path = output().join(format!("{case}.error"));
    assert!(
        !error_path.exists(),
        "Previous execution of {case} failed: {}. Artifacts: {}",
        fs::read_to_string(&error_path).unwrap_or_default(),
        output().display()
    );
    {
        let log = fs::File::create(output().join(format!("{case}.log"))).unwrap();
        let mut cmd = Command::new("python3");
        cmd.arg(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("support/runtime.py"))
            .arg("--case")
            .arg(case)
            .arg("--output")
            .arg(output())
            .stdout(Stdio::from(log.try_clone().unwrap()))
            .stderr(Stdio::from(log));
        // Descendants are killed together if the core or transport hangs.
        #[cfg(unix)]
        {
            use std::os::unix::process::CommandExt;
            cmd.process_group(0);
        }
        let mut child = cmd.spawn().expect("Start scenario backend");
        let started = Instant::now();
        loop {
            if let Some(status) = child.try_wait().unwrap() {
                assert!(
                    status.success(),
                    "Scenario setup/execution failed: {case}. {} Artifacts: {}",
                    fs::read_to_string(&error_path).unwrap_or_default(),
                    output().display()
                );
                break;
            }
            if started.elapsed() > Duration::from_secs(600) {
                let _ = Command::new("kill")
                    .args(["-TERM", "--", &format!("-{}", child.id())])
                    .status();
                let _ = child.kill();
                let _ = child.wait();
                panic!(
                    "Scenario deadline exceeded: {case}. Artifacts: {}",
                    output().display()
                );
            }
            std::thread::sleep(Duration::from_millis(50));
        }
    }
    serde_json::from_slice(&fs::read(&path).expect("Scenario evidence missing"))
        .expect("Invalid scenario evidence")
}
