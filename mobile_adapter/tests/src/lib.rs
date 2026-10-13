//! Acceptance contracts execute production code; they do not model the renderer.
pub mod contracts;
mod scheduler;
pub fn assumptions() -> &'static serde_json::Value {
    static DECLARED: OnceLock<serde_json::Value> = OnceLock::new();
    DECLARED.get_or_init(|| {
        serde_json::from_str(include_str!("../assumptions.json"))
            .expect("Valid assumption manifest")
    })
}
use serde_json::Value;
use std::{
    collections::{BTreeMap, VecDeque},
    fs::{self, File, OpenOptions},
    ops::Deref,
    path::PathBuf,
    process::{Command, Stdio},
    sync::{Arc, Mutex, OnceLock},
    time::{Duration, Instant, SystemTime, UNIX_EPOCH},
};

static SCHEDULER: OnceLock<scheduler::Scheduler> = OnceLock::new();
static CASES: Mutex<BTreeMap<String, Arc<Mutex<bool>>>> = Mutex::new(BTreeMap::new());
// Keep two parsed scenes rather than reparsing the same large JSON for every
// contract or retaining every scenario in memory on a 16 GiB developer machine.
static LOADED: Mutex<VecDeque<(String, Arc<Value>)>> = Mutex::new(VecDeque::new());

pub fn performance_case(case: &str) -> bool {
    case == "build"
        || assumptions()["performance_cases"]
            .as_array()
            .expect("Declared exclusive performance cases")
            .iter()
            .any(|name| name == case)
}

pub struct Evidence {
    value: Arc<Value>,
    _permit: scheduler::Permit<'static>,
}

impl Deref for Evidence {
    type Target = Value;
    fn deref(&self) -> &Value {
        &self.value
    }
}

fn execution_lane() -> &'static scheduler::Scheduler {
    SCHEDULER.get_or_init(|| {
        let limit = std::env::var("MOBILE_ACCEPTANCE_JOBS")
            .map(|s| {
                s.parse::<usize>()
                    .expect("MOBILE_ACCEPTANCE_JOBS must be a positive integer")
            })
            .unwrap_or(2);
        scheduler::Scheduler::new(limit)
    })
}
struct RunOutput {
    path: PathBuf,
    // Hold a shared OS lock for this entire test executable, including time
    // between backends. A janitor in another run must not reclaim its evidence.
    _active: File,
}
static OUTPUT: OnceLock<RunOutput> = OnceLock::new();

pub fn output() -> &'static PathBuf {
    &OUTPUT
        .get_or_init(|| {
            let path = std::env::var_os("MOBILE_ACCEPTANCE_OUTPUT")
                .map(PathBuf::from)
                .unwrap_or_else(|| {
                    std::env::var_os("MOBILE_TEST_CACHE")
                        .map(PathBuf::from)
                        .unwrap_or_else(|| PathBuf::from("/Volumes/Storage/Cache/mobileadapter"))
                        .join(format!(
                            "acceptance-suite-{}-{}",
                            SystemTime::now()
                                .duration_since(UNIX_EPOCH)
                                .unwrap()
                                .as_secs(),
                            std::process::id()
                        ))
                });
            fs::create_dir_all(&path).expect("Create acceptance artifact directory");
            let active = OpenOptions::new()
                .create(true)
                .append(true)
                .open(path.join(".active.lock"))
                .expect("Open active-run lease");
            #[cfg(unix)]
            {
                use std::os::fd::AsRawFd;
                unsafe extern "C" {
                    fn flock(fd: i32, operation: i32) -> i32;
                }
                // POSIX flock LOCK_SH. The descriptor remains open until process exit.
                assert_eq!(
                    unsafe { flock(active.as_raw_fd(), 1) },
                    0,
                    "Lock active acceptance run"
                );
            }
            RunOutput {
                path,
                _active: active,
            }
        })
        .path
}

/// Functional cases share bounded capacity; timing cases take the entire lane.
/// The permit also covers JSON decoding and assertions until Evidence is dropped.
/// A per-case lock ensures concurrent contracts never execute a scenario twice.
pub fn evidence(case: &str) -> Evidence {
    let cell = CASES
        .lock()
        .unwrap_or_else(|p| p.into_inner())
        .entry(case.to_owned())
        .or_insert_with(|| Arc::new(Mutex::new(false)))
        .clone();
    let mut complete = cell.lock().unwrap_or_else(|p| p.into_inner());
    let permit = execution_lane().enter(performance_case(case));
    eprintln!(
        "Acceptance scenario {case} [{}]: {}",
        if performance_case(case) {
            "exclusive timing"
        } else {
            "parallel functional"
        },
        output().display()
    );
    let path = output().join(format!("{case}.json"));
    let error_path = output().join(format!("{case}.error"));
    assert!(
        !error_path.exists(),
        "Previous execution of {case} failed: {}. Artifacts: {}",
        fs::read_to_string(&error_path).unwrap_or_default(),
        output().display()
    );
    if !*complete {
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
                *complete = true;
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
    let cached = {
        let mut loaded = LOADED.lock().unwrap_or_else(|p| p.into_inner());
        loaded
            .iter()
            .position(|(name, _)| name == case)
            .map(|index| {
                let item = loaded.remove(index).unwrap();
                let value = item.1.clone();
                loaded.push_back(item);
                value
            })
    };
    let value = cached.unwrap_or_else(|| {
        let value: Arc<Value> = Arc::new(
            serde_json::from_slice(&fs::read(&path).expect("Scenario evidence missing"))
                .expect("Invalid scenario evidence"),
        );
        let mut loaded = LOADED.lock().unwrap_or_else(|p| p.into_inner());
        loaded.push_back((case.to_owned(), value.clone()));
        while loaded.len() > 2 {
            loaded.pop_front();
        }
        value
    });
    Evidence {
        value,
        _permit: permit,
    }
}
