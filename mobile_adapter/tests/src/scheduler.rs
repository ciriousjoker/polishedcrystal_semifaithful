use std::sync::{Condvar, Mutex};

#[derive(Default)]
struct State {
    active: usize,
    exclusive: bool,
    waiting_exclusive: usize,
}

pub struct Scheduler {
    limit: usize,
    state: Mutex<State>,
    changed: Condvar,
}

impl Scheduler {
    pub fn new(limit: usize) -> Self {
        assert!(limit > 0);
        Self {
            limit,
            state: Mutex::new(State::default()),
            changed: Condvar::new(),
        }
    }

    pub fn enter(&self, exclusive: bool) -> Permit<'_> {
        let mut state = self.state.lock().unwrap_or_else(|p| p.into_inner());
        if exclusive {
            state.waiting_exclusive += 1;
            self.changed.notify_all();
        }
        while state.exclusive
            || if exclusive {
                state.active != 0
            } else {
                state.active >= self.limit || state.waiting_exclusive != 0
            }
        {
            state = self.changed.wait(state).unwrap_or_else(|p| p.into_inner());
        }
        if exclusive {
            state.waiting_exclusive -= 1;
            state.exclusive = true;
        }
        state.active += 1;
        Permit {
            scheduler: self,
            exclusive,
        }
    }
}

pub struct Permit<'a> {
    scheduler: &'a Scheduler,
    exclusive: bool,
}

impl Drop for Permit<'_> {
    fn drop(&mut self) {
        let mut state = self
            .scheduler
            .state
            .lock()
            .unwrap_or_else(|p| p.into_inner());
        state.active -= 1;
        if self.exclusive {
            state.exclusive = false;
        }
        self.scheduler.changed.notify_all();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{
        sync::{mpsc, Arc},
        thread,
        time::Duration,
    };

    #[test]
    fn waiting_performance_run_excludes_and_precedes_functional_runs() {
        let scheduler = Arc::new(Scheduler::new(2));
        let first = scheduler.enter(false);
        let (entered, observed) = mpsc::channel();
        let (release, wait) = mpsc::channel();
        let copy = scheduler.clone();
        let report = entered.clone();
        let writer = thread::spawn(move || {
            let _permit = copy.enter(true);
            report.send("performance").unwrap();
            wait.recv().unwrap();
        });
        let mut state = scheduler.state.lock().unwrap();
        while state.waiting_exclusive == 0 {
            state = scheduler.changed.wait(state).unwrap();
        }
        drop(state);
        let copy = scheduler.clone();
        let reader = thread::spawn(move || {
            let _permit = copy.enter(false);
            entered.send("functional").unwrap();
        });
        drop(first);
        assert_eq!(
            observed.recv_timeout(Duration::from_secs(5)).unwrap(),
            "performance"
        );
        assert!(observed.try_recv().is_err());
        release.send(()).unwrap();
        assert_eq!(
            observed.recv_timeout(Duration::from_secs(5)).unwrap(),
            "functional"
        );
        writer.join().unwrap();
        reader.join().unwrap();
    }

    #[test]
    fn functional_runs_share_capacity_and_unwinding_releases_it() {
        let scheduler = Arc::new(Scheduler::new(2));
        let first = scheduler.enter(false);
        let second = scheduler.enter(false);
        let (sent, received) = mpsc::channel();
        let copy = scheduler.clone();
        let worker = thread::spawn(move || {
            let _permit = copy.enter(false);
            sent.send(()).unwrap();
        });
        assert!(received.try_recv().is_err());
        drop(first);
        received.recv_timeout(Duration::from_secs(5)).unwrap();
        drop(second);
        worker.join().unwrap();
        let _ = std::panic::catch_unwind(|| {
            let _permit = scheduler.enter(true);
            panic!("Deliberate failing contract");
        });
        let _permit = scheduler.enter(false);
    }
}
