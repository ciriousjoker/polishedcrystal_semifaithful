//! Shared admission limits; host traffic protection remains outside this model.
pub const MESSAGES_PER_SECOND: u16 = 1024;
pub const GROUP_IDLE_MS: u64 = 10_000;

#[derive(Clone)]
#[cfg_attr(feature = "checkpoint", derive(serde::Serialize, serde::Deserialize))]
pub struct TrafficBudget {
    window: u64,
    count: u16,
    last_seen: u64,
}
impl TrafficBudget {
    pub fn new(now: u64) -> Self {
        Self {
            window: now,
            count: 0,
            last_seen: now,
        }
    }
    pub fn admit(&mut self, now: u64) -> bool {
        if now.saturating_sub(self.window) >= 1000 {
            self.window = now;
            self.count = 0;
        }
        if self.count >= MESSAGES_PER_SECOND {
            return false;
        }
        self.count += 1;
        self.last_seen = self.last_seen.max(now);
        true
    }
    pub fn idle(&self, now: u64) -> bool {
        now.saturating_sub(self.last_seen) > GROUP_IDLE_MS
    }
    pub fn validate_checkpoint(&self) -> bool {
        self.count <= MESSAGES_PER_SECOND && self.window <= self.last_seen
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn one_aggressive_group_exhausts_only_its_budget() {
        let mut bad = TrafficBudget::new(5000);
        let mut healthy = TrafficBudget::new(5000);
        for _ in 0..MESSAGES_PER_SECOND {
            assert!(bad.admit(5000));
        }
        for now in [0, 4999, 5000, 5999] {
            assert!(!bad.admit(now));
        }
        assert!(healthy.admit(5999));
        assert!(bad.admit(6000));
        assert!(bad.validate_checkpoint());
        assert!(!bad.idle(16_000));
        assert!(bad.idle(16_001));
    }
}
