//! Relative timing contract: existing native gaps are part of the reference.
pub fn cadence_regression(native: &[u64], multiplayer: &[u64]) -> Option<String> {
    if native.is_empty() || native.len() != multiplayer.len() {
        return Some("Missing or differently sized native/multiplayer frame windows".into());
    }
    let (mut native_total, mut multiplayer_total) = (0, 0);
    for (frame, (&n, &m)) in native.iter().zip(multiplayer).enumerate() {
        native_total += n;
        multiplayer_total += m;
        if multiplayer_total < native_total {
            return Some(format!("Additional world-update delay at relative frame {frame}: native={native_total}, multiplayer={multiplayer_total}"));
        }
        if m > 1 && m > n {
            return Some(format!(
                "Additional duplicate world updates at relative frame {frame}"
            ));
        }
    }
    None
}
