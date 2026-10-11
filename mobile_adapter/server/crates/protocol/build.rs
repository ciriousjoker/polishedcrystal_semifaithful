//! Read the authoritative assembly capacity without a second hand-maintained value.
use std::{env, fs, path::PathBuf};
fn main() {
    let source =
        PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").unwrap()).join("../../../rom/capacity.asm");
    println!("cargo:rerun-if-changed={}", source.display());
    let text = fs::read_to_string(source).expect("Read lobby capacity");
    let values: Vec<u8> = text
        .lines()
        .filter_map(|line| {
            line.strip_prefix("DEF MOBILE_CAPACITY EQU ")
                .map(|value| value.parse().expect("Integer lobby capacity"))
        })
        .collect();
    assert_eq!(
        values.len(),
        1,
        "One authoritative capacity declaration required"
    );
    let capacity = values[0];
    assert!(
        (2..=7).contains(&capacity),
        "Capacity exceeds bounded roster payload"
    );
    fs::write(
        PathBuf::from(env::var_os("OUT_DIR").unwrap()).join("capacity.rs"),
        format!("pub const CAPACITY: u8 = {capacity};\n"),
    )
    .expect("Write capacity constant");
}
