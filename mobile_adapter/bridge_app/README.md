# Polished Crystal Bridge

Flutter presents local or Internet lobby selection and the connected emulator
list. Native Rust owns TCP transport, grouped sessions and the embedded lobby.
The same socket-independent room owner is used by the native hosted simulator
and the WASM Cloudflare Durable Object. Dart never validates or relays game packets.

From the repository root, build the native macOS app with:

```sh
python3 mobile_adapter/tools/build_bridge_app.py --flutter /path/to/flutter --debug
```

Add `--run` to open it. The app listens on 127.0.0.1:17423 after selecting Start.
Do not run it alongside the current manual-development server on that port.
Local mode needs no server. Internet mode uses one WebSocket per Bridge for
multiple separate emulator players. In the local prototype, start the hosted
model with:

```sh
cargo run --locked --manifest-path mobile_adapter/server/Cargo.toml -p mobile-bridge --bin mobile-hosted -- --bind 127.0.0.1:8787 --test-one-way-delay-ms 50
```

The delay is 50ms in each direction, adding approximately 100ms RTT. Delayed
packets use bounded due-time queues, not one sleep per packet. Production clients
cannot choose this setting. Configure the Flutter app with ws://127.0.0.1:8787
and the same lobby code on both Bridges. Public origins must use WSS.
The initial room codes are canonical nonzero 16-bit IDs. This is not a secure
invite/password design or proof of collision-free lobby creation.

Embedded local mode publishes dirty state immediately through the shared Rust
owner. Internet mode coalesces upstream presence and authority pushes at a shared
33ms interval (approximately 30Hz),
retaining only the latest state. Frequent local ROM polls never become cloud
requests. Transport modes use identical validation, capacity and identity rules.

Test commands:

```sh
cargo test --locked --manifest-path mobile_adapter/server/Cargo.toml --workspace
cd mobile_adapter/bridge_app
flutter test
MOBILE_BRIDGE_LIBRARY=/absolute/path/to/server/target/debug/libmobile_bridge.dylib flutter test test/native_bridge_test.dart
```

The ordinary Flutter tests require no emulator. The FFI test uses an ephemeral
loopback port, starts the actual Rust listener and verifies port release.
MOBILE_BRIDGE_LIBRARY overrides the bundled library for development; the app
otherwise loads it from its platform bundle. MOBILE_BRIDGE_PORT provides a test
port without changing the stable default. The current macOS bundle is built and
the FFI boundary is tested. Android/iOS/Linux/Windows packaging and background
service lifecycle are future checks, not completed platform support. iOS native
Rust compilation alone does not guarantee background networking while a separate
emulator runs in the foreground.
