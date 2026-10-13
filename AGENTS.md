# Repository agent instructions

- Use `gh`, not the GitHub plugin. Do not prefix branches with `codex`.
- Write code, documentation, issues and commits in English. Speak German with
  the user unless they switch languages. Do not use em dashes.
- Commit completed changes locally unless the user requests otherwise.
- Keep Mobile Adapter assembly in `mobile_adapter/rom`, with only minimal
  conditional integration elsewhere. Preserve disabled-ROM byte identity.
- Keep at least 10 GiB free on the Storage volume. Check before substantial
  builds/test runs and after artifact creation. If available space falls below
  this threshold, remove obsolete generated test-run directories, oldest first.
  Never delete active runs, repository/worktree files, user saves or toolchains
  such as RGBDS and the supplied MBC30 emulator. Restrict automatic cleanup to
  identified generated test runs in the configured test cache. If no safe
  deletion can restore the reserve, report the condition and stop generating
  large artifacts.
- Functional acceptance scenarios may run at 400% concurrently on isolated
  ports/workspaces, using observed states to trigger actions. Retain real-time
  execution for wall-clock networking and performance measurements.
  Performance comparisons must run exclusively, with no other acceptance
  scenarios competing for CPU. Keep individual contract results and do not
  relax acceptance thresholds to accelerate tests.

- Verify each change with the relevant tests or focused checks, demonstrating
  improvement or no regression against the applicable baseline. During iteration,
  run only the affected test cases; do not run the entire suite by default or
  assume unrelated cases need repeating. Run the complete suite once at the end
  of a substantial work step, especially before declaring a milestone complete.
  Keep existing failures visible and do not weaken assertions to obtain a pass.
  Documentation-only changes need a content review, not unrelated game tests.
