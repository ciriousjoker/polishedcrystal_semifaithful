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
- Acceptance tests must retain real-time multiplayer execution. Independent
  functional scenarios may run concurrently on isolated ports/workspaces.
  Performance comparisons must run exclusively, with no other acceptance
  scenarios competing for CPU. Keep individual contract results and do not
  relax acceptance thresholds to accelerate tests.
