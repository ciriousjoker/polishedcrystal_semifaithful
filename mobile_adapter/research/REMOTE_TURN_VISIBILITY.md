# Remote turn visibility regression

## Cause and correction

The normal engine marks a temporary actor's facing `STANDING` while handling
player events. The adapter preserves accepted remote facings so menus retain
frozen figures. That same path runs during `PLAYEREVENT_JOYCHANGEFACING`.

An offscreen or waiting lease previously hid only its facing. Its flags still
said it was visible, so a turn could restore the cached facing and submit its
old position to hardware OAM. With two actual Game Corner cores, the old ROM
repeatedly revived the figure during facing changes. The regression fails at
frame 3934, after a genuine absent presentation. The first four-player test,
which started turns only after full map cleanup, missed this defect. The final
scenario starts turns during movement and continues through the actual exit.

`MobileNativePrepare.hide_waiting` and `MobileNativeReleaseLease.hide` now set
`INVISIBLE_F` before setting `STANDING`. Existing menu preservation respects that
flag. Existing successful presentation clears it, and original NPC restoration
copies the original saved object. All changes remain in `mobile_adapter/rom`;
there is no new foreground hook, interrupt work, RAM field or save field.

The change costs twelve bytes in bank 128. Total feature sections occupy 10,958
upper bytes. Net lower integration remains 174 bytes, with 13,787 lower bytes
free. The disabled 2 MiB build retains SHA-256
`314001ec5a2dde0211d65211b7c3311c3f9d0e430a3d7de3a1b567b6db754b1e`.
The corrected enabled 4 MiB build is
`73c113958b7b5207efeabe9c7141bcb3749fed8db9c640f08cefb207254308ed`.

## Reproduction

```sh
python3 mobile_adapter/tools/verify_milestone2.py \
  --output /Volumes/Storage/Cache/mobileadapter/turn-visibility-acceptance \
  --port 17860 --cases warp-turn,start-menu,rejoin,cadence,save-caches
```

Choose a fresh output directory and unused test port range. The verifier runs
`warp-turn` with two and four players, using normal batteries and button inputs.
One player walks down the Game Corner aisle and out the actual door. Remaining
players receive alternating one-frame left/right input, which exercises native
turns without moving their positions. The observer checks complete hardware OAM
quad groups, including figures partly intersecting the viewport. It requires a
visible-to-hidden transition, then rejects any revived hidden figure. After
map departure it also checks the map mismatch and absence through repeated turns.

The immutable per-run ROM differs from the production ROM only in test endpoint
port and global header checksum. The observer does not inject RAM or savestates.
Cadence controls temporarily change native map fixtures and rebuild the disabled
counterpart, restoring sources and the exact baseline after each build. Do not
run another source-editing build concurrently with that control generator.

## Validation results

The old ROM fails the two-player regression at frame 3934. The corrected default
case passes with two and four players, including 1,301 and 3,812 post-departure
video observations respectively. Start menu and disconnect/rejoin pass. A separate
normal-input offscreen round trip confirms that invisible leases become visible
again with correct native graphics. Connected Save and fresh offline Continue,
checksum and native save-copy checks pass.

Four hosted clients at 100%, 200%, 300% and 400% each execute 800 world updates
in an 800-frame window, without duplicates, missed frames or idle/upload VBlank
crossings. Matched ordinary NPC controls produce 784, 784, 783 and 783 updates,
with maximum gap two; multiplayer adds no missed world update in this comparison.
The separate two-player offscreen round trip also profiles 800/800 updates and
zero background VBlank crossings. These are finite measured gates, not a proof
of universal map rendering compatibility.

Production assembly contracts, eight native lease tests, eleven ROM-budget
tests and the strict MBC30 layout/budget audit pass. The disabled build was
rebuilt and retains the exact baseline hash. Input scenario saves are unchanged.

Final measured results and retained artifact paths are recorded in
`remote-turn-visibility-evidence.json`. Existing protected-NPC scanline conflicts,
public Cloudflare deployment and unavailable physical Android validation are
outside this correction.
