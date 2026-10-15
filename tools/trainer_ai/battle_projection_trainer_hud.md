# Trainer HUD graphics-leaf projection probe

The isolated caller and actual endturn continuation have both been executed by
the root coordinator under the single-CPU limit. Scope and results are below.

Run with a valid banked CGB controller snapshot taken at enemy replacement:

```
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 python3 -B \
  tools/trainer_ai/battle_projection_trainer_hud_check.py \
  --project /Volumes/Storage/tmp/polished-ai \
  --snapshot /absolute/path/to/enemy-ko.state \
  --metadata /absolute/path/to/enemy-ko.json
```

The JSON must contain state_sha256, rom, symbols, rom_sha256, symbols_sha256 and
banked_wram:true. The State layout is unchanged: fixed WRAM0 at C000-CFFF,
canonical bank one at D000-DFFF and banks two through seven in m[0000:6000].
A real KO with a living enemy reserve supplies a useful state; a statused or
partially damaged reserve adds a distinct ball-icon condition. The probe pins the
snapshot/ROM/symbol identities before changing only the isolated call frame and
explicit frontend initialization in both copies.

Both mGBA paths execute actual EnemySwitch_TrainerHud, StageBallTilesData,
LoadTrainerHudOAM and source WRAM effects. The conditional path returns only from
LoadBallIconGFX. Actual hardware FF48 writes occur normally in both paths, so this
experiment validates the omitted graphics leaf rather than pretending the write
was itself a CPU-state transformation. The actual path runs real Get2bpp,
Request2bpp, LCD, VBlank and DMA without UI hooks. Source bank selection and all
physical WRAM are loaded/extracted through real mGBA SVBK registers.

The report compares every physical WRAM byte, raw SVBK, HRAM and all CPU fields.
It separately asserts source-defined battle/party/inventory regions, staged HUD
ball tiles, border tiles, shadow OAM, source return PC/stack and successful calls.
Every remaining difference is retained with symbol aliases. Register/scratch
inequality must be traced to the replacement controller's next source consumer;
passing the isolated HUD projection alone does not establish that liveness.
In particular wBattleHUDTiles shares storage with enemy AI and Forewarn scratch.
No global mask or whole-HUD identity return is introduced.

Results are saved under build/battle-projection/trainer-hud. A source fault,
unreached graphics leaf or semantic mismatch fails after saving diagnostics. The
source-controller continuation uses the actual KO snapshot through the endturn
return, without inferring full trainer-fight fidelity from this leaf.

## Actual endturn continuation mode

Add `--continuation` to preserve the original source EnemySwitch_TrainerHud
entry SP and outer FarCall/controller frames. The caller then continues through
HandleBetweenTurnEffects to the metadata's real FFFE return stop. Other current
presentation hooks are explicit outside the trainer HUD in both modes. All
trainer-HUD descendants still execute unhooked in the actual context, including
the real graphics request, stack scratch and bank operations. No whole source
replacement controller is substituted with a host approximation.

Source combat Random outputs can be recorded in the reference and replayed in
the actual context with the validated FarCall/Range origin-frame convention.
Random calls inside the actual HUD stay UI draws. The report records count and
exact consumption; a zero-draw snapshot does not claim the optional replay branch
was exercised. Unsupported policy/terminal choices are not identity hooks.

The root executed the original isolated revision successfully: both calls
completed, gameplay/HUD fields and all CPU registers matched, with thirteen raw
scratch differences retained. Four tiles fit in the actual HBlank transfer path,
so zero VBlanks occurred; this does not prove the VBlank fallback. Exact executed
sources/report are preserved under isolated-revision.

The root also executed continuation mode successfully: both contexts returned
from the actual endturn controller, with equal gameplay/HUD fields, CPU registers,
SP, ROM bank and SVBK. Conditional execution took 8,672 instructions and actual
graphics execution 9,823. The actual context serviced one VBlank. Neither context
made a combat or UI Random call, so this run does not exercise the optional RNG
replay branch. The report retains all twenty-three raw WRAM/HRAM differences.

Those differences consist of eight inactive stack bytes; text/frame, VBlank,
private RNG and bank-backup state; two bytes of wSPBuffer; background-map half;
and four tile-transfer source/destination bytes. Source HBlankCopy2bpp and
_Serve2bppRequest save the current SP before repurposing it and restore that saved
SP afterwards. WriteVCopyRegistersToHRAM initializes both tile pointers before a
new request. These are concrete local lifetimes, not a global scratch mask.
The complete HUD tile union, including its AI/Forewarn aliases, agrees after the
source replacement continuation. No failing gameplay consumer was observed in
this snapshot. Other presentation roots remain conditional outside the HUD;
other teams, abilities, timing phases and full ordinary frontend equivalence
require separate evidence.
