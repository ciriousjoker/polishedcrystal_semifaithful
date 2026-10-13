NAME := polishedcrystal
MODIFIERS :=
VERSION := 3.2.3
AUTHOR := RANGI42

ROM_NAME = $(NAME)$(MODIFIERS)-$(VERSION)
EXTENSION := gbc

TITLE := PKPCRYSTAL
MCODE := PKPC
ROMVERSION := 0x32

FILLER := 0xff

COPYRIGHT = @$(shell date '+%Y') $(AUTHOR) v$(VERSION)

ifneq ($(wildcard rgbds/.*),)
RGBDS ?= rgbds/
else
RGBDS ?=
endif
RGBASM  ?= $(RGBDS)rgbasm
RGBFIX  ?= $(RGBDS)rgbfix
RGBGFX  ?= $(RGBDS)rgbgfx
RGBLINK ?= $(RGBDS)rgblink

Q :=

.SECONDEXPANSION:

POCKET_LOGO = gfx/logo/pocket.bin

RGBASMFLAGS    = -Weverything -Wtruncation=1 -E -Q8 -P includes.asm
RGBASMVCFLAGS  = $(RGBASMFLAGS) -DVIRTUAL_CONSOLE
RGBLINKFLAGS   = -Weverything -Wtruncation=1 -M -n $(ROM_NAME).sym -m $(ROM_NAME).map -p $(FILLER)
RGBLINKVCFLAGS = -Weverything -M -n $(ROM_NAME)_vc.sym -m $(ROM_NAME)_vc.map -p $(FILLER)
RGBFIXFLAGS    = -Weverything -csjv -t $(TITLE) -i $(MCODE) -n $(ROMVERSION) -p $(FILLER) -k 01 -l 0x33 -m MBC3+TIMER+RAM+BATTERY -r 3
RGBGFXFLAGS    = -Weverything

ifeq ($(filter faithful,$(MAKECMDGOALS)),faithful)
MODIFIERS := $(MODIFIERS)-faithful
RGBASMFLAGS += -DFAITHFUL
COPYRIGHT += F
endif
ifeq ($(filter mobile,$(MAKECMDGOALS)),mobile)
ifneq ($(filter vc pocket debug monochrome noir hgss huffman,$(MAKECMDGOALS)),)
$(error Mobile Adapter currently supports standard and faithful GBC builds)
endif
MODIFIERS := $(MODIFIERS)-mobile
RGBASMFLAGS += -DMOBILE_ADAPTER
ifdef MOBILE_RENDERER
$(error MOBILE_RENDERER was a temporary prototype switch; use make mobile)
endif
endif
ifeq ($(MOBILE_TEST_SCENARIOS),1)
ifneq ($(filter faithful vc pocket debug monochrome noir hgss huffman,$(MAKECMDGOALS)),)
$(error MOBILE_TEST_SCENARIOS currently supports standard crystal/mobile builds)
endif
MODIFIERS := $(MODIFIERS)-test
RGBASMFLAGS += -DMOBILE_TEST_SCENARIOS
endif
ifeq ($(filter monochrome,$(MAKECMDGOALS)),monochrome)
MODIFIERS := $(MODIFIERS)-monochrome
RGBASMFLAGS += -DMONOCHROME
endif
ifeq ($(filter noir,$(MAKECMDGOALS)),noir)
MODIFIERS := $(MODIFIERS)-noir
RGBASMFLAGS += -DNOIR
endif
ifeq ($(filter hgss,$(MAKECMDGOALS)),hgss)
MODIFIERS := $(MODIFIERS)-hgss
RGBASMFLAGS += -DHGSS
endif
ifeq ($(filter debug,$(MAKECMDGOALS)),debug)
MODIFIERS := $(MODIFIERS)-debug
RGBASMFLAGS += -DDEBUG
COPYRIGHT += dbg
endif
ifeq ($(filter pocket,$(MAKECMDGOALS)),pocket)
MODIFIERS :=
NAME := pkpc
EXTENSION := pocket
RGBASMFLAGS += -DANALOGUE_POCKET -DNO_RTC
RGBFIXFLAGS = -Weverything -csjv -t $(TITLE) -i $(MCODE) -n $(ROMVERSION) -p $(FILLER) -k 01 -l 0x33 -m MBC5+RAM+BATTERY -r 3 -L $(POCKET_LOGO)
endif
ifeq ($(filter huffman,$(MAKECMDGOALS)),huffman)
# Context models require token output to remain in source order.
.NOTPARALLEL:
Q := @
RGBASMFLAGS += -DHUFFMAN
endif

ifeq ($(filter mobile,$(MAKECMDGOALS)),mobile)
MOBILE_OBJ_DIR := mobile_adapter/build$(MODIFIERS)
else ifeq ($(MOBILE_TEST_SCENARIOS),1)
MOBILE_OBJ_DIR := mobile_adapter/build$(MODIFIERS)
endif
ifdef MOBILE_OBJ_DIR
MOBILE_MKDIR = @mkdir -p $(dir $@)
endif
ifeq ($(filter mobile,$(MAKECMDGOALS)),mobile)
MOBILE_AVATAR_RAW := $(shell python3 mobile_adapter/tools/prepare_virtual_avatars.py --dependencies)
$(MOBILE_OBJ_DIR)/virtual-avatars.asm: mobile_adapter/tools/prepare_virtual_avatars.py mobile_adapter/tools/prepare_native_rectangles.py data/sprites/facings.asm data/player/state_sprites.asm constants/sprite_constants.asm data/sprites/sprites.asm gfx/sprites.asm $(MOBILE_AVATAR_RAW)
	python3 mobile_adapter/tools/prepare_virtual_avatars.py --output "$@"
$(MOBILE_OBJ_DIR)/main.o: $(MOBILE_OBJ_DIR)/virtual-avatars.asm $(MOBILE_OBJ_DIR)/compatibility.sha256
MOBILE_COUNTERPART_GOAL := $(if $(filter faithful,$(MAKECMDGOALS)),faithful,crystal)
MOBILE_COUNTERPART_ROM := $(NAME)$(if $(filter faithful,$(MAKECMDGOALS)),-faithful)-$(VERSION).gbc
.PHONY: mobile-counterpart
mobile-counterpart:
	$(MAKE) $(MOBILE_COUNTERPART_GOAL) MOBILE_TEST_SCENARIOS= RGBDS="$(RGBDS)"
$(MOBILE_OBJ_DIR)/compatibility.sha256: mobile_adapter/tools/prepare_compatibility.py mobile-counterpart
	python3 mobile_adapter/tools/prepare_compatibility.py --baseline "$(MOBILE_COUNTERPART_ROM)" --output "$@"
endif

ifeq ($(MOBILE_TEST_SCENARIOS),1)
TEST_BASELINE := $(NAME)-$(VERSION).gbc
$(TEST_BASELINE):
	$(MAKE) crystal MOBILE_TEST_SCENARIOS= RGBDS="$(RGBDS)"
$(MOBILE_OBJ_DIR)/acceptance-scenarios.asm: mobile_adapter/tools/test_scenarios.py mobile_adapter/rom/test_bootstrap.asm $(TEST_BASELINE)
	python3 mobile_adapter/tools/test_scenarios.py --baseline "$(TEST_BASELINE)" --output "$@"
$(MOBILE_OBJ_DIR)/main.o: $(MOBILE_OBJ_DIR)/acceptance-scenarios.asm
endif

rom_obj := \
	main.o \
	home.o \
	ram.o \
	audio.o \
	audio/music_player.o \
	data/pokemon/dex_entries.o \
	data/pokemon/egg_moves.o \
	data/pokemon/evos_attacks.o \
	data/maps/map_data.o \
	data/text/common.o \
	data/tilesets.o \
	engine/movie/credits.o \
	engine/overworld/events.o \
	gfx/minis_icons.o \
	gfx/pokemon.o \
	gfx/sprites.o \
	gfx/trainers.o \
	gfx/items.o \
	gfx/misc.o

crystal_obj    := $(rom_obj:.o=.o)
crystal_vc_obj := $(rom_obj:.o=_vc.o)
ifdef MOBILE_OBJ_DIR
crystal_obj := $(addprefix $(MOBILE_OBJ_DIR)/,$(rom_obj))
endif

.SUFFIXES:
.PHONY: clean tidy crystal faithful mobile pocket debug monochrome freespace tools bsp huffman vc test-mobile-host test-mobile-rom
.PRECIOUS: %.2bpp %.1bpp
.SECONDARY:
.DEFAULT_GOAL: crystal

crystal: $$(ROM_NAME).$$(EXTENSION)
faithful: crystal
mobile: crystal
ifdef MOBILE_OBJ_DIR
.PHONY: mobile-layout-check
mobile: mobile-layout-check
mobile-layout-check: $$(ROM_NAME).$$(EXTENSION)
	python3 mobile_adapter/tools/audit_rom_budget.py --mobile "$(ROM_NAME).gbc" --layout-only
endif
MOBILE_CARGO ?= cargo
.PHONY: test-mobile-suite
test-mobile-suite:
	MOBILE_ACCEPTANCE_OUTPUT="$(MOBILE_TEST_OUTPUT)" $(MOBILE_CARGO) test --locked --manifest-path mobile_adapter/tests/Cargo.toml --no-fail-fast $(MOBILE_SUITE_ARGS) -- --nocapture
test-mobile-host:
	$(MOBILE_CARGO) test --locked --manifest-path mobile_adapter/server/Cargo.toml
	python3 mobile_adapter/tools/test_asm_contracts.py --rgbds "$(or $(RGBDS),./)"
	python3 -m unittest discover -s mobile_adapter/tools -p 'test_dev_session.py'
	python3 -m unittest discover -s mobile_adapter/tools -p 'test_save_scenarios.py'
	python3 -m unittest discover -s mobile_adapter/tools -p 'test_independent_maps.py'
	python3 -m unittest discover -s mobile_adapter/tools -p 'test_scanline_selection.py'
	python3 -m unittest discover -s mobile_adapter/tools -p 'test_rom_budget.py'
# Emulator acceptance is deliberately separate from host-only unit/ASM tests.
MOBILE_TEST_CACHE ?= /Volumes/Storage/Cache/mobileadapter
MOBILE_TEST_CORE ?=
MOBILE_TEST_PROFILE_CORE ?=
MOBILE_CORE_ARGS = $(if $(MOBILE_TEST_CORE),--core "$(MOBILE_TEST_CORE)") $(if $(MOBILE_TEST_PROFILE_CORE),--profile-core "$(MOBILE_TEST_PROFILE_CORE)")
MOBILE_TEST_CONTROL ?=
MOBILE_TEST_OUTPUT ?= $(MOBILE_TEST_CACHE)/acceptance-$(shell date +%Y%m%d-%H%M%S)
MOBILE_TEST_CASES ?= cadence,avatar-load,local-latency,hosted-latency,priority,saves,running,start-menu,menu-avatar,corner-menu-avatar,warp,warp-turn,rejoin,resume,save-caches,duplicate-ids,route37,exeggutor,night,snow,pool-full
.PHONY: prepare-mobile-core
prepare-mobile-core:
	python3 mobile_adapter/tools/prepare_mbc30_core.py --profile
.PHONY: test-mobile-mbc30
test-mobile-mbc30:
	$(MAKE) -B crystal RGBDS="$(RGBDS)"
	$(MAKE) mobile RGBDS="$(RGBDS)"
	$(MAKE) faithful mobile RGBDS="$(RGBDS)"
	python3 mobile_adapter/tools/audit_rom_budget.py --output "$(MOBILE_TEST_OUTPUT)-budget.json" --strict
	python3 mobile_adapter/tools/check_layout.py --output "$(MOBILE_TEST_OUTPUT)-layout.json"
	python3 mobile_adapter/tools/probe_mbc30.py --cache "$(MOBILE_TEST_CACHE)" --output "$(MOBILE_TEST_OUTPUT)-mapper" --require-supported
	$(MOBILE_CARGO) build --locked --manifest-path mobile_adapter/server/Cargo.toml
	python3 mobile_adapter/tools/verify_milestone2.py --cache "$(MOBILE_TEST_CACHE)" $(MOBILE_CORE_ARGS) --output "$(MOBILE_TEST_OUTPUT)" --cases "$(MOBILE_TEST_CASES)"
.PHONY: test-mobile-maps
test-mobile-maps:
	$(MAKE) crystal RGBDS="$(RGBDS)"
	$(MAKE) mobile RGBDS="$(RGBDS)"
	python3 mobile_adapter/tools/audit_independent_maps.py --baseline polishedcrystal-$(VERSION).gbc --mobile polishedcrystal-mobile-$(VERSION).gbc --output "$(MOBILE_TEST_OUTPUT)-maps.json"
.PHONY: test-mobile-budget
test-mobile-budget:
	$(MAKE) crystal RGBDS="$(RGBDS)"
	$(MAKE) mobile RGBDS="$(RGBDS)"
	python3 mobile_adapter/tools/audit_rom_budget.py --baseline polishedcrystal-$(VERSION).gbc --mobile polishedcrystal-mobile-$(VERSION).gbc --output "$(MOBILE_TEST_OUTPUT)-rom-budget.json" --strict
.PHONY: test-mobile-scanlines
test-mobile-scanlines:
	$(MAKE) mobile RGBDS="$(RGBDS)"
	$(MOBILE_CARGO) build --locked --manifest-path mobile_adapter/server/Cargo.toml --bin mobile-bridge
	python3 mobile_adapter/tools/probe_scanline_pressure.py --cache "$(MOBILE_TEST_CACHE)" $(MOBILE_CORE_ARGS) --output "$(MOBILE_TEST_OUTPUT)-scanlines"
test-mobile-rom:
	$(MAKE) test-mobile-budget RGBDS="$(RGBDS)" MOBILE_TEST_OUTPUT="$(MOBILE_TEST_OUTPUT)"
	$(MAKE) test-mobile-maps RGBDS="$(RGBDS)" MOBILE_TEST_OUTPUT="$(MOBILE_TEST_OUTPUT)"
	$(MOBILE_CARGO) build --locked --manifest-path mobile_adapter/server/Cargo.toml
	python3 mobile_adapter/tools/verify_milestone2.py --cache "$(MOBILE_TEST_CACHE)" $(MOBILE_CORE_ARGS) $(if $(MOBILE_TEST_CONTROL),--control "$(MOBILE_TEST_CONTROL)") --output "$(MOBILE_TEST_OUTPUT)" --cases "$(MOBILE_TEST_CASES)"

monochrome: crystal
noir: crystal
hgss: crystal
debug: crystal
pocket: crystal
vc: $$(ROM_NAME).patch

tools:
	$(MAKE) -C tools/

clean: tidy
	find gfx maps data/tilesets -name '*.lzp' -delete
	find gfx \( -name '*.[12]bpp' -o -name '*.2bpp.vram[012]' -o -name '*.2bpp.vram[012]p' \) -delete
	find gfx/pokemon -mindepth 1 \( -name 'bitmask.asm' -o -name 'frames.asm' \
		-o -name 'front.animated.tilemap' -o -name 'front.dimensions' \) -delete
	find data/tilesets -name '*_collision.bin' -delete
	$(MAKE) clean -C tools/

tidy:
	$(RM) $(crystal_obj) $(crystal_vc_obj) $(wildcard $(NAME)-*.gbc) $(wildcard $(NAME)-*.pocket) $(wildcard $(NAME)-*.bsp) \
		$(wildcard $(NAME)-*.map) $(wildcard $(NAME)-*.sym) $(wildcard $(NAME)-*.patch) rgbdscheck.o $(wildcard gfx/title/version.2bpp*)

freespace: crystal tools/bankends
ifdef MOBILE_OBJ_DIR
	python3 mobile_adapter/tools/audit_rom_budget.py --mobile "$(ROM_NAME).gbc" --layout-only
else
	tools/bankends $(ROM_NAME).map > bank_ends.txt
endif

bsp: $(ROM_NAME).bsp

huffman: crystal


rgbdscheck.o: rgbdscheck.asm
	$Q$(RGBASM) -o $@ $<

ifeq (,$(filter clean tidy tools,$(MAKECMDGOALS)))
$(info $(shell $(MAKE) -C tools))
endif

preinclude_deps := includes.asm $(shell tools/scan_includes includes.asm)

define DEP
$1: $2 $$(filter-out mobile_adapter/build-test/acceptance-scenarios.asm mobile_adapter/build-mobile-test/acceptance-scenarios.asm mobile_adapter/build-mobile/compatibility.sha256 mobile_adapter/build-mobile-test/compatibility.sha256 mobile_adapter/build-faithful-mobile/compatibility.sha256,$$(shell tools/scan_includes $2)) $(preinclude_deps) | rgbdscheck.o
	$$(MOBILE_MKDIR)
	$Q$$(RGBASM) $$(RGBASMFLAGS) -o $$@ $$<
endef

define VCDEP
$1: $2 $$(filter-out mobile_adapter/build-test/acceptance-scenarios.asm mobile_adapter/build-mobile-test/acceptance-scenarios.asm mobile_adapter/build-mobile/compatibility.sha256 mobile_adapter/build-mobile-test/compatibility.sha256 mobile_adapter/build-faithful-mobile/compatibility.sha256,$$(shell tools/scan_includes $2)) $(preinclude_deps) | rgbdscheck.o
	$Q$$(RGBASM) $$(RGBASMVCFLAGS) -o $$@ $$<
endef

ifeq (,$(filter clean tidy tools,$(MAKECMDGOALS)))
$(foreach obj, $(crystal_obj), $(eval $(call DEP,$(obj),$(patsubst $(MOBILE_OBJ_DIR)/%,%,$(obj:.o=.asm)))))
$(foreach obj, $(crystal_vc_obj), $(eval $(call VCDEP,$(obj),$(obj:_vc.o=.asm))))
endif

$(ROM_NAME).patch: $(ROM_NAME)_vc.gbc $(ROM_NAME).$(EXTENSION) vc.patch.template
	tools/make_patch $(ROM_NAME)_vc.sym $^ $@

.$(EXTENSION): tools/bankends
$(ROM_NAME).$(EXTENSION): $(crystal_obj) layout.link
	$Q$(RGBLINK) $(RGBLINKFLAGS) -l layout.link -o $@ $(filter %.o,$^)
	$Q$(RGBFIX) $(RGBFIXFLAGS) $@
ifdef MOBILE_OBJ_DIR
	$Qpython3 mobile_adapter/tools/audit_rom_budget.py --mobile "$@" --layout-only
else
	$Qtools/bankends -q $(ROM_NAME).map >&2
endif

$(ROM_NAME)_vc.gbc: $(crystal_vc_obj) layout.link
	$Q$(RGBLINK) $(RGBLINKVCFLAGS) -l layout.link -o $@ $(filter %.o,$^)
	$Q$(RGBFIX) $(RGBFIXFLAGS) $@
	$Qtools/bankends -q $(ROM_NAME)_vc.map >&2

.bsp: tools/bspcomp
%.bsp: $(wildcard bsp/*.txt)
	$Qcd bsp; ../tools/bspcomp patch.txt ../$@; cd ..

gfx/battle/lyra_back.2bpp: RGBGFXFLAGS += -Z
gfx/battle/substitute-back.2bpp: RGBGFXFLAGS += -Z
gfx/battle/substitute-front.2bpp: RGBGFXFLAGS += -Z
gfx/battle/ghost.2bpp: RGBGFXFLAGS += -Z
gfx/battle/hpexpbar.2bpp: tools/gfx += --trim-whitespace

gfx/battle_anims/angels.2bpp: tools/gfx += --trim-whitespace
gfx/battle_anims/beam.2bpp: tools/gfx += --remove-xflip --remove-yflip --remove-whitespace
gfx/battle_anims/bubble.2bpp: tools/gfx += --trim-whitespace
gfx/battle_anims/charge.2bpp: tools/gfx += --trim-whitespace
gfx/battle_anims/egg.2bpp: tools/gfx += --remove-whitespace
gfx/battle_anims/explosion.2bpp: tools/gfx += --remove-whitespace
gfx/battle_anims/hit.2bpp: tools/gfx += --remove-whitespace
gfx/battle_anims/horn.2bpp: tools/gfx += --remove-whitespace
gfx/battle_anims/lightning.2bpp: tools/gfx += --remove-whitespace
gfx/battle_anims/misc.2bpp: tools/gfx += --remove-duplicates --remove-xflip
gfx/battle_anims/noise.2bpp: tools/gfx += --remove-whitespace
gfx/battle_anims/objects.2bpp: tools/gfx += --remove-whitespace --remove-xflip
gfx/battle_anims/reflect.2bpp: tools/gfx += --remove-whitespace
gfx/battle_anims/rocks.2bpp: tools/gfx += --remove-whitespace
gfx/battle_anims/skyattack.2bpp: tools/gfx += --remove-whitespace
gfx/battle_anims/status.2bpp: tools/gfx += --remove-whitespace

gfx/card_flip/card_flip_1.2bpp: tools/gfx += --trim-whitespace
gfx/card_flip/card_flip_2.2bpp: tools/gfx += --remove-whitespace

gfx/evo/bubble.2bpp: tools/gfx += --trim-whitespace

gfx/font/%.1bpp: tools/gfx += --trim-whitespace
gfx/font/space.2bpp: tools/gfx =

gfx/mail/dragonite.1bpp: tools/gfx += --remove-whitespace
gfx/mail/flower_mail_border.1bpp: tools/gfx += --remove-whitespace
gfx/mail/large_note.1bpp: tools/gfx += --remove-whitespace
gfx/mail/litebluemail_border.1bpp: tools/gfx += --remove-whitespace
gfx/mail/surf_mail_border.1bpp: tools/gfx += --remove-whitespace

gfx/music_player/bg.2bpp: tools/gfx += --trim-whitespace
gfx/music_player/music_player.2bpp: gfx/music_player/bg.2bpp gfx/music_player/ob.2bpp ; $Qcat $^ > $@

gfx/new_game/shrink1.2bpp: RGBGFXFLAGS += -Z
gfx/new_game/shrink2.2bpp: RGBGFXFLAGS += -Z

gfx/overworld/overworld.2bpp: gfx/overworld/puddle_splash.2bpp gfx/overworld/cut_grass.2bpp gfx/overworld/cut_tree.2bpp gfx/overworld/heal_machine.2bpp gfx/overworld/fishing_rod.2bpp gfx/overworld/shadow.2bpp gfx/overworld/shaking_grass.2bpp gfx/overworld/boulder_dust.2bpp ; $Qcat $^ > $@

gfx/pack/pack_left.2bpp: tools/gfx += --trim-whitespace
gfx/pack/pack_top_left.2bpp: gfx/pack/pack_top.2bpp gfx/pack/pack_left.2bpp ; $Qcat $^ > $@

gfx/paintings/%.2bpp: RGBGFXFLAGS += -Z

gfx/player/chris_back.2bpp: RGBGFXFLAGS += -Z
gfx/player/kris_back.2bpp: RGBGFXFLAGS += -Z
gfx/player/crys_back.2bpp: RGBGFXFLAGS += -Z
gfx/player/beta_back.2bpp: RGBGFXFLAGS += -Z

gfx/pokedex/%.bin: gfx/pokedex/%.tilemap gfx/pokedex/%.attrmap ; $Qcat $^ > $@
gfx/pokedex/oam.2bpp: tools/gfx += --trim-whitespace
gfx/pokedex/pokedex.2bpp: gfx/pokedex/pokedex0.2bpp gfx/pokedex/pokedex1.2bpp gfx/pokedex/area.2bpp ; $Qcat $^ > $@
gfx/pokedex/question_mark.2bpp: RGBGFXFLAGS += -Z

gfx/pokegear/pokegear.2bpp: tools/gfx += --trim-whitespace
gfx/pokegear/pokegear_sprites.2bpp: tools/gfx += --trim-whitespace

gfx/pokemon/%/back.2bpp: RGBGFXFLAGS += -Z

gfx/pc/obj.2bpp: gfx/pc/modes.2bpp gfx/pc/bags.2bpp ; $Qcat $^ > $@

gfx/slots/slots_1.2bpp: tools/gfx += --trim-whitespace
gfx/slots/slots_2.2bpp: tools/gfx += --interleave --png=$<
gfx/slots/slots_3.2bpp: tools/gfx += --interleave --png=$< --remove-duplicates --keep-whitespace --remove-xflip

gfx/splash/copyright.2bpp: gfx/splash/copyright.txt tools/fine_print.c
	$Qtools/fine_print -c 013 -s 0 "$$(cat "$<")" "$@"

gfx/stats/%.bin: gfx/stats/%.tilemap gfx/stats/%.attrmap ; $Qcat $^ > $@
gfx/stats/judge.2bpp: tools/gfx += --trim-whitespace

gfx/title/crystal.2bpp: tools/gfx += --interleave --png=$<

gfx/title/version.2bpp: Makefile tools/fine_print.c
	$Qtools/fine_print -e 20 '$(COPYRIGHT)' $@

ifdef MOBILE_OBJ_DIR
# Keep variant title generation from contaminating the disabled standard build.
gfx/title/version.2bpp: COPYRIGHT = @$(shell date '+%Y') $(AUTHOR) v$(VERSION)
$(MOBILE_OBJ_DIR)/main.o: $(MOBILE_OBJ_DIR)/title-version.2bpp.lzp
$(MOBILE_OBJ_DIR)/title-version.2bpp: Makefile tools/fine_print.c
	@mkdir -p $(dir $@)
	$Qtools/fine_print -e 20 '$(COPYRIGHT)' $@
endif

gfx/title/suicune_unowns.2bpp: RGBGFXFLAGS += --unique-tiles --nb-tiles 127,127 --base-tiles 0,128
gfx/title/suicune_unowns.tilemap: RGBGFXFLAGS += --unique-tiles --nb-tiles 127,127 --base-tiles 0,128
gfx/title/suicune_unowns.tilemap: gfx/title/suicune_unowns.png
	$Q$(RGBGFX) -c dmg $(RGBGFXFLAGS) -t $@ $<

gfx/town_map/town_map.2bpp: tools/gfx += --trim-whitespace

gfx/trade/game_boy.2bpp: tools/gfx += --remove-duplicates --remove-xflip --remove-yflip --remove-whitespace
gfx/trade/link_cable.2bpp: tools/gfx += --remove-duplicates --remove-whitespace
gfx/trade/poof_cable.2bpp: gfx/trade/poof.2bpp gfx/trade/cable.2bpp ; $Qcat $^ > $@
gfx/trade/game_boy_cable.2bpp: gfx/trade/game_boy.2bpp gfx/trade/link_cable.2bpp ; $Qcat $^ > $@
gfx/trade/trade_screen.2bpp: gfx/trade/border.2bpp gfx/trade/textbox.2bpp ; $Qcat $^ > $@

gfx/trainer_card/chris_card.2bpp: RGBGFXFLAGS += -Z
gfx/trainer_card/kris_card.2bpp: RGBGFXFLAGS += -Z
gfx/trainer_card/crys_card.2bpp: RGBGFXFLAGS += -Z
gfx/trainer_card/beta_card.2bpp: RGBGFXFLAGS += -Z

gfx/trainers/%.2bpp: RGBGFXFLAGS += -Z

gfx/type_chart/bg.2bpp: tools/gfx += --remove-duplicates --remove-xflip --remove-yflip
gfx/type_chart/bg0.2bpp: gfx/type_chart/bg.2bpp.vram1p gfx/type_chart/bg.2bpp.vram0p ; $Qcat $^ > $@
gfx/type_chart/ob.2bpp: tools/gfx += --interleave --png=$<


gfx/pokemon/%/front.animated.2bpp: gfx/pokemon/%/front.2bpp gfx/pokemon/%/front.dimensions
	$Qtools/pokemon_animation_graphics -o $@ $^
gfx/pokemon/%/front.animated.tilemap: gfx/pokemon/%/front.2bpp gfx/pokemon/%/front.dimensions
	$Qtools/pokemon_animation_graphics -t $@ $^
gfx/pokemon/%/bitmask.asm: gfx/pokemon/%/front.animated.tilemap gfx/pokemon/%/front.dimensions
	$Qtools/pokemon_animation -b $^ > $@
gfx/pokemon/%/frames.asm: gfx/pokemon/%/front.animated.tilemap gfx/pokemon/%/front.dimensions
	$Qtools/pokemon_animation -f $^ > $@



%.lzp: %
	$Qtools/lzpcompress -- $< $@

#%.4bpp: %.png
#	$Qsuperfamiconv tiles -R -i $@ -d $<

%.2bpp: %.png
	$Q$(RGBGFX) -c dmg $(RGBGFXFLAGS) -o $@ $<
	$(if $(tools/gfx),\
		$Qtools/gfx $(tools/gfx) -o $@ $@)

%.1bpp: %.png
	$(RGBGFX) -c dmg $(RGBGFXFLAGS) -d1 -o $@ $<
	$(if $(tools/gfx),\
		$Qtools/gfx $(tools/gfx) -d1 -o $@ $@)

%.2bpp.vram0: %.2bpp
	$Qtools/sub_2bpp.sh $< 128 > $@

%.2bpp.vram1: %.2bpp
	$Qtools/sub_2bpp.sh $< 128 128 > $@

%.2bpp.vram2: %.2bpp
	$Qtools/sub_2bpp.sh $< 256 128 > $@

%.2bpp.vram0p: %.2bpp
	$Qtools/sub_2bpp.sh $< 127 > $@

%.2bpp.vram1p: %.2bpp
	$Qtools/sub_2bpp.sh $< 127 128 > $@

%.2bpp.vram2p: %.2bpp
	$Qtools/sub_2bpp.sh $< 255 128 > $@

%.vwf.1bpp: %.2bpp
	$Qtools/vwf -o $@ $<

%.vwf.widths: %.2bpp
	$Qtools/vwf -w $@ $<

%.dimensions: %.png
	$Qtools/png_dimensions $< $@

data/tilesets/%_collision.bin: data/tilesets/%_collision.asm
	$QRGBASM=$(RGBASM) RGBLINK=$(RGBLINK) tools/collision_asm2bin.sh $< $@
