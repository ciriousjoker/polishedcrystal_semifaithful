#!/usr/bin/env python3
"""Retain typed delayed/volatile source witnesses without importing battle drivers."""
from __future__ import annotations

import argparse
import json
from pathlib import Path

import battle_coverage as coverage
from battle_family_evidence import artifact_sink, bind_execution, finish, digest, read, require

ROOT = Path(__file__).resolve().parents[2]
MANIFEST = 'dec3cb17a34aeeb9e0f36e44590d0890b7d0816ddff98fc7bb0346b9b32737b0'
HOSTS = \
{
  "battle_controller_delayed.py": "ea5db14843349a4cb339099512b8f371a8bc3648fc593391fe95abd5c69e4d1d",
  "battle_controller.py": "6abe731e7eac74a3cae16673c2c6339ad985ddc455a1ba913d915f2f31cda9b3",
  "battle_controller_profile.py": "9cf85ef64a22eda7781b739119d4f9eb958a860453af12189900f7b84d13e33d",
  "battle_controller_fight.py": "eb92e12bc7d1100da6e90e6382c04e8b32ce769c7747c2f679818774569deb79",
  "battle_controller_fight_check.py": "065bba593a79d3103de9d8d752b067ba9c63ec7116c9f4edbd7aa62c30eda249",
  "battle_controller_check.py": "d3621ffd1c9c142e496476e162833a91853992788d561b083b3a873d035dc9d4",
  "battle_controller_oracle.c": "7840c8b7a31bc6431518c5c61569cf961fc82a0ae40fd365f9768ebf12648e23",
  "battle_decisions.py": "9aea66f9c5ba912da9032a503e5a3f8399ff25d42f3deec58068724fc0225d4c",
  "battle_scenarios.py": "39b61a8bd9175f610b3ad63a3167117ca3038016f73621721168f29b5e44eee9",
  "battle_bag.py": "0f77ff9fb79a8c2d58f5d1effc3f1faf731477300847f2270165091dffad95c8",
  "battle_actions.py": "09e4aa5358ac724d8c3b8c6d4209ca1aff1f6a9b7f6c92fd80a9a26a135d39cd",
  "battle_coverage.py": "a53bb4c9352e290cd4535f260628bae00cdae1152c345501ec9dd1f8af24e490",
  "battle_env.py": "2c281bda28d290c8cb7e4e608907a69e0786ef4f72ed5e46cc28b130781865ee",
  "battle_env_check.py": "f9403ece032b28de048a8e20469823005a870eba8ed7c6627a5a65a3792b63b7",
  "stateful_lifting.py": "5e5febf491dca885db415475d4b2c34a5a50c2d150e6ca21bcb998401dc9eade",
  "stateful_fixture_check.py": "8a01e4700625e9a8a015aa6bbc525caf070ad9d3fb84664bd7386e4be7dc3d87",
  "battle_actions_profile.py": "f5371e089fed22473b0fd473598d059fb04032108b45078f4bcd386ed77419e2",
  "battle_env_profile.py": "bf97f6cee377cb5bef9e557e8771b648ad37d4f853591b9fbfcca12468890c74",
  "battle_party.py": "55b77cd8911680b4b713174308b19e2f20df8f9f3443f84cbd830eadaa734ff4",
  "battle_party_profile.py": "1169d7def08b0ef8f2ee35e941278437bc40874870bafd8830220b4ef302a528",
  "battle_party_probe.py": "8ff21c6c10c5995f1fe3109d561e8adc15ce1d7cfcea2a7d81a8bab0bd5715d6",
  "battle_status.py": "b666d872f52de6c59922c284d179cb2f2dc666fafa22081a0abad5df56d7435a",
  "battle_status_profile.py": "97cda432b618c0b119c0f66b94a4d3c9f08936b3ef393b706f9690c83ea0ee1f"
}
GAME_HASHES = \
{
  "engine/battle/endturn.asm": "94b649be025a90fd6730b92caedd5416d9b23bd17052dfe59c4772d081685563",
  "engine/battle/effect_commands.asm": "1a702f8cbdd03b93c0811350bd47c20b601558f065ca87f1a388b2b7855dc608",
  "engine/battle/core.asm": "c70c34b834411bfe4ecb36fb98ac7881a3876eb3429eaa03cf2745eb79cbae65",
  "home/battle.asm": "2c718f1d14a728b174701e10e4999720ae224679e39bc0d4f2a23619ee21a70c",
  "engine/battle/move_effects/leech_seed.asm": "5fbd2ba55eec87b8598d7f0a4defcf7b5bb5ee014c4b2d580ea80f11c5b670b7",
  "engine/battle/move_effects/curse.asm": "e0b2b1742b1267fb46ec18d3eb19c16018ecfe030187530c92685dfc625a4212",
  "engine/battle/move_effects/perish_song.asm": "45477f53ca6a34bb9c62629548f38d6eac8a983952723b0659e0c73837fd9148",
  "engine/battle/move_effects/future_sight.asm": "61e441f17454833cbd5b377cde5843f1dc205e613a5449950e1c11f22a529d75",
  "constants/battle_constants.asm": "e4d1319a050241bc7efeba533c6370323aae87b5bbbe913787e7a3f597df1649",
  "constants/type_constants.asm": "afc5f619d21132ac54f771081344a5585ac2549b1ef22c17752351c1518b4799",
  "constants/pokemon_constants.asm": "6b92811d43d304c0b44deb78e81ee3f1ae2889caa0d14b8da080014b02ebc7cb",
  "constants/pokemon_data_constants.asm": "dbf901faf06638eabf556d447ce0eebdcb80bd543b2665f5b3048caa41aaea30",
  "constants/ability_constants.asm": "87b2f29bb5e6bdf3b60970512a0b78f58da778bc56705b916bde1386123d1dff",
  "constants/move_constants.asm": "80cb65ae226fa4151d8d813e8aef1de2edfd1a0091a2941424cbef53ccee7ec1",
  "constants/move_effect_constants.asm": "10779f333b4ffa93d4baced10b98324838244d338db6e6dceeb0ad4429ba54f2",
  "data/moves/moves.asm": "cf08c6155a498dc207e50052d76dc8b5c6f423ccbe7ed81e89250661f10215e2",
  "data/pokemon/base_stats/horsea.asm": "640bc72ace5c12840c9ee20da7f5a7bfd42eafd8f3835cc3e8deed5aafe37aa9",
  "data/pokemon/base_stats/gastly.asm": "95a6f434099454221ee6151863270dd7626162efd692ad2bbfab6c8c7ce584e5",
  "data/pokemon/base_stats/oddish.asm": "b4dbe9dcb172ddb2408d848b6a5e1485145dfe33594906e91b309d1f3c65fbff"
}
GAME_SOURCES = set(GAME_HASHES)
BOUNDARY = 'post-reset-first-frozen-actor-decision'
CASES = {
    'leech': ('LEECH_SEED', 3), 'leech-switch': ('LEECH_SEED', 3),
    'leech-grass': ('LEECH_SEED', 3), 'curse-ghost': ('CURSE', 3),
    'curse-other': ('CURSE', 3), 'perish': ('PERISH_SONG', 5),
    'future-switch': ('FUTURE_SIGHT', 4), 'wrap': ('WRAP', 6),
}


def constants(root, path):
    result, value = {}, 0
    def integer(text):
        return int(text[1:], 16) if text.startswith('$') else int(text, 0)
    for _, code in coverage.lines(root, path):
        if path == 'constants/pokemon_constants.asm' and code.startswith('DEF NUM_SPECIES '):
            break
        if code.startswith('const_def'):
            value = integer(code[len('const_def'):].strip() or '0')
        elif code.startswith('const_next '):
            value = integer(code.split()[1])
        elif code.startswith('const_skip'):
            value += integer(code[len('const_skip'):].strip() or '1')
        elif code.startswith('const '):
            result[code.split()[1]] = value
            value += 1
    return result


def source_contract(root):
    for path, expected in GAME_HASHES.items():
        require(digest(root/path) == expected, 'Reviewed delayed source changed: '+path)
    endturn = (root/'engine/battle/endturn.asm').read_text()
    effects = (root/'engine/battle/effect_commands.asm').read_text()
    home = (root/'home/battle.asm').read_text()
    def handler(name, following):
        return endturn.split(name+':', 1)[1].split(following+':', 1)[0]
    leech = handler('HandleLeechSeed', 'HandlePoison')
    curse = handler('HandleCurse', 'HandleWrap')
    wrap = handler('HandleWrap', 'HandleEncore')
    perish = handler('HandlePerishSong', 'HandleTrickRoom')
    future = handler('HandleFutureSight', 'HandleLeftovers')
    require(leech.index('call GetEighthMaxHP') < leech.index('farcall SubtractHPFromUser') <
            leech.index('farcall GetHPAbsorption') and 'jmp HalveBC' in
            effects.split('GetHPAbsorption:', 1)[1].split('HandleBigRoot:', 1)[0],
            'Leech source eighth drain then half-drain healing changed')
    require('call GetQuarterMaxHP' in curse and 'farcall SubtractHPFromUser' in curse,
            'Curse residual source changed')
    casting = (root/'engine/battle/move_effects/curse.asm').read_text()
    require(all(code in casting for code in ('call CheckIfUserIsGhostType', 'call GetHalfMaxHP',
            'set SUBSTATUS_CURSE, [hl]', 'ld b, SPEED', 'ld b, ATTACK', 'ld b, DEFENSE')),
            'Curse type branch/cost/stage source changed')
    require('ld [hl], 4' in (root/'engine/battle/move_effects/perish_song.asm').read_text() and
            perish.index('dec [hl]') < perish.index('ret nz') < perish.index('call GetMaxHP'),
            'Perish four-count decrement/faint source changed')
    require('or $3' in (root/'engine/battle/move_effects/future_sight.asm').read_text() and
            future.index('dec [hl]') < future.index('and $f') < future.index('farcall DoMove'),
            'Future Sight packed counter and delayed move source changed')
    trap = effects.split('BattleCommand_traptarget:', 1)[1].split('.Traps:', 1)[0]
    require('and 1\n\tadd 4' in trap and wrap.index('dec [hl]') < wrap.index('and %111') <
            wrap.index('call GetEighthMaxHP'), 'Wrap sampled duration/decrement-before-damage changed')
    require('GetQuarterMaxHP::\n\tcall GetHalfMaxHP\n\tjr HalveBC' in home and
            'GetEighthMaxHP::\n\tcall GetQuarterMaxHP\n\tjr HalveBC' in home and
            'GetHalfMaxHP::\n\tcall GetMaxHP\nHalveBC::' in home,
            'Delayed source fractional HP helper changed')


def validate_witness(root, name, setter, raw, row, rules, ids, rom_bytes, symbols):
    move, rounds = CASES[name]
    abilities, types, moves, effects, species_ids = ids
    target = setter ^ 1
    w = raw['witness']
    require(w == row['witness'] and w['kind'] == name and w['setter'] == setter,
            'Delayed raw/report typed witness mismatch')
    data = list(rom_bytes('Moves', (moves[move]-1)*8, 8))
    effect_name = 'EFFECT_TRAP' if move == 'WRAP' else 'EFFECT_'+move
    cast_source = 'engine/battle/effect_commands.asm' if move == 'WRAP' else (
        'engine/battle/move_effects/'+move.lower()+'.asm')
    expected_meta = dict(move=move, identifier=moves[move], compiled_move=data,
        effect=effects[effect_name], effect_name=effect_name,
        interval_source='engine/battle/endturn.asm', cast_source=cast_source)
    require(w['metadata'] == expected_meta and data[:2] == [moves[move], effects[effect_name]] and
            rules['move:'+move]['identifier'] == moves[move] and
            rules['move:'+move]['effect'] == effect_name and
            rules['effect:'+effect_name]['identifier'] == effects[effect_name],
            'Delayed source/ROM move/effect binding differs')
    teams = [raw['scenario']['player'], raw['scenario']['enemy']]
    lengths = [2 if side == setter and name == 'perish' or side == target and name in
               ('leech-switch', 'future-switch', 'wrap', 'perish') else 1 for side in (0, 1)]
    require(list(map(len, teams)) == lengths and raw['scenario']['inventory'] == [None, None] and
            raw['scenario']['bag'] == [], 'Delayed fixture teams/inventory differ')
    before = w['initial']
    require(len(before) == 2 and before[0]['teams'] == before[1]['teams'] and
            before[0]['battlefield'] == before[1]['battlefield'], 'Delayed initial observations disagree')
    for side, team in enumerate(teams):
        for index, fixture in enumerate(team):
            species = 'GASTLY' if side == setter and index == 0 and name == 'curse-ghost' else (
                'ODDISH' if side == target and index == 0 and name == 'leech-grass' else 'HORSEA')
            move_names = ['SPLASH', 'WATER_GUN']+([move] if side == setter and index == 0 else [])
            ability = {'HORSEA': 'SWIFT_SWIM', 'GASTLY': 'LEVITATE', 'ODDISH': 'CHLOROPHYLL'}[species]
            expected_types = {'HORSEA': ['WATER', 'WATER'], 'GASTLY': ['GHOST', 'POISON'],
                              'ODDISH': ['GRASS', 'POISON']}[species]
            level = 100 if side == target and name == 'future-switch' else 50
            hp = 10 if side == setter and index == 0 and name.startswith('leech') else None
            require(fixture['species'] == species and fixture['moves'] == move_names and fixture['level'] == level and
                    fixture['hp'] == hp and fixture['item'] == 'NO_ITEM' and fixture['status'] == 0 and
                    fixture['ability_slot'] == fixture['form'] == fixture['nature'] == fixture['happiness'] == 0 and
                    fixture['dvs'] == [0]*3 and fixture['evs'] == [0]*6 and fixture['pp'] is None,
                    'Delayed isolated fixture differs from reviewed source inputs')
            mon = before[side]['teams'][side][index]
            expected_moves = [moves[value] for value in move_names]+[0]*(4-len(move_names))
            expected_pp = [rom_bytes('Moves', (value-1)*8, 8)[5] if value else 0 for value in expected_moves]
            require(mon['species'] == species_ids[species] and mon['ability'] == abilities[ability] and
                    mon['ability_slot'] == 1 and mon['level'] == level and mon['item'] == mon['status'] == 0 and
                    mon['moves'] == expected_moves and mon['pp'] == expected_pp and mon['pp_ups'] == [0]*4 and
                    mon['HP'] == (hp if hp is not None else mon['MaxHP']) and mon['active'] is (index == 0),
                    'Delayed initial observed identity/health/move/PP differs')
            if index == 0:
                require(mon['types'] == [types[value] for value in expected_types], 'Delayed active source type differs')
    probes = w['probes']
    require(len(probes) == rounds and len(raw['turns']) >= rounds, 'Delayed probe incomplete')
    masks = w['initial_legal_masks']
    replacement = [dict(side=side, action=dict(type='SwitchAction', index=1)) for side in (0, 1)]
    actual_replacements = [choice for turn in raw['turns'][:rounds] for choice in turn['replacement_choices']]
    require(w['replacement_choices'] == actual_replacements and
            (sorted(actual_replacements, key=lambda choice: choice['side']) == replacement if name == 'perish' else not actual_replacements),
            'Delayed explicit source replacement choices differ')
    active = lambda obs, side: next(mon for mon in obs[side]['teams'][side] if mon['active'])
    for number, probe in enumerate(probes):
        after = probe['observations']
        require(probe['round'] == number and probe['turn'] == number+1 and len(after) == 2 and
                after[0]['teams'] == after[1]['teams'] and after[0]['battlefield'] == after[1]['battlefield'],
                'Delayed probe round or both-side observations differ')
        actions = [dict(type='MoveAction', slot=0), dict(type='MoveAction', slot=0)]
        if number == 0:
            actions[setter] = dict(type='MoveAction', slot=2)
        elif number == 1 and name in ('leech-switch', 'future-switch'):
            actions[target] = dict(type='SwitchAction', index=1)
        require(raw['turns'][number]['actions'] == actions and all(action in masks[side] for side, action in enumerate(actions)),
                'Delayed cast/Splash/switch action or initial legal mask differs')
        old_t, new_t, old_s, new_s = active(before, target), active(after, target), active(before, setter), active(after, setter)
        field, own = after[target]['battlefield'][target], after[setter]['battlefield'][setter]
        if name.startswith('leech'):
            seeded = name != 'leech-grass' and (name != 'leech-switch' or number == 0)
            damage = max(1, old_t['MaxHP']//8) if seeded else 0
            healing = max(1, damage//2) if damage else 0
            require(field['volatile']['leech_seed'] is seeded and new_s['HP'] == min(new_s['MaxHP'], old_s['HP']+healing) and
                    new_t['HP'] == (old_t['HP']-damage if old_t['index'] == new_t['index'] else new_t['MaxHP']) and
                    new_t['index'] == (1 if name == 'leech-switch' and number else 0),
                    'Leech eighth drain/half-drain healing/Grass immunity/switch cure differs')
        elif name == 'curse-ghost':
            cost = max(1, old_s['MaxHP']//2) if number == 0 else 0
            require(field['volatile']['curse'] is True and new_t['HP'] == old_t['HP']-max(1, old_t['MaxHP']//4) and
                    new_s['HP'] == old_s['HP']-cost, 'Ghost Curse half-cost or quarter residual differs')
        elif name == 'curse-other':
            stages = list(w['initial'][setter]['battlefield'][setter]['stages'])
            stages[0] += 1
            stages[1] += 1
            stages[2] -= 1
            require(own['stages'] == stages and not field['volatile']['curse'] and
                    new_t['HP'] == old_t['HP'] and new_s['HP'] == old_s['HP'], 'Non-Ghost Curse stage/no-damage branch differs')
        elif name == 'perish':
            for side in (0, 1):
                count = after[side]['battlefield'][side]['PerishCount']
                if number < 3:
                    require(count == 3-number and active(after, side)['HP'] == active(before, side)['HP'] and
                            active(after, side)['index'] == 0, 'Perish source countdown/early faint differs')
                else:
                    require(count == 0 and after[side]['teams'][side][0]['HP'] == 0 and active(after, side)['index'] == 1 and
                            active(after, side)['HP'] == active(after, side)['MaxHP'], 'Perish both lead faints/replacements differ')
        elif name == 'future-switch':
            require(own['FutureSightCount'] == [0x12, 0x11, 0, 0][number] and new_t['index'] == (0 if number == 0 else 1) and
                    new_s['HP'] == old_s['HP'], 'Future Sight packed counter/switched target differs')
            require(0 < new_t['HP'] < old_t['HP'] if number == 2 else
                    new_t['HP'] == (old_t['HP'] if old_t['index'] == new_t['index'] else new_t['MaxHP']),
                    'Future Sight positive delayed hit or no-early-damage differs')
        elif name == 'wrap':
            count = field['WrapCount']
            if number == 0:
                require(count in (3, 4) and new_t['HP'] < old_t['HP'] and field['TrappingMove'] == moves[move] and
                        dict(type='SwitchAction', index=1) in masks[target], 'Wrap source hit/sampled initial duration differs')
            else:
                expected = max(0, before[target]['battlefield'][target]['WrapCount']-1)
                require(count == expected and new_t['HP'] == old_t['HP']-(max(1, old_t['MaxHP']//8) if expected else 0),
                        'Wrap decrement/residual/expiry differs')
            require((dict(type='SwitchAction', index=1) in probe['legal_masks'][target]) is (count == 0) and
                    new_s['HP'] == old_s['HP'], 'Wrap source switch-mask recovery differs')
        for side in (0, 1):
            for index, new in enumerate(after[side]['teams'][side]):
                old = before[side]['teams'][side][index]
                for field_name in ('index', 'species', 'form', 'egg', 'level', 'item', 'status', 'nature', 'ability_slot',
                                   'happiness', 'dvs', 'moves', 'pp_ups', 'gender', 'MaxHP', 'Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef'):
                    require(new[field_name] == old[field_name], 'Delayed permanent identity/moves/status changed: '+field_name)
                pp = list(old['pp'])
                if old['active'] and actions[side]['type'] == 'MoveAction':
                    pp[actions[side]['slot']] -= 1
                require(new['pp'] == pp and 0 <= new['HP'] <= new['MaxHP'], 'Delayed PP or HP bounds differ')
                if not old['active'] and not new['active']:
                    require(new['HP'] == old['HP'], 'Delayed inactive reserve HP changed')
        before, masks = after, probe['legal_masks']
    if name == 'leech-grass':
        return []
    handler = {'LEECH_SEED': 'HandleLeechSeed', 'CURSE': 'HandleCurse', 'PERISH_SONG': 'HandlePerishSong',
               'FUTURE_SIGHT': 'HandleFutureSight', 'WRAP': 'HandleWrap'}[move]
    keys = {'move:'+move, 'effect:'+effect_name}
    if name != 'curse-other':
        keys.add('endturn:'+handler)
    if name in ('leech', 'leech-switch'):
        keys.add('volatile:SUBSTATUS_LEECH_SEED')
    if name == 'curse-ghost':
        keys.add('volatile:SUBSTATUS_CURSE')
    require(all(key in rules and rules[key]['required'] for key in keys), 'Invented/non-required delayed rule')
    return sorted(keys)


def collect(args):
    current = coverage.inventory(args.root)
    require(read(args.inventory) == current, 'Current source inventory mismatch')
    source_contract(args.root)
    reports = [read(path) for path in args.report]
    require(reports and len(set(path.resolve() for path in args.report)) == len(reports), 'Duplicate/no delayed reports')
    require(len(args.run_log) == len(reports), 'Delayed successful run logs must match reports')
    first = reports[0]
    require(first.get('manifest_sha256') == MANIFEST and first.get('host_sources') == HOSTS, 'Unreviewed delayed execution checkpoint')
    retain, write, artifacts = artifact_sink(args.output)
    retain(args.inventory, 'raw/inventory.json')
    # Resolve actual executed bytes from the original report snapshots only.
    host_checkpoint = {}
    for name, expected in HOSTS.items():
        matches = [entry for path, entry in first['inputs'].items()
                   if Path(path).name == name and Path(path).parent.name == 'trainer_ai']
        require(len(matches) == 1 and matches[0]['sha256'] == expected, 'Missing reviewed delayed host identity: '+name)
        snapshot = Path(matches[0]['snapshot'])
        retain(snapshot, 'host/'+name, expected)
        host_checkpoint[name] = dict(sha256=expected, snapshot=str(snapshot))
    write('host/identity.json', dict(verified=True, files=host_checkpoint, basis='Original delayed report retained actual input bytes'))
    manifest, execution, oracle, oracle_artifacts, configuration, identity, symbols = bind_execution(
        args, current, first, HOSTS, retain, artifacts)
    rom = args.rom.read_bytes()
    def rom_bytes(label, offset, length):
        bank, address = symbols[label]
        require(0 <= offset and length > 0, 'Invalid delayed ROM table range')
        # BaseData deliberately spans banks through its contiguous linked ROM table.
        start = bank*0x4000+address-(0x4000 if bank else 0)+offset
        value = rom[start:start+length]
        require(len(value) == length, 'Delayed compiled table bytes unavailable')
        return value
    rules = {rule['key']:rule for rule in current['rules']}
    paths = ('ability', 'type', 'move', 'move_effect', 'pokemon')
    ids = [constants(args.root, 'constants/'+name+'_constants.asm') for name in paths]
    records, accepted, seen = [], set(), set()
    retained_inputs = set()
    index = 0
    for batch, (report_path, report) in enumerate(zip(args.report, reports)):
        require(report.get('scope') == 'bounded-source-delayed-duration-residual-entry-full-fights' and
                report.get('passed') is True and report.get('host_sources') == HOSTS and
                report.get('oracle_provenance') == oracle and report.get('game_sources') == first['game_sources'] and
                all(report.get(field) == value for field,value in identity.items()), 'Delayed report execution identity mismatch')
        require({path:entry['sha256'] for path,entry in report['inputs'].items()} ==
                {path:entry['sha256'] for path,entry in first['inputs'].items()},
                'Mixed/missing delayed executed input snapshots')
        require(set(report['game_sources']) == GAME_SOURCES and report.get('cases'), 'Delayed source identity set/cases incomplete')
        for path, expected in report['game_sources'].items():
            require(digest(args.root/path) == expected == manifest['sources'].get(path), 'Delayed executed source changed: '+path)
        retain(report_path, f'raw/report-{batch}.json')
        logged = [json.loads(line) for line in args.run_log[batch].read_text().splitlines() if line.strip()]
        log_fields = ('case', 'setter', 'seed', 'passed', 'replay_equal', 'turns', 'winner', 'comparisons',
                      'initial_wram_sha256', 'initial_memory_sha256', 'initial_wram_boundary')
        require([{key:row[key] for key in log_fields} for row in logged] ==
                [{key:row[key] for key in log_fields} for row in report['cases']],
                'Delayed successful run log/report rows differ')
        retain(args.run_log[batch], f'raw/run-{batch}.log')
        # Keep every original input byte, including generated native C/library.
        for original, entry in sorted(report['inputs'].items()):
            require(isinstance(entry.get('sha256'), str), 'Missing delayed input identity')
            snapshot = Path(entry['snapshot']).resolve()
            snapshot.relative_to(report_path.parent.resolve())
            require(digest(snapshot) == entry['sha256'], 'Delayed retained input bytes changed: '+original)
            input_key = entry['sha256'], Path(original).name
            if input_key not in retained_inputs:
                retain(snapshot, 'raw/input-'+input_key[0]+'-'+input_key[1], entry['sha256'])
                retained_inputs.add(input_key)
        for row in report['cases']:
            name, setter, seed = row['case'], row['setter'], row['seed']
            key = name, setter, seed
            require(name in CASES and type(setter) is int and setter in (0, 1) and type(seed) is int and seed == 193 and key not in seen,
                    'Unknown/duplicate delayed case')
            seen.add(key)
            path = Path(row['evidence']).resolve()
            path.relative_to(report_path.parent.resolve())
            raw = read(path)
            raw_ref = retain(path, f'raw/case-{index}.json', row['evidence_sha256'])
            require(raw.get('host_sources') == HOSTS and raw.get('game_sources') == report['game_sources'] and
                    raw.get('inputs') == report['inputs'] and raw.get('oracle_provenance') == oracle and
                    all(raw.get(field) == value for field,value in identity.items()) and
                    (raw.get('case'), raw.get('setter'), raw.get('seed')) == key, 'Delayed raw/report identity mismatch')
            require(row.get('passed') is True and row.get('replay_equal') is True and raw.get('terminal') is True and
                    raw.get('failed') is False and raw.get('truncated') is False and raw.get('winner') == row['winner'] and
                    raw.get('scope') == 'ordinary-trainer-fixed-level-source-controller', 'Delayed source terminal/exact replay missing')
            require(raw.get('options') == dict(initial=131, initial2=6, link=0, tower=0, battle_mode=2) and
                    raw.get('initial_normalized_state_sha256') and raw.get('initial_boundaries') and raw.get('decision_boundary'),
                    'Delayed initial options/state/boundary missing')
            initial_contract = raw['initial_contract']
            require(initial_contract['options'] == raw['options'] and initial_contract['scenario'] == raw['scenario'] and
                    initial_contract['decision_boundary'] == raw['decision_boundary'] and
                    initial_contract['fixed_party'] == raw['fixed_party'] and initial_contract['turns'] == [] and
                    initial_contract['terminal'] is False and initial_contract['failed'] is False and
                    initial_contract['truncated'] is False, 'Delayed initial/final fixed-party contract differs')
            segments, comparisons, turns = raw['recorded_segments'], raw['comparisons'], raw['turns']
            require(len(turns) == row['turns'] and turns and turns[-1]['terminal'] is True and
                    turns[-1]['truncated'] is False and row['comparisons'] == len(segments) == len(comparisons) > 0 and
                    all(segment.get('kind') in ('segment', 'one') and segment.get('state') for segment in segments) and
                    all(isinstance(comparison.get('event'), str) and type(comparison.get('div_events')) is int and
                        comparison['div_events'] >= 0 for comparison in comparisons), 'Delayed full-fight differential records missing')
            case_rules = validate_witness(args.root, name, setter, raw, row, rules, ids, rom_bytes, symbols)
            wram = Path(row['initial_wram']).resolve()
            wram.relative_to(report_path.parent.resolve())
            require(wram.stat().st_size == 8192 and raw.get('initial_wram') == row['initial_wram'] and
                    raw.get('initial_wram_sha256') == row['initial_wram_sha256'] and
                    raw.get('initial_wram_boundary') == row.get('initial_wram_boundary') == BOUNDARY,
                    'Delayed standard initial WRAM/path/hash/boundary missing')
            wram_ref = retain(wram, f'raw/initial-wram-{index}.bin', row['initial_wram_sha256'])
            memory = Path(row['initial_memory']).resolve()
            memory.relative_to(report_path.parent.resolve())
            require(raw.get('initial_memory') == row['initial_memory'] and
                    raw.get('initial_memory_sha256') == row['initial_memory_sha256'] and
                    memory.stat().st_size == 0x6000+8192+127+1 and memory.read_bytes()[0x6000:0x8000] == wram.read_bytes(),
                    'Delayed initial full memory differs from plain WRAM')
            retain(memory, f'raw/initial-memory-{index}.bin', row['initial_memory_sha256'])
            index += 1
            if not case_rules:
                continue
            record = dict(case_id=f'delayed-{name}-{setter}-{seed}', passed=True, unsupported=False,
                complete_fight=True, full_state_comparisons=len(segments),
                full_state_components=['cpu', 'hram', 'wram-banks-0-7', 'svbk'],
                initial_wram=wram_ref, raw_case=raw_ref, rule_keys=case_rules,
                choice_contract=dict(roles=configuration['roles'], player_bag=True))
            coverage.runtime_case_options(record, {artifact['path']:artifact for artifact in artifacts}, symbols,
                                          args.output, configuration, current)
            records.append(record)
            accepted.update(case_rules)
    require(seen == {(name, side, 193) for name in CASES for side in (0, 1)} and index == 16 and len(records) == 14,
            'Delayed exact sixteen cases with fourteen positive records are required')
    cases_ref = write('cases.json', dict(manifest_sha256=identity['manifest_sha256'], configuration=configuration, cases=records))
    adapter = dict(**identity, inventory_sha256=current['inventory_sha256'], configuration=configuration,
        acceptance_scope='conditional-native-gameplay', oracle_kind='same-presentation-hooks-actual-rom-differential',
        failed_cases=0, unsupported_cases=0, complete_fights=len(records),
        full_state_comparisons=sum(record['full_state_comparisons'] for record in records), rule_keys=sorted(accepted),
        execution_artifacts=execution, oracle_artifacts=oracle_artifacts, case_reports=[cases_ref], artifacts=list(artifacts),
        limitations=first['limits']+['Exact replay and source before/after capture are bound to the pinned executed driver.',
            'Two Grass nonactivation cases are retained supporting controls without positive rule claims.'],
        metadata_basis='Reviewed 23-file delayed checkpoint plus exact source/ROM HP fractions, packed counters, stages, PP, legal masks and replacements.')
    return finish(current, configuration, adapter, 'source-delayed', args.output, write,
        dict(collected_cases=len(records), executed_cases=index, collected_rules=sorted(accepted),
             rounds=sum(row['turns'] for report in reports for row in report['cases'])))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=ROOT)
    parser.add_argument('--report', type=Path, action='append', required=True)
    parser.add_argument('--run-log', type=Path, action='append', required=True)
    for name in ('inventory', 'manifest', 'rom', 'symbols', 'build-receipt', 'build-log',
                 'oracle-library', 'mgba-library', 'output'):
        parser.add_argument('--'+name, type=Path, required=True)
    try:
        return collect(parser.parse_args())
    except (ValueError, KeyError, OSError, TypeError, IndexError, StopIteration) as error:
        parser.exit(2, 'Delayed evidence collection rejected: '+str(error)+'\n')


if __name__ == '__main__':
    raise SystemExit(main())
