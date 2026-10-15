#!/usr/bin/env python3
"""Retain typed trainer inventory witnesses without importing battle drivers."""
from __future__ import annotations

import argparse
import json
import re
from pathlib import Path

import battle_coverage as coverage
from battle_family_evidence import artifact_sink, bind_execution, finish, digest, read, require

ROOT = Path(__file__).resolve().parents[2]
MANIFEST = 'dec3cb17a34aeeb9e0f36e44590d0890b7d0816ddff98fc7bb0346b9b32737b0'
HOSTS = {
    "battle_controller_trainer_items.py": "908f767650f8e7245191ab4c897fd817d4547df9b275ad4de2c1f44a23f7bc82",
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
GAME_HASHES = {
    "engine/battle/ai/items.asm": "53629d1377a7b85b31a85e5625f2e55361b15e0c709f189f64b3970839a0e18b",
    "engine/battle/stats.asm": "e776c46a01773df01156ecf021ea4d5a24e3f0926d3dad4385d5c32fc47d06e6",
    "engine/battle/endturn.asm": "94b649be025a90fd6730b92caedd5416d9b23bd17052dfe59c4772d081685563",
    "engine/battle/effect_commands.asm": "1a702f8cbdd03b93c0811350bd47c20b601558f065ca87f1a388b2b7855dc608",
    "engine/battle/core.asm": "c70c34b834411bfe4ecb36fb98ac7881a3876eb3429eaa03cf2745eb79cbae65",
    "home/battle.asm": "2c718f1d14a728b174701e10e4999720ae224679e39bc0d4f2a23619ee21a70c",
    "engine/items/item_effects.asm": "c78886dafabf2a4a98173012fe44b916fe08d55fa63f88534ce101e7ce4dd34a",
    "data/items/attributes.asm": "1d4ab98336670891392660f0655b116a5eeb60e94641028a7fa1464e681bda42",
    "constants/item_constants.asm": "a060d98acce657700db9dd6e07fb86d783b84307d6dd4d50f44b4810d4b13423",
    "constants/item_data_constants.asm": "56a81c46929da1b47cdbf1a8c319a8b29d19704a8dbacde5cda773b1c6256f88",
    "constants/battle_constants.asm": "e4d1319a050241bc7efeba533c6370323aae87b5bbbe913787e7a3f597df1649",
    "constants/pokemon_constants.asm": "6b92811d43d304c0b44deb78e81ee3f1ae2889caa0d14b8da080014b02ebc7cb",
    "constants/pokemon_data_constants.asm": "dbf901faf06638eabf556d447ce0eebdcb80bd543b2665f5b3048caa41aaea30",
    "constants/ability_constants.asm": "87b2f29bb5e6bdf3b60970512a0b78f58da778bc56705b916bde1386123d1dff",
    "constants/type_constants.asm": "afc5f619d21132ac54f771081344a5585ac2549b1ef22c17752351c1518b4799",
    "constants/move_constants.asm": "80cb65ae226fa4151d8d813e8aef1de2edfd1a0091a2941424cbef53ccee7ec1",
    "data/pokemon/base_stats.asm": "2d0f8419ef73390b40a55e5c7f95c8009fd028f2b2e0bf640089180b54dd78c1",
    "data/pokemon/base_stats/horsea.asm": "640bc72ace5c12840c9ee20da7f5a7bfd42eafd8f3835cc3e8deed5aafe37aa9"
}
BOUNDARY = 'post-reset-first-frozen-actor-decision'
SNAPSHOT_FOLDER = 'trainer-item-inputs'
CASES = ('POTION', 'SUPER_POTION', 'HYPER_POTION', 'MAX_POTION', 'FULL_HEAL',
         'FULL_RESTORE', 'GUARD_SPEC', 'DIRE_HIT', 'X_ATTACK', 'X_DEFEND',
         'X_SPEED', 'X_SP_ATK', 'X_SP_DEF', 'X_ACCURACY')
HEALS = {'POTION', 'SUPER_POTION', 'HYPER_POTION', 'MAX_POTION', 'FULL_RESTORE'}
EFFECT_HANDLERS = {
    'POTION': 'EnemyUsedPotion', 'SUPER_POTION': 'EnemyUsedSuperPotion',
    'HYPER_POTION': 'EnemyUsedHyperPotion', 'MAX_POTION': 'EnemyUsedMaxPotion',
    'FULL_HEAL': 'EnemyUsedFullHeal', 'FULL_RESTORE': 'EnemyUsedFullRestore',
    'GUARD_SPEC': 'EnemyUsedGuardSpec', 'DIRE_HIT': 'EnemyUsedDireHit',
    **{name: 'EnemyUsedXItem' for name in
       ('X_ATTACK', 'X_DEFEND', 'X_SPEED', 'X_SP_ATK', 'X_SP_DEF', 'X_ACCURACY')},
}


def constants(root, path):
    result, value = {}, 0
    def integer(text):
        if text.startswith('$'):
            return int(text[1:], 16)
        if text.startswith('%'):
            return int(text[1:], 2)
        return int(text, 0)
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
        elif re.fullmatch(r'DEF\s+[A-Z_]+\s+EQU\s+(?:\$[0-9a-fA-F]+|%[01]+|\d+)', code):
            _, name, _, literal = code.split()
            result[name] = integer(literal)
    return result


def source_contract(root, rules, ids, rom_bytes, symbols):
    for path, expected in GAME_HASHES.items():
        require(digest(root/path) == expected, 'Reviewed trainer item source changed: '+path)
    items, fields, abilities, moves, species = ids
    macro = (root/'data/items/attributes.asm').read_text().split('MACRO item_attribute', 1)[1].split('ENDM', 1)[0]
    macro = [line.split(';', 1)[0].strip() for line in macro.splitlines()]
    require([line for line in macro if line] == [r'dw \1', r'db \2, \3, \4', r'dn \5, \6'],
            'Trainer source item attribute layout changed')
    source = (root/'engine/battle/ai/items.asm').read_text()
    table = source.split('AI_Items:', 1)[1].split('db $ff', 1)[0]
    entries = re.findall(r'^\s*dbw ([A-Z_]+),\s*(\.[A-Za-z]+)', table, re.M)
    require(len(entries) == 14 and {name for name, _ in entries} == set(CASES),
            'Trainer source AI item table mapping changed')
    for index, (name, local) in enumerate(entries):
        rule = rules['trainer-item:'+name]
        label = 'AI_Items'+local
        require(rule['handler'] == label and rule['source'] == 'engine/battle/ai/items.asm',
                'Trainer inventory wrapper mapping differs from source rule')
        address = symbols[label][1]
        require(rom_bytes('AI_Items', 3*index, 3) == bytes((items[name], address & 255, address >> 8)),
                'Trainer compiled item/wrapper mapping differs')
        start = source.index('\n'+local+':')+len(local)+2
        following = [source.find('\n'+other+':', start) for _, other in entries if other != local]
        end = min([position for position in following if position >= 0]+[len(source)])
        wrapper = source[start:end]
        require(('call '+EFFECT_HANDLERS[name] in wrapper or 'jmp '+EFFECT_HANDLERS[name] in wrapper) and
                (not name.startswith('X_') or 'ld a, '+name in wrapper),
                'Trainer table wrapper no longer calls the reviewed item effect')
    used = source.split('.used_item', 1)[1].split('.IsHighestLevel:', 1)[0]
    require('ld [de], a' in used and 'ld [wEnemyUsingItem], a' in used and
            'ld [wCurEnemyMove], a' in used and 'call UpdateEnemyMonInParty' in
            source.split('AIUpdateHUD:', 1)[1].split('AIUsedItemSound:', 1)[0],
            'Trainer source inventory consumption/cache owner changed')
    require('call AI_HealStatus' in source.split('EnemyUsedFullHeal:', 1)[1].split('EnemyUsedMaxPotion:', 1)[0] and
            'call AI_HealStatus' in source.split('EnemyUsedFullRestore:', 1)[1].split('FullRestoreContinue:', 1)[0] and
            'wEnemyMonMaxHP' in source.split('FullRestoreContinue:', 1)[1].split('EnemyUsedPotion:', 1)[0] and
            'ld [wEnemyMonStatus], a' in source.split('AI_HealStatus:', 1)[1].split('EnemyUsedGuardSpec:', 1)[0],
            'Trainer source full restoration/status effect changed')
    stats = [code for _, code in coverage.lines(root, 'engine/battle/stats.asm')]
    scaffold = ['ld a, [wChangedStat]', 'and $f0', 'swap a', 'inc a', 'ld c, a']
    require(sum(stats[index:index+5] == scaffold for index in range(len(stats)-4)) == 1 and
            'inc [hl]' in stats and 'cp MAX_STAT_LEVEL' in stats,
            'Trainer source X item amount/cap decoding changed')
    x = source.split('EnemyUsedXItem:', 1)[1].split('PrintText_UsedItemOn:', 1)[0]
    require(x.index('farcall GetItemHeldEffect') < x.index('ld b, c') < x.index('farcall _ForceRaiseStat') and
            'ld a, [wFailedMessage]' in x and 'scf' in x,
            'Trainer source item-parameter/stat-handler chain changed')
    endturn = (root/'engine/battle/endturn.asm').read_text()
    mist = endturn.split('HandleMist:', 1)[1].split('GetTurnAndPlacePrefix:', 1)[0]
    require('call HandleMist' in endturn.split('HandleBetweenTurnEffects:', 1)[1].split('HandleWeather:', 1)[0] and
            'ld hl, wEnemyGuards' in mist and 'DecrementHighNibble:' in mist and
            'sub $10' in mist and 'ld [hl], a' in mist and fields['GUARD_MIST'] == 0xf0,
            'Trainer source Mist end-turn expiry path changed')
    bank, start = symbols['HandleBetweenTurnEffects']
    end_bank, end = symbols['HandleEndturnBlockA']
    mist_bank, mist_address = symbols['HandleMist']
    require(bank == end_bank == mist_bank and end > start and
            rom_bytes('HandleBetweenTurnEffects', 0, end-start).count(
                bytes((0xcd, mist_address & 255, mist_address >> 8))) == 1 and
            rom_bytes('DecrementHighNibble', 0, 8) == bytes.fromhex('7ed610d877d610d0'),
            'Trainer compiled Mist caller/high-nibble countdown changed')
    path = 'data/pokemon/base_stats/horsea.asm'
    active = [code for _, code in coverage.lines(root, path)]
    require('abilities_for HORSEA, SWIFT_SWIM, SNIPER, DAMP' in active,
            'Trainer neutral Horsea source ability changed')
    includes = re.findall(r'^INCLUDE "(data/pokemon/base_stats/[^"]+)"',
        (root/'data/pokemon/base_stats.asm').read_text(), re.M)
    origin = symbols['wCurBaseData'][1]
    stride = symbols['wCurBaseDataEnd'][1]-origin
    record = rom_bytes('BaseData', includes.index(path)*stride, stride)
    ability = abilities['SWIFT_SWIM']
    require(record[symbols['wBaseAbility1'][1]-origin] == ability,
            'Trainer neutral Horsea compiled ability changed')
    return source, ability


def item_metadata(root, name, source, ability, ids, rom_bytes, symbols):
    items, fields, _, _, _ = ids
    identifier = items[name]
    handler = EFFECT_HANDLERS[name]
    attributes = (root/'data/items/attributes.asm').read_text()
    records = re.findall(r'^; ([^\n]+)\n\s*item_attribute ([^\n;]+)', attributes, re.M)
    require(records[identifier-1][0] == name.replace('_', ' ')+('.' if name == 'GUARD_SPEC' else ''),
            'Trainer source attribute table membership differs')
    parameter = records[identifier-1][1].split(',')[2].strip()
    record = rom_bytes('ItemAttributes', (identifier-1)*6, 6)
    metadata = dict(item=name, identifier=identifier, handler=handler,
        compiled_attributes=record.hex(), pocket=record[4] & 15, context=record[5] & 15,
        source_attribute_parameter=parameter)
    if name in ('POTION', 'SUPER_POTION', 'HYPER_POTION'):
        match = re.search(r'^'+handler+r':\n\s*ld a, '+name+r'\n\s*ld b, (\d+)\s*$', source, re.M)
        require(match is not None, 'Trainer source potion literal scaffold changed')
        amount = int(match[1])
        require(amount == {'POTION': 20, 'SUPER_POTION': 50, 'HYPER_POTION': 200}[name] and
                rom_bytes(handler, 0, 4) == bytes((0x3e, identifier, 0x06, amount)),
                'Trainer compiled potion literal differs from source')
        metadata['heal_amount'] = amount
    elif name.startswith('X_'):
        match = re.fullmatch(r'\$([0-9a-fA-F]+) \| ([A-Z_]+)', parameter)
        require(match is not None, 'Trainer source X item parameter changed')
        stat, amount = fields[match[2]], int(match[1], 16)
        require(0 <= stat < 6 and not amount & 15 and record[3] == (amount | stat),
                'Trainer compiled X stat/amount differs from source')
        metadata.update(stat=stat, stat_name=match[2], stage_increment=(amount >> 4)+1)
    elif name == 'GUARD_SPEC':
        match = re.search(r'^EnemyUsedGuardSpec:\n\s*call AIUsedItemSound\n\s*ld a, (\d+) << 4', source, re.M)
        require(match is not None, 'Trainer source Guard Spec duration missing')
        duration = int(match[1])
        require(duration == 5 and rom_bytes(handler, 3, 2) == bytes((0x3e, duration << 4)),
                'Trainer compiled Guard Spec duration differs')
        metadata['duration'] = duration
    elif name == 'DIRE_HIT':
        bit, address = fields['SUBSTATUS_FOCUS_ENERGY'], symbols['wEnemySubStatus4'][1]
        require(re.search(r'^EnemyUsedDireHit:\n\s*call AIUsedItemSound\n\s*ld hl, wEnemySubStatus4\n\s*set SUBSTATUS_FOCUS_ENERGY, \[hl\]', source, re.M) and
                rom_bytes(handler, 3, 5) == bytes((0x21, address & 255, address >> 8, 0xcb, 0xc6+8*bit)),
                'Trainer compiled Dire Hit focus-energy bit differs')
    metadata.update(ability=ability, status=(1 << fields['PAR']) if name in ('FULL_HEAL', 'FULL_RESTORE') else 0)
    return metadata


def validate_witness(name, slot, raw, row, probe, metadata, ids, rules, rom_bytes):
    items, fields, _, moves, species = ids
    rounds = 6 if name == 'GUARD_SPEC' else 1
    inventory = ['DIRE_HIT', 'DIRE_HIT']
    inventory[slot] = name
    expected_inventory = [items[value] for value in inventory]
    expected_move_ids = [moves['SPLASH'], moves['WATER_GUN'], 0, 0]
    expected_initial_pp = [rom_bytes('Moves', (value-1)*8, 8)[5] if value else 0 for value in expected_move_ids]
    scenario = raw['scenario']
    require(scenario['inventory'] == inventory and scenario['bag'] == [] and
            scenario['initial_options'] == 131 and scenario['initial_options2'] == 6 and
            len(scenario['player']) == len(scenario['enemy']) == 1,
            'Trainer fixture roster/inventory/options differ')
    for side, team in enumerate((scenario['player'], scenario['enemy'])):
        require(team[0] == dict(species='HORSEA', moves=['SPLASH', 'WATER_GUN'], level=50,
            ability_slot=0, item='NO_ITEM', form=0,
            status=metadata['status'] if side else 0, nature=0, dvs=[0]*3, evs=[0]*6,
            happiness=0, hp=1 if side and name in HEALS else None, female=False, egg=False, pp=None),
            'Trainer fixture no longer isolates reviewed source inputs')
    witness = raw['witness']
    require(witness == row['witness'] and witness.get('scope') ==
            'actual-enemy-trainer-item-positive-effect-and-complete-fight' and
            witness.get('item') == name and witness.get('slot') == slot and witness.get('enemy_side') == 1 and
            witness.get('metadata') == metadata and witness['inventory_before'] == expected_inventory,
            'Trainer reconstructed source metadata/witness identity differs')
    transitions, caches, before = probe['transitions'], probe['caches'], probe['initial']
    require(len(transitions) == len(caches) == rounds and len(witness['probes']) == 2*rounds and
            len(raw['turns']) >= rounds and len(before) == 2,
            'Trainer complete positive probe interval missing')
    require(before[0]['teams'] == before[1]['teams'] and before[0]['battlefield'] == before[1]['battlefield'],
            'Trainer initial public state views differ')
    base_stats = raw['fixed_party']['base_stats']
    for side in (0, 1):
        view, member, field = before[side], before[side]['teams'][side][0], before[side]['battlefield'][side]
        require(len(view['teams'][0]) == len(view['teams'][1]) == 1 and
                member['HP'] == (1 if side and name in HEALS else member['MaxHP']) and
                member['status'] == (metadata['status'] if side else 0) and
                member['moves'] == expected_move_ids and member['pp'] == expected_initial_pp and
                member['pp_ups'] == [0]*4 and field['stages'] == [fields['BASE_STAT_LEVEL']]*7 and
                all(value is False for value in field['volatile'].values()) and
                all(field[key] == 0 for key in ('mist_turns', 'safeguard_turns', 'reflect_turns',
                    'light_screen_turns', 'spikes', 'toxic_spikes')) and
                view['weather'] == view['weather_turns'] == view['trick_room'] == 0,
                'Trainer initial health/status/stage/volatile/weather condition differs')
        base = next(record for record in base_stats if record['side'] == side and record['index'] == 0)
        require(all(member[key] == base[key] for key in ('MaxHP', 'Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef')),
                'Trainer initial source base-stat identity differs')
    require(before[1]['trainer_items'] == expected_inventory and before[0]['trainer_items'] == [],
            'Trainer initial enemy-only inventory differs')
    expected_order = [(round_index, side) for round_index in range(rounds) for side in (0, 1)]
    require([(fact['round'], fact['side']) for fact in witness['probes']] == expected_order,
            'Trainer witness probe ordering differs')
    for round_index, transition in enumerate(transitions):
        actions = [dict(type='MoveAction', slot=0),
            dict(type='ItemAction', slot=slot) if round_index == 0 else dict(type='MoveAction', slot=0)]
        after = transition['observations']
        require(transition['turn'] == round_index+1 and transition['actions'] == actions ==
                raw['turns'][round_index]['actions'] and len(after) == 2 and
                after[0]['teams'] == after[1]['teams'] and after[0]['battlefield'] == after[1]['battlefield'],
                'Trainer source probe submitted actions/public observations differ')
        expected_inventory[slot] = 0
        require(after[1]['trainer_items'] == expected_inventory and after[0]['trainer_items'] == [],
                'Trainer selected inventory slot consumption or unselected slot preservation differs')
        require(all(view['weather'] == view['weather_turns'] == view['trick_room'] == 0 for view in after),
                'Trainer isolated probe changed battlefield weather')
        for side in (0, 1):
            old, new = before[side]['teams'][side][0], after[side]['teams'][side][0]
            previous_field, field = before[side]['battlefield'][side], after[side]['battlefield'][side]
            fact = witness['probes'][round_index*2+side]
            require(fact['before'] == old and fact['after'] == new and
                    fact['stages_before'] == previous_field['stages'] and fact['stages_after'] == field['stages'] and
                    fact['focus_energy'] is field['volatile']['focus_energy'] and fact['mist_turns'] == field['mist_turns'] and
                    fact['stored'] == caches[round_index][side], 'Trainer source probe/witness/cache views differ')
            hp = old['HP']
            if side == 1 and round_index == 0 and name in HEALS:
                hp = min(old['MaxHP'], hp+metadata['heal_amount']) if 'heal_amount' in metadata else old['MaxHP']
                require(hp > old['HP'], 'Trainer restoration lacks a positive source HP change')
            status = 0 if side == 1 and round_index == 0 and name in ('FULL_HEAL', 'FULL_RESTORE') else old['status']
            require(new['HP'] == hp and new['status'] == status and 0 < hp <= new['MaxHP'],
                    'Trainer source HP restoration/status effect differs')
            stages = list(previous_field['stages'])
            if side == 1 and round_index == 0 and name.startswith('X_'):
                stages[metadata['stat']] = min(fields['MAX_STAT_LEVEL'], stages[metadata['stat']]+metadata['stage_increment'])
            require(field['stages'] == stages, 'Trainer source stat target or stage increment differs')
            mist = max(0, metadata['duration']-round_index-1) if side == 1 and name == 'GUARD_SPEC' else 0
            focus = side == 1 and name == 'DIRE_HIT'
            require(field['mist_turns'] == mist, 'Trainer source Guard Spec end-turn countdown differs')
            require(field['volatile']['focus_energy'] is focus, 'Trainer source Dire Hit focus-energy flag differs')
            for volatile, value in field['volatile'].items():
                require(value is (focus if volatile == 'focus_energy' else False),
                        'Trainer isolated probe changed another volatile flag')
            require(all(field[key] == 0 for key in ('safeguard_turns', 'reflect_turns', 'light_screen_turns',
                    'spikes', 'toxic_spikes')), 'Trainer isolated probe changed another field guard')
            pp = list(old['pp'])
            if side == 0 or round_index:
                pp[0] -= 1
            require(new['pp'] == pp, 'Trainer opposing PP use or own item-turn PP preservation differs')
            for member in (old, new):
                require(member['index'] == 0 and member['active'] is True and member['species'] == species['HORSEA'] and
                        member['ability'] == metadata['ability'] and member['ability_slot'] == 1 and member['item'] == 0 and
                        member['level'] == 50 and member['moves'] == expected_move_ids and member['pp_ups'] == [0]*4,
                        'Trainer observed neutral source species/ability/move identity differs')
            for key in ('index', 'species', 'form', 'egg', 'level', 'item', 'nature', 'ability_slot',
                        'happiness', 'dvs', 'moves', 'pp_ups', 'gender', 'MaxHP', 'Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef', 'types'):
                require(new[key] == old[key], 'Trainer probe changed permanent neutral identity: '+key)
            cache = caches[round_index][side]
            require(cache['side'] == side and cache['index'] == 0 and set(cache['caches']) == {'party', 'battle'},
                    'Trainer source party/active cache identity differs')
            require(all(all(cached[key] == new[key] for key in ('HP', 'status', 'MaxHP', 'item', 'pp'))
                        for cached in cache['caches'].values()), 'Trainer source party/active cache values differ')
        before = after
    require(witness['inventory_after'] == expected_inventory, 'Trainer witness final inventory differs')
    keys = {'trainer-item:'+name}
    if name == 'GUARD_SPEC':
        require([transition['observations'][1]['battlefield'][1]['mist_turns'] for transition in transitions] == [4, 3, 2, 1, 0, 0],
                'Trainer Guard Spec lacks positive expiry and clear-round source witness')
        keys.update(('field:GUARD_MIST', 'endturn:HandleMist'))
    if name == 'DIRE_HIT':
        require(probe['initial'][1]['battlefield'][1]['volatile']['focus_energy'] is False and
                transitions[0]['observations'][1]['battlefield'][1]['volatile']['focus_energy'] is True,
                'Trainer Dire Hit lacks positive source false-to-true witness')
        keys.add('volatile:SUBSTATUS_FOCUS_ENERGY')
    require(all(key in rules and rules[key]['required'] for key in keys), 'Invented/non-required trainer item claim')
    return sorted(keys)


def validate_initial_wram(payload, probe, raw, symbols, fields):
    def data(label, size=1):
        address = symbols[label][1]-0xc000
        require(0 <= address <= 8192-size, 'Trainer initial WRAM field lies outside retained image: '+label)
        return payload[address:address+size]
    def byte(label):
        return data(label)[0]
    def word(label):
        return int.from_bytes(data(label, 2), 'big')
    require([byte('wEnemyTrainerItem1'), byte('wEnemyTrainerItem2')] == probe['initial'][1]['trainer_items'] and
            byte('wCurBattleMon') == byte('wCurOTMon') == 0,
            'Trainer initial source WRAM active/inventory identity differs')
    for side in (0, 1):
        mon = probe['initial'][side]['teams'][side][0]
        for stem in (('wBattleMon', 'wEnemyMon')[side], ('wPartyMon1', 'wOTPartyMon1')[side]):
            require(word(stem+'HP') == mon['HP'] and byte(stem+'Status') == mon['status'] and
                    list(data(stem+'PP', 4)) == mon['pp'] and list(data(stem+'Moves', 4)) == mon['moves'] and
                    all(word(stem+key) == mon[key] for key in ('MaxHP', 'Attack', 'Defense', 'Speed', 'SpAtk', 'SpDef')),
                    'Trainer initial source WRAM party/active cache differs')
        prefix = ('wPlayer', 'wEnemy')[side]
        field = probe['initial'][side]['battlefield'][side]
        require([byte(prefix+name) for name in ('AtkLevel', 'DefLevel', 'SpeLevel', 'SAtkLevel', 'SDefLevel', 'AccLevel', 'EvaLevel')] == field['stages'] and
                byte(prefix+'Guards') >> 4 == field['mist_turns'] and
                bool(byte(prefix+'SubStatus4') & (1 << fields['SUBSTATUS_FOCUS_ENERGY'])) is field['volatile']['focus_energy'],
                'Trainer initial source WRAM stages/flags differ')
        party = ('wPartyMon1', 'wOTPartyMon1')[side]
        fixed = raw['fixed_party']['level_xp'][side]
        require(byte(party+'Level') == fixed['level'] and data(party+'Exp', 3).hex() == fixed['xp_hex'],
                'Trainer initial source WRAM level/XP differs')


def collect(args):
    current = coverage.inventory(args.root)
    require(read(args.inventory) == current, 'Current source inventory mismatch')
    reports = [read(path) for path in args.report]
    require(len(reports) == len(args.run_log) == 3 and len({path.resolve() for path in args.report}) == 3,
            'Exactly three distinct trainer reports and matching successful run logs are required')
    first = reports[0]
    require(first.get('manifest_sha256') == MANIFEST and first.get('host_sources') == HOSTS,
            'Unreviewed trainer item execution checkpoint')
    shared = Path(first['shared_snapshot_root']).resolve()
    require(shared == args.report[0].parent.parent.resolve()/SNAPSHOT_FOLDER,
            'Trainer shared snapshot root is outside the reviewed sibling folder')
    retain, write, artifacts = artifact_sink(args.output)
    retain(args.inventory, 'raw/inventory.json')
    checkpoint = {}
    for name, expected in HOSTS.items():
        matches = [entry for path, entry in first['inputs'].items()
            if Path(path).name == name and Path(path).parent.name == 'trainer_ai']
        require(len(matches) == 1 and matches[0]['sha256'] == expected,
                'Missing reviewed trainer host identity: '+name)
        snapshot = Path(matches[0]['snapshot']).resolve()
        require(snapshot.parent == shared, 'Trainer host snapshot escaped shared root')
        retain(snapshot, 'host/'+name, expected)
        checkpoint[name] = dict(sha256=expected, snapshot=str(snapshot))
    write('host/identity.json', dict(verified=True, files=checkpoint,
        basis='Original trainer reports retain the reviewed 23 executed host files'))
    manifest, execution, oracle, oracle_artifacts, configuration, identity, symbols = bind_execution(
        args, current, first, HOSTS, retain, artifacts)
    rom = args.rom.read_bytes()
    def rom_bytes(label, offset, length):
        bank, address = symbols[label]
        require(type(offset) is int and offset >= 0 and type(length) is int and length > 0,
                'Invalid trainer ROM table range')
        start = bank*0x4000+address-(0x4000 if bank else 0)+offset
        result = rom[start:start+length]
        require(len(result) == length, 'Trainer compiled table bytes unavailable')
        return result
    rules = {rule['key']:rule for rule in current['rules']}
    ids = [constants(args.root, 'constants/'+name+'_constants.asm') for name in ('item', 'battle', 'ability', 'move', 'pokemon')]
    source, ability = source_contract(args.root, rules, ids, rom_bytes, symbols)
    # Bind snapshot bytes to actual runtime/assembler identities, including the
    # Oracle dylib loaded at execution time, not merely the supplied library.
    expected_inputs = [(args.manifest, MANIFEST), (args.rom, identity['rom_sha256']),
        (args.symbols, identity['symbols_sha256']), (args.build_receipt, digest(args.build_receipt)),
        (args.build_log, read(args.build_receipt)['build_log_sha256']),
        (Path(manifest['library_path']), manifest['library_sha256']),
        (args.manifest.parent/'stateful.c', manifest['generated_c_sha256'])]
    for kind in ('source', 'library', 'libmgba'):
        expected_inputs.append((Path(oracle[kind]['path']), oracle[kind]['sha256']))
    for abi in ('State', 'Hook'):
        entry = oracle['abi_sources'][abi]
        expected_inputs.append((Path(entry['path']), entry['sha256']))
    for path, expected in expected_inputs:
        entry = first['inputs'].get(str(path))
        require(entry and entry.get('sha256') == expected,
                'Trainer executed runtime/Oracle input identity differs: '+str(path))
    profiles = [entry for path, entry in first['inputs'].items() if Path(path).name == 'controller-profile.json']
    freezes = [entry for path, entry in first['inputs'].items() if Path(path).name == 'frozen-inputs.json']
    require(len(profiles) == len(freezes) == 1, 'Trainer prepared profile/freeze provenance missing')
    profile = read(Path(profiles[0]['snapshot']))
    frozen = read(Path(freezes[0]['snapshot']))
    require(profile['audit'] == manifest['controller_audit'] and frozen['host_sources'] == HOSTS and
            frozen['game_sources'] == GAME_HASHES and frozen['manifest_sha256'] == MANIFEST and
            frozen['controller_profile_sha256'] == profiles[0]['sha256'],
            'Trainer prepared source/profile freeze differs')
    records, accepted, seen, pending_inputs = [], set(), set(), {}
    index = 0
    for batch, (report_path, report, log_path) in enumerate(zip(args.report, reports, args.run_log)):
        require(report.get('scope') == 'bounded-source-enemy-trainer-inventory-full-fights' and
                report.get('passed') is True and report.get('host_sources') == HOSTS and
                report.get('game_sources') == GAME_HASHES and report.get('oracle_provenance') == oracle and
                all(report.get(field) == value for field, value in identity.items()),
                'Trainer report execution/source identity differs')
        require(Path(report['shared_snapshot_root']).resolve() == shared == report_path.parent.parent.resolve()/SNAPSHOT_FOLDER,
                'Trainer report shared snapshot root differs from reviewed sibling folder')
        require(report['inputs'] == first['inputs'] and len(report['inputs']) == 52,
                'Trainer reports mix or omit executed input snapshots')
        for path, expected in GAME_HASHES.items():
            matches = [entry for original, entry in report['inputs'].items()
                if original.endswith('/build/stateful/game/'+path)]
            require(manifest['sources'].get(path) == expected and len(matches) == 1 and matches[0]['sha256'] == expected,
                    'Trainer executed game source identity differs: '+path)
        retain(report_path, f'raw/report-{batch}.json')
        logged = [json.loads(line) for line in log_path.read_text().splitlines() if line.strip()]
        require(logged == [{key:value for key,value in row.items() if key != 'witness'} for row in report['cases']],
                'Trainer successful run log/report rows differ')
        retain(log_path, f'raw/run-{batch}.log')
        for original, entry in sorted(report['inputs'].items()):
            snapshot, expected = Path(entry['snapshot']).resolve(), entry['sha256']
            require(snapshot.parent == shared and snapshot.name == expected+'-'+Path(original).name,
                    'Trainer snapshot path differs from content-addressed shared root')
            input_key = expected, Path(original).name
            if input_key not in pending_inputs:
                require(digest(snapshot) == expected, 'Trainer retained execution input bytes changed: '+original)
                pending_inputs[input_key] = snapshot
        for row in report['cases']:
            name, slot, seed, side = row['case'], row['slot'], row['seed'], row['enemy_side']
            key = name, slot, side, seed
            require(name in CASES and type(slot) is int and slot in (0, 1) and type(seed) is int and seed == 193 and
                    type(side) is int and side == 1 and key not in seen, 'Unknown/duplicate trainer item case')
            seen.add(key)
            path = Path(row['evidence']).resolve()
            path.relative_to(report_path.parent.resolve())
            raw = read(path)
            raw_ref = retain(path, f'raw/case-{index}.json', row['evidence_sha256'])
            require(raw.get('host_sources') == HOSTS and raw.get('game_sources') == GAME_HASHES and
                    raw.get('inputs') == report['inputs'] and raw.get('oracle_provenance') == oracle and
                    raw.get('shared_snapshot_root') == report['shared_snapshot_root'] and
                    all(raw.get(field) == value for field, value in identity.items()) and
                    (raw.get('case'), raw.get('slot'), raw.get('enemy_side'), raw.get('seed')) == key,
                    'Trainer raw/report execution identity differs')
            require(row.get('passed') is True and row.get('replay_equal') is True and raw.get('terminal') is True and
                    raw.get('failed') is False and raw.get('truncated') is False and raw.get('winner') == row['winner'] and
                    raw.get('scope') == 'ordinary-trainer-fixed-level-source-controller' and
                    raw.get('controller_audit') == manifest['controller_audit'], 'Trainer source terminal/exact replay missing')
            require(raw.get('options') == dict(initial=131, initial2=6, link=0, tower=0, battle_mode=2) and
                    raw.get('initial_normalized_state_sha256') and raw.get('initial_boundaries') and raw.get('decision_boundary'),
                    'Trainer initial source options/state/decision boundary missing')
            initial = raw['initial_contract']
            require(all(initial[field] == raw[field] for field in ('options', 'scenario', 'decision_boundary', 'fixed_party')) and
                    initial['turns'] == [] and initial['terminal'] is False and initial['failed'] is False and initial['truncated'] is False and
                    all(member['level'] == 50 for member in raw['fixed_party']['level_xp']),
                    'Trainer source initial/final fixed-party contract differs')
            segments, comparisons, turns = raw['recorded_segments'], raw['comparisons'], raw['turns']
            require(len(turns) == row['turns'] and turns and turns[-1]['terminal'] is True and turns[-1]['truncated'] is False and
                    row['comparisons'] == len(segments) == len(comparisons) > 0 and
                    all(segment.get('kind') in ('segment', 'one') and re.fullmatch('[0-9a-f]{64}', segment.get('state', ''))
                        for segment in segments) and all(isinstance(comparison.get('event'), str) and
                        type(comparison.get('div_events')) is int and comparison['div_events'] >= 0 for comparison in comparisons),
                    'Trainer full-fight independent comparison/retained replay records missing')
            for segment, comparison in zip(segments, comparisons):
                require(comparison['div_events'] == len(segment['tape']) and
                        all(type(value) is int and 0 <= value <= 255 for value in segment['tape']),
                        'Trainer compared consumed DIV tape differs')
            probe_path = path.with_name(path.stem+'.probe.json')
            probe = read(probe_path)
            require(all(probe[field] == raw[field] for field in ('host_sources', 'game_sources', 'oracle_provenance', 'inputs')),
                    'Trainer source observation probe execution identity differs')
            probe_ref = retain(probe_path, f'raw/source-probe-{index}.json')
            metadata = item_metadata(args.root, name, source, ability, ids, rom_bytes, symbols)
            case_rules = validate_witness(name, slot, raw, row, probe, metadata, ids, rules, rom_bytes)
            wram = Path(row['initial_wram']).resolve()
            wram.relative_to(report_path.parent.resolve())
            require(wram.stat().st_size == 8192 and raw.get('initial_wram') == row['initial_wram'] and
                    raw.get('initial_wram_sha256') == row['initial_wram_sha256'] and
                    raw.get('initial_wram_boundary') == row.get('initial_wram_boundary') == BOUNDARY,
                    'Trainer standalone initial WRAM/path/hash/boundary missing')
            validate_initial_wram(wram.read_bytes(), probe, raw, symbols, ids[1])
            wram_ref = retain(wram, f'raw/initial-wram-{index}.bin', row['initial_wram_sha256'])
            memory = Path(row['initial_memory']).resolve()
            memory.relative_to(report_path.parent.resolve())
            require(raw.get('initial_memory') == row['initial_memory'] and raw.get('initial_memory_sha256') == row['initial_memory_sha256'] and
                    memory.stat().st_size == 0x6000+8192+127+1 and memory.read_bytes()[0x6000:0x8000] == wram.read_bytes() and
                    raw['initial_memory_layout'] == ['WRAM banks 2-7:0000-5fff', 'WRAM banks 0-1:c000-dfff',
                        'HRAM:ff80-fffe', 'SVBK:ff70'], 'Trainer all-bank initial memory differs from plain WRAM')
            retain(memory, f'raw/initial-memory-{index}.bin', row['initial_memory_sha256'])
            record = dict(case_id=f'trainer-item-{name}-{slot}-{seed}', passed=True, unsupported=False,
                complete_fight=True, full_state_comparisons=len(segments),
                full_state_components=['cpu', 'hram', 'wram-banks-0-7', 'svbk'],
                initial_wram=wram_ref, raw_case=raw_ref, source_probe=probe_ref, rule_keys=case_rules,
                choice_contract=dict(roles=configuration['roles'], player_bag=True))
            coverage.runtime_case_options(record, {artifact['path']:artifact for artifact in artifacts}, symbols,
                args.output, configuration, current)
            records.append(record)
            accepted.update(case_rules)
            index += 1
    require(seen == {(name, slot, 1, 193) for name in CASES for slot in (0, 1)} and len(records) == 28 and
            sum(record['full_state_comparisons'] for record in records) == 11200,
            'Trainer exact twenty-eight source fights/independent comparison count required')
    require(accepted == {'trainer-item:'+name for name in CASES} |
            {'field:GUARD_MIST', 'endturn:HandleMist', 'volatile:SUBSTATUS_FOCUS_ENERGY'},
            'Trainer claim set escaped its seventeen positive source keys')
    for (expected, name), snapshot in sorted(pending_inputs.items()):
        retain(snapshot, 'raw/input-'+expected+'-'+name, expected)
    cases_ref = write('cases.json', dict(manifest_sha256=identity['manifest_sha256'], configuration=configuration, cases=records))
    adapter = dict(**identity, inventory_sha256=current['inventory_sha256'], configuration=configuration,
        acceptance_scope='conditional-native-gameplay', oracle_kind='same-presentation-hooks-actual-rom-differential',
        failed_cases=0, unsupported_cases=0, complete_fights=28, full_state_comparisons=11200,
        rule_keys=sorted(accepted), execution_artifacts=execution, oracle_artifacts=oracle_artifacts,
        case_reports=[cases_ref], artifacts=list(artifacts),
        limitations=first['limits']+['Trainer item keys claim actual source effects and inventory consumption under typed action selection.',
            'The AI_Items desirability wrappers map the source inventory but their policy is not claimed executed.',
            'Paralysis cure is supporting status evidence, not a positive status:PAR behavior claim.'],
        metadata_basis='Pinned 23-host execution checkpoint, 28 typed source fights and independent source/ROM item parameter, HP, PP, cache, stage and flag reconstruction.')
    return finish(current, configuration, adapter, 'source-trainer-items', args.output, write,
        dict(collected_cases=28, executed_cases=28, collected_rules=sorted(accepted),
            shared_snapshot_root=str(shared), retained_input_snapshots=len(pending_inputs),
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
        parser.exit(2, 'Trainer item evidence collection rejected: '+str(error)+'\n')


if __name__ == '__main__':
    raise SystemExit(main())
