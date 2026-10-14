#!/usr/bin/env python3
"""Focused linked all-fit transaction checks, not native/cadence acceptance."""
import argparse
import json
from pathlib import Path
from asm_retained_source_tests import symbols, machine, put, get, raw, expected_header_output
from retained_source_cpu import require


def run(image):
    facing = next(i for i in range(62) if len(expected_header_output(image, bytes((48,48,32,0,i,0,0x84,255)))) == 16)
    cases = []
    for count, overlap, peers, cold, expected in ((0, False, 0, False, True), (1, False, 0, False, True), (5, True, 0, False, True), (6, True, 0, False, False), (13, False, 0, False, False), (0, False, 3, False, True), (0, False, 3, True, False), (1, False, 3, False, True), (1, False, 3, True, False)):
        current = machine(image)
        current.memory[:] = bytes(65536)
        for bank in current.banks:
            bank[:] = bytes(4096)
        current.memory[0xff70] = 5
        current.bank = image[1]['MobileVirtualComposeStep'][0]
        current.memory[current.address('hROMBank')] = current.bank
        for name in ('wMobileOnline', 'wMobileWanted', 'wMobileStableSourceValid', 'wMobileStableActorViewValid', 'wMobileVirtualStage'):
            put(current,name,1)
        current.banks[5][0x868:0x890] = bytes([255])*40
        current.banks[5][0x840:0x868] = bytes([160])*40
        header_start = current.address('wMobileVirtualDescriptors')
        intended = bytearray()
        for index in range(count):
            y = 48 if overlap else 24 + index * 10
            header = bytes((48,y,32,0,facing,0,0x84,255))
            current.memory[header_start+index*8:header_start+index*8+8] = header
            intended.extend(expected_header_output(image,header))
        put(current,'wMobileLocalValid',1)
        put(current,'wMobilePeerCount',peers)
        for peer in range(peers):
            start=current.address('wMobilePeer1')+peer*64-0xd000
            for offset,value in {17:1,18:1,24:1,30:1,34:0 if cold else 7,54:1}.items():
                current.banks[2][start+offset]=value
            for offset,value in {13:40+peer*25,15:40,20:40+peer*25,22:40}.items():
                current.banks[2][start+offset]=value
            current.write(current.address('wMobileVirtualWindowOwners')+peer,peer+1)
        clocks = 0
        calls = 0
        stages = {}
        while get(current,'wMobileVirtualStage'):
            stage=get(current,'wMobileVirtualStage')
            elapsed=current.execute('MobileVirtualComposeStep')
            clocks += elapsed
            item=stages.setdefault(stage,{'calls':0,'t_clocks':0})
            item['calls']+=1
            item['t_clocks']+=elapsed
            calls += 1
            require(calls < 100,'Transaction did not terminate')
        published = get(current,'wMobileStablePendingValid') == 1
        require(published == expected,f'Unexpected publication for {count}/{overlap}: {published}')
        if published:
            require(get(current,'wMobileStableCapacityValid') == 1,'Uncertified publication')
            actual = current.banks[5][current.address('wMobileVirtualOutput')-0xd000:current.address('wMobileVirtualOutput')-0xd000+len(intended)]
            require(actual == intended,'Incomplete or reordered figure output')
        require(int.from_bytes(current.banks[5][0x8b0:0x8b2],'little') == (1<<count)-1 | (0xe000 if peers else 0),'Eligibility was hidden by capacity or cold readiness')
        if published:
            require(get(current,'wMobileVirtualLength') == count*16+peers*16,'Complete group count differs')
        cases.append({'native_count':count,'overlap':overlap,'peer_count':peers,'cold':cold,'published':published,'compose_calls':calls,'whole_composition_t_clocks':clocks,'stages':stages})
    return {'cases':cases,'scope':'Actual linked complete composition caller only. No interrupts, outer MobileNativeIdle overhead, wall-clock performance, LCD, stable pressure phases or M2 acceptance.'}


def deadline_checks(image):
    """Static-LY emitted bounds, including the real idle caller return path."""
    cases = []

    def fixture(count, ly, cursor=1, invalid=False):
        current = machine(image)
        current.memory[:] = bytes(65536)
        for bank in current.banks:
            bank[:] = bytes(4096)
        current.memory[0xff70] = 5
        current.bank = image[1]['MobileVirtualComposeStep'][0]
        put(current, 'hROMBank', current.bank)
        for name in ('wMobileOnline', 'wMobileWanted', 'wMobileStableSourceValid'):
            put(current, name, 1)
        put(current, 'wMobileVirtualStage', 2)
        put(current, 'wMobileVirtualCursor', cursor)
        address = current.address('wMobileVirtualDescriptors')
        current.memory[address+6] = 0x84
        if cursor < 13:
            facing = next(i for i in range(62) if len(expected_header_output(image, bytes((48,48,32,0,i,0,0x80+count,255)))) == count*4) if count else 0
            current.memory[address+cursor*8:address+cursor*8+8] = bytes((48,48,32,0,255 if invalid else facing,0,0x80+count if count else 0,255))
        current.memory[0xff44] = ly
        return current

    def transaction(current):
        return (bytes(current.memory[0xc000:0xc700]), bytes(current.memory[0xc900:0xe000]), bytes(current.banks[5]))

    for count, boundary, cursor, invalid in ((4,131,1,False), (12,110,1,False), (16,110,1,False), (0,140,1,False), (4,131,1,True), (0,140,13,False), (0,140,255,False)):
        for ly in sorted({boundary-1, boundary, 138}):
            current = fixture(count, ly, cursor, invalid)
            before = transaction(current)
            guard = current.execute('MobileVirtualCanCompose')
            admitted = bool(current.f & 16)
            require(admitted == (ly < boundary), 'Current-candidate deadline differs')
            require(transaction(current) == before, 'Outer guard mutated transaction')
            step = current.execute('MobileVirtualComposeStep')
            if not admitted:
                require(transaction(current) == before, 'Deferred step mutated transaction/history')
            else:
                require(get(current, 'wMobileVirtualCursor') == (cursor+1 if cursor < 13 else 0), 'Accepted cursor path differs')
                require(get(current, 'wMobileVirtualStage') == (2 if cursor < 13 else 3), 'Accepted stage path differs')

            # Execute the actual MobileNativeIdle entry, dispatch and bank-restoring
            # return. A synthetic LY jump at the next loop/transport check ends
            # this one-candidate measurement without executing a second candidate.
            outer = fixture(count, ly, cursor, invalid)
            for name in ('wMobileLastFrame', 'wMobileVirtualBackgroundFrame', 'hVBlankCounter'):
                put(outer, name, 1)
            outer.memory[0xff55] = 0x80
            outer.memory[0xff70] = 3
            outer.memory[0xff4f] = 1
            initial_registers = dict(outer.r), outer.f
            read = outer.read
            reads = 0
            def bounded_read(address):
                nonlocal reads
                if address == 0xff44:
                    reads += 1
                    return ly if reads <= (3 if admitted else 2) else 144
                return read(address)
            outer.read = bounded_read
            total = outer.execute('MobileNativeIdle')
            require((outer.r, outer.f) == initial_registers, 'Idle caller registers changed')
            require(outer.memory[0xff70] == 3 and outer.memory[0xff4f] == 1, 'Idle caller banks changed')
            expected_cursor = (cursor+1 if cursor < 13 else 0) if admitted else cursor
            require(get(outer, 'wMobileVirtualCursor') == expected_cursor, 'Outer caller disagrees with guard')
            # CGB double-speed: 912 CPU T per line; subtract the worst starting
            # position, 908T, even though the guard samples LY later in the call.
            available = (144-ly)*912-908
            if admitted:
                require(total < available, f'Complete caller exceeds worst-position window: {count}/{ly}: {total}/{available}')
            cases.append({'quads':count, 'invalid':invalid, 'cursor':cursor, 'ly':ly, 'admitted':admitted, 'guard_t':guard, 'step_t':step, 'complete_idle_caller_t':total, 'worst_position_available_t':available, 'return_margin_t':available-total if admitted else None})
    # Exercise all immutable facing shapes, mask-bit positions and row rollback.
    # Reserve 2048T beyond the complete guard/step pair for outer dispatch and
    # restoration, exceeding every measured complete idle-caller overhead above.
    require(max(case['complete_idle_caller_t']-case['guard_t']-case['step_t'] for case in cases if case['admitted']) < 2048, 'Outer return reserve is too small')
    shapes = []
    for facing in range(62):
        header = bytes((48,48,32,0,facing,0,0x84,255))
        count = len(expected_header_output(image, header))//4
        if not count:
            continue
        boundary = 131 if count <= 4 else 110
        for cursor in (0,7,12):
            for rollback in (False, True):
                current = fixture(4, boundary-1, cursor)
                address = current.address('wMobileVirtualDescriptors')+cursor*8
                header = bytes((48,48,32,0,facing,0,0x80+count,255))
                current.memory[address:address+8] = header
                if rollback:
                    # The final emitted quad's last visible row forces exact
                    # rollback after any preceding admissible rows/quads.
                    output = expected_header_output(image, header)
                    row = output[-4]-16+15
                    if 0 <= row < 144:
                        current.write(current.address('wMobileVirtualLines')+row, 10)
                guard = current.execute('MobileVirtualCanCompose')
                require(current.f & 16, 'Facing unexpectedly deferred at accepted boundary')
                step = current.execute('MobileVirtualComposeStep')
                available = (145-boundary)*912-908
                require(guard+step+2048 < available, f'Facing/rollback exceeds reserved caller bound: {facing}/{cursor}/{rollback}')
                shapes.append({'facing':facing,'quads':count,'cursor':cursor,'rollback':rollback,'guard_step_t':guard+step,'reserved_complete_caller_t':guard+step+2048,'available_t':available})
    return {'cases':cases, 'facing_bounds':shapes, 'scope':'Actual emitted CanCompose/ComposeStep and complete MobileNativeIdle caller with a synthetic LY jump only at the next scheduling check. Static LY, no interrupts or native cadence acceptance.'}


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--rom',type=Path,required=True)
    parser.add_argument('--sym',type=Path,required=True)
    parser.add_argument('--output',type=Path)
    args=parser.parse_args()
    image=(args.rom.read_bytes(),symbols(args.sym))
    result=run(image)
    result['native_deadlines']=deadline_checks(image)
    text=json.dumps(result,indent=2)+'\n'
    if args.output:args.output.write_text(text)
    print(text)

if __name__=='__main__':main()
