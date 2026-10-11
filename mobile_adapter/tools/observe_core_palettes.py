#!/usr/bin/env python3
"""Expose pinned DoubleCherryGB hardware colors through a read-only memory map.

This changes frontend observations only, not CPU instructions, emulation cycles,
ROM data, peripheral state or palette writes. The address is a separate debugger
space, not a Game Boy bus address. Reapply safely to an already prepared source.
"""
import argparse
from pathlib import Path


def prepare(source):
    path = source / 'libretro/inline/inline_functions.h'
    data = path.read_bytes()
    if b'"mobile-colors"' in data:
        if b'get_lcd()->get_pal(8),' not in data:
            raise ValueError('Hardware observer must begin at object palette 8')
        return
    text = data.decode('utf-8-sig')
    newline = '\r\n' if '\r\n' in text else '\n'
    text = text.replace('\r\n', '\n')
    original = 'struct retro_memory_descriptor descs[10] ='
    if text.count(original) != 1 or text.count('    mmaps.descriptors = descs;') != 1:
        raise ValueError('Pinned core memory-map source changed; review before observing')
    text = text.replace(original, 'struct retro_memory_descriptor descs[11] =')
    text = text.replace('    mmaps.descriptors = descs;', '''    // Read-only hardware color observer, outside the CPU address space.
    descs[i++] = { RETRO_MEMDESC_CONST, v_gb[0]->get_lcd()->get_pal(8),
                  0, 0x10000, 0, 0, 64, "mobile-colors" };
    mmaps.descriptors = descs;''')
    path.write_bytes(text.replace('\n', newline).encode('utf-8'))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('source', type=Path)
    prepare(parser.parse_args().source)
