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
        prepare_frontend_clock(source)
        prepare_frontend_raster(source)
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
    prepare_frontend_clock(source)
    prepare_frontend_raster(source)


def prepare_frontend_clock(source):
    """Expose normal cartridge clock persistence, never CPU/game-state writes.

    The pinned single-instance RTC getter indexes renderer 1 instead of 0 and
    exposes fixed_time, not the clock offset used by the system-clock mode.
    A separate frontend hook persists only the offset produced by ordinary ROM
    RTC writes. Explicit fixture restore uses it before the first emulated frame.
    """
    for name, anchor, replacement in (
        ('libretro/dmy_renderer.h', '\tdword fixed_time;',
         '\tdword fixed_time;\n'
         '\tint32_t mobile_frontend_clock(bool system) const { return system ? rtc_offset_seconds : cur_time; }\n'
         '\tvoid mobile_frontend_clock_restore(bool system, int32_t value) {\n'
         '\t\tif (system) rtc_offset_seconds = value; else cur_time = value;\n'
         '\t}'),
        ('libretro/libretro.cpp', 'static void check_variables(void);',
         'static void check_variables(void);\n'
         'extern "C" bool mobile_frontend_rtc(uint32_t* state, bool restore) {\n'
         '    if (!state || !render[0]) return false;\n'
         '    if (restore) {\n'
         '        if (state[0] != 0x31544352 || state[1] != unsigned(use_system_clock)) return false;\n'
         '        render[0]->mobile_frontend_clock_restore(use_system_clock, int32_t(state[2]));\n'
         '    } else {\n'
         '        state[0] = 0x31544352; state[1] = unsigned(use_system_clock);\n'
         '        state[2] = uint32_t(render[0]->mobile_frontend_clock(use_system_clock));\n'
         '    }\n'
         '    return true;\n'
         '}')):
        path = source / name
        data = path.read_text(encoding='utf-8-sig')
        if 'mobile_frontend_clock' in data or 'mobile_frontend_rtc' in data:
            continue
        if data.count(anchor) != 1:
            raise ValueError('Pinned frontend RTC anchor changed: ' + name)
        path.write_text(data.replace(anchor, replacement))


def prepare_frontend_raster(source):
    """Align test frontend calls through ordinary scanline execution only."""
    path = source / "libretro/libretro.cpp"
    text = path.read_text(encoding="utf-8-sig")
    if "mobile_frontend_align_raster" in text:
        return
    anchor = "void retro_run(void)"
    if text.count(anchor) != 1:
        raise ValueError("Pinned scanline frontend anchor changed")
    hook = r'''extern "C" unsigned mobile_frontend_raster_state() {
    if (emulated_gbs != 1 || !v_gb[0]) return 0xffff;
    const auto* regs = v_gb[0]->get_regs();
    return regs->LY | ((regs->LCDC & 0x80) ? 0x100 : 0);
}
extern "C" unsigned mobile_frontend_align_raster() {
    if (emulated_gbs != 1 || !v_gb[0] || infrared_lockstep) return 0;
#ifndef DISABLE_SOCKETS
    if (mobile_adapter_enabled && mobile_adapter) mobile_adapter->update();
#endif
    for (unsigned lines = 1; lines <= 155; ++lines) {
        const auto before = mobile_frontend_raster_state();
        if (!(before & 0x100)) return 0;
        if (extra_inputpolling_enabled) performExtraInputPoll();
        v_gb[0]->run();
        if (master_link) master_link->process();
        const auto after = mobile_frontend_raster_state();
        if (!(after & 0x100)) return 0;
        if ((before & 255) == 153 && (after & 255) == 0) return lines;
    }
    return 0;
}

'''
    path.write_text(text.replace(anchor, hook + anchor))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('source', type=Path)
    prepare(parser.parse_args().source)
