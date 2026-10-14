// Finite disabled-native evidence. Never enabled by the ordinary physical mode.
#pragma once
#include <algorithm>
#include <array>
#include <cstdint>
namespace native_reference {
constexpr unsigned bytes = 444 + 16 * 28 + 80;
struct Point { unsigned bank = 0, address = 0; };
struct Snapshot {
    uint64_t id = 0, entry = 0, returned = 0, closed = 0;
    std::array<unsigned char, bytes> data{};
};
struct Observer {
    // shadow OAM, used OAM index, loaded IDs/type/glows/old glows,
    // actor glows/old glows/fade, time-of-day/time palette/pal state/hour/fade,
    // palette flags, current endpoint[4], weather, map time/palset/group/number/environment, fixture lighting and needed glow.
    std::array<Point, 24> fields{};
    std::array<unsigned char, 4> sprites{}, palettes{}, map_x{}, map_y{};
    bool enabled = false, emitter_active = false;
    unsigned failure = 0, actor = 0, start = 0, emitter_sp = 0, return_pc = 0, return_bank = 0;
    unsigned completed = 0;
    Snapshot pending{}, latest{}, dma{}, raster{};
    void fail(unsigned value) { if (!failure) failure = value; }
    template<class Read> unsigned value(unsigned field, unsigned offset, Read read) const {
        const auto p = fields[field]; return read(p.bank, p.address + offset);
    }
    template<class Source, class Read> void before(const Source& source, bool entry, bool emit, bool close,
            unsigned bc, unsigned sp, unsigned bank, Read read) {
        if (!enabled || failure) return;
        if (entry) {
            if (emitter_active) { fail(1); return; }
            pending = Snapshot{}; completed = 0;
        }
        if (emit) {
            if (emitter_active || bc < source.objects || (bc - source.objects) % 34 ||
                    (bc - source.objects) / 34 >= 4) { fail(2); return; }
            actor = (bc - source.objects) / 34;
            if (completed & (1u << actor)) { fail(3); return; }
            start = value(1, 0, read);
            if (start % 4 || start > 144) { fail(4); return; }
            emitter_active = true; emitter_sp = sp;
            return_pc = read(8, sp) | read(8, (sp + 1) & 65535) << 8;
            return_bank = return_pc < 0x4000 ? 0 : bank;
        }
        if (!close) return;
        if (source.failure || !source.latest || emitter_active || completed != 15) { fail(5); return; }
        const auto& s = source.sources[(source.latest - 1) % source.sources.size()];
        if (s.emitted != 15) { fail(6); return; }
        // Independent finite key from the original GameCorner INDOOR/PALETTE_DAY
        // metadata, hour12 RTC and nonfading CopyObjectSpritePalHandler.
        // LookupOBPalette reads only USE_DAYTIME_PAL_F (bit2) of wPalFlags.
        // Other flag bits control scan/apply scheduling, not palette selection.
        if (value(9, 0, read) != 1 || value(10, 0, read) != 1 ||
                value(11, 0, read) != 1 || value(12, 0, read) != 12 ||
                (value(14, 0, read) & 4) || value(15, 0, read) ||
                value(15, 1, read) || value(15, 2, read) || value(15, 3, read) != 1 ||
                value(16, 0, read) || value(17, 0, read) != 1 || value(18, 0, read) != 0x55 ||
                value(19, 0, read) != 11 || value(20, 0, read) != 23 || value(21, 0, read) != 3 ||
                value(22, 0, read) != 255 || value(23, 0, read)) {
            fail(14); return;
        }
        pending.id = s.id; pending.entry = s.entry; pending.returned = s.returned; pending.closed = s.closed;
        std::copy(s.at_close.begin(), s.at_close.end(), pending.data.begin());
        for (unsigned a = 0; a < 13; ++a) {
            const unsigned at = a * 34;
            if (a >= 4) { if (s.at_close[at]) { fail(7); return; } continue; }
            // Fixed standing input and canonical non-glowing native palette.
            const unsigned slot = s.at_close[at + 6] & 7;
            if (s.original[at] != sprites[a] || s.at_return[at] != sprites[a] || s.at_close[at] != sprites[a] ||
                    s.at_close[at + 16] != map_x[a] || s.at_close[at + 17] != map_y[a] ||
                    s.at_close[at + 33] != palettes[a] || value(2, slot, read) != palettes[a] ||
                    (value(3, 0, read) & (1u << slot)) || value(4, slot, read) || value(5, slot, read) ||
                    value(6, a, read) || value(7, a, read) || value(8, 0, read) || value(13, 0, read) ||
                    s.at_close[at + 4] & 1 || s.at_close[at + 5] & 0xc8 ||
                    s.at_close[at + 13] != 0 || s.at_close[at + 25] || s.at_close[at + 26]) {
                fail(8); return;
            }
            for (unsigned f : {0u, 5u, 6u, 13u, 16u, 17u, 23u, 24u, 25u, 26u, 33u})
                if (s.original[at + f] != s.at_close[at + f] || s.at_return[at + f] != s.at_close[at + f]) {
                    fail(9); return;
                }
        }
        // Resource bytes are read from original hardware at genuine source close.
        for (unsigned q = 0; q < 16; ++q) {
            const unsigned at = 444 + q * 28;
            const unsigned tile = pending.data[at + 2], flags = pending.data[at + 3];
            for (unsigned n = 0; n < 16; ++n) pending.data[at + 4 + n] = read(9, ((flags >> 3) & 1) * 8192 + tile * 16 + n);
            for (unsigned n = 0; n < 8; ++n) pending.data[at + 20 + n] = read(10, (8 + (flags & 7)) * 8 + n);
        }
        unsigned at = 444 + 16 * 28;
        for (unsigned f = 2; f < fields.size(); ++f) {
            const unsigned count = f == 2 || f == 4 || f == 5 ? 8 : f == 6 || f == 7 ? 13 : f == 15 ? 4 : 1;
            for (unsigned n = 0; n < count; ++n) pending.data[at++] = value(f, n, read);
        }
        latest = pending;
    }
    template<class Read> void after(unsigned bank, unsigned pc, unsigned sp, unsigned pre_sp, bool returned, Read read) {
        if (!enabled || failure || !emitter_active || !returned || pre_sp != emitter_sp ||
                sp != ((emitter_sp + 2) & 65535) || pc != return_pc || bank != return_bank) return;
        if (value(1, 0, read) != start + 16) { fail(10); return; }
        for (unsigned n = 0; n < 4; ++n) {
            const unsigned q = 39 - start / 4 - n;
            for (unsigned f = 0; f < 4; ++f)
                pending.data[444 + (actor * 4 + n) * 28 + f] = value(0, q * 4 + f, read);
        }
        completed |= 1u << actor; emitter_active = false;
    }
    void on_dma(unsigned page, unsigned bank) {
        if (!enabled || failure) return;
        // Before the first observed producer closes there is no reference token.
        if (!latest.id) { dma = Snapshot{}; return; }
        if (page != fields[0].address / 256 || bank != fields[0].bank) { fail(11); return; }
        dma = latest;
    }
    void row(unsigned line) {
        if (!enabled || failure) return;
        if (!line) raster = dma;
        if (!raster.id || dma.id != raster.id) fail(12);
    }
};
}
