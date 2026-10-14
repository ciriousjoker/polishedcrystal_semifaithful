// Independent accepted wire input, with no renderer, transport-return or cache authority.
#pragma once
#include <array>
#include <cstddef>
#include <cstdint>
#include <limits>

namespace accepted_ordinary_input {
constexpr unsigned max_peers = 3, definition_capacity = 8;
using Bytes20 = std::array<uint8_t, 20>;
using CommittedSlots = std::array<Bytes20, max_peers>;
enum class Failure : unsigned {
    none, ordinal, overflow, malformed, receiver, pending_replaced, transaction,
    lifetime, empty_delta, unknown_delta, identity_transition, static_transition,
    committed_input, definition_pressure, missing_source, retired, missing_definition
};
struct Peer {
    uint16_t id = 0, generation = 0, revision = 0, instance = 0, epoch = 0, x = 0, y = 0;
    uint8_t map_group = 0, map_number = 0, facing = 0, movement = 0, status = 0, avatar = 0;
    Bytes20 expanded{};
};
struct Definition {
    uint64_t id = 0, lifetime = 0, transaction = 0, captured = 0, committed = 0;
    uint16_t receiver_id = 0, receiver_generation = 0;
    unsigned count = 0;
    std::array<Peer, max_peers> peers{};
};
struct Latch {
    uint64_t opportunity = 0, instruction = 0, source = 0, source_closed = 0, input = 0;
};
struct Candidate {
    uint64_t transaction = 0, lifetime = 0, captured = 0;
    unsigned length = 0, count = 0;
    std::array<uint8_t, 64> frame{};
    std::array<Peer, max_peers> peers{};
};
struct Observer {
    Failure failure = Failure::none;
    uint64_t failure_instruction = 0, instruction = 0, lifetime = 0, next_transaction = 0;
    uint64_t next_definition = 0, released = 0, latest = 0, next_opportunity = 0;
    uint16_t receiver_id = 0, receiver_generation = 0, lobby = 0;
    std::array<uint8_t, 8> token{};
    uint8_t capacity = 0;
    bool online = false, pending = false;
    Candidate candidate{};
    Definition current{};
    Latch latched{};
    std::array<Definition, definition_capacity> definitions{};

    void fail(Failure why) {
        if (failure == Failure::none) { failure = why; failure_instruction = instruction; }
    }
    bool event(uint64_t ordinal) {
        if (failure != Failure::none) return false;
        if (instruction == std::numeric_limits<uint64_t>::max()) { fail(Failure::overflow); return false; }
        if (!ordinal || ordinal <= instruction) { fail(Failure::ordinal); return false; }
        instruction = ordinal;
        return true;
    }
    bool increment(uint64_t& value) {
        if (value == std::numeric_limits<uint64_t>::max()) { fail(Failure::overflow); return false; }
        ++value; return true;
    }
    static uint16_t be(const uint8_t* p) { return uint16_t(unsigned(p[0]) * 256 + p[1]); }
    static void put(Bytes20& b, unsigned at, uint16_t value) {
        b[at] = uint8_t(value >> 8); b[at + 1] = uint8_t(value);
    }
    static bool header(const uint8_t* p, unsigned size, unsigned kind, unsigned payload) {
        return p && size == payload + 8 && p[0] == 0x50 && p[1] == 0x43 && p[2] == 1 &&
            p[3] == kind && be(p + 6) == payload;
    }
    // Call only at the actual accepted WELCOME path, never a general carry-clear return.
    bool welcome(uint64_t ordinal, const uint8_t* frame, unsigned length) {
        if (!event(ordinal)) return false;
        if (!header(frame, length, 17, 15) || !be(frame + 8) || !be(frame + 10) ||
            be(frame + 12) != 1 || frame[22] != 4) { fail(Failure::malformed); return false; }
        const bool reconnect = lifetime != 0;
        pending = false; online = false; current = Definition{}; latest = 0;
        if (!increment(lifetime)) return false;
        receiver_id = be(frame + 8); receiver_generation = be(frame + 10);
        lobby = be(frame + 12); capacity = frame[22];
        for (unsigned i = 0; i < token.size(); ++i) token[i] = frame[14 + i];
        online = true;
        if (reconnect) { fail(Failure::lifetime); return false; }
        return true;
    }
    bool parse_peer(const uint8_t* p, Peer& out) {
        out.id = be(p); out.generation = be(p + 2); out.revision = be(p + 4);
        out.map_group = p[6]; out.map_number = p[7]; out.instance = be(p + 8);
        out.epoch = be(p + 10); out.x = uint16_t((unsigned(p[12]) << 4) | (p[13] >> 4));
        out.y = uint16_t((unsigned(p[13] & 15) << 8) | p[14]);
        out.facing = p[15] >> 4; out.movement = (p[15] >> 1) & 7;
        out.status = p[15] & 1; out.avatar = p[16];
        if (!out.id || !out.generation || out.id == receiver_id || out.movement > 5 || out.avatar > 3)
            return false;
        for (unsigned i = 0; i < 12; ++i) out.expanded[i] = p[i];
        put(out.expanded, 12, out.x); put(out.expanded, 14, out.y);
        out.expanded[16] = out.facing; out.expanded[17] = out.status;
        out.expanded[18] = out.avatar; out.expanded[19] = out.movement;
        return true;
    }
    // Capture exactly at MobileExpandRoster entry, before its in-place expansion.
    // The returned observer transaction, not sequence or buffer address, binds commit.
    uint64_t capture(uint64_t ordinal, const uint8_t* frame, unsigned length, unsigned decode_phase = 0) {
        if (!event(ordinal)) return 0;
        if (decode_phase != 0) { fail(Failure::transaction); return 0; }
        if (pending) { fail(Failure::pending_replaced); return 0; }
        if (!online) { fail(Failure::receiver); return 0; }
        if (!frame || length < 13 || length > 64 || (frame[3] != 19 && frame[3] != 24) ||
            frame[12] > 3 || !header(frame, length, frame[3], 5 + 17 * frame[12])) {
            fail(Failure::malformed); return 0;
        }
        if (be(frame + 8) != receiver_id || be(frame + 10) != receiver_generation) {
            fail(Failure::receiver); return 0;
        }
        Candidate next{}; next.count = frame[12]; next.length = length;
        for (unsigned i = 0; i < length; ++i) next.frame[i] = frame[i];
        for (unsigned i = 0; i < next.count; ++i) {
            if (!parse_peer(frame + 13 + 17 * i, next.peers[i])) { fail(Failure::malformed); return 0; }
            for (unsigned j = 0; j < i; ++j) if (next.peers[j].id == next.peers[i].id) {
                fail(Failure::malformed); return 0;
            }
        }
        if (!increment(next_transaction)) return 0;
        next.transaction = next_transaction; next.captured = instruction; next.lifetime = lifetime;
        candidate = next; pending = true;
        return candidate.transaction;
    }
    static bool static_equal(const Peer& a, const Peer& b) {
        // Revision may advance without changing the fixed-scene pose.
        for (unsigned i = 6; i < 20; ++i) if (a.expanded[i] != b.expanded[i]) return false;
        return true;
    }
    void retire_membership(Failure why) {
        pending = false; current = Definition{}; latest = 0; fail(why);
    }
    bool compare(const Definition& expected, const CommittedSlots& slots) {
        if (expected.count > max_peers) { fail(Failure::malformed); return false; }
        // Only accepted membership defines the valid prefix. Stale tail slots have
        // no authority, even if they duplicate a retained ID after roster removal.
        for (unsigned i = 0; i < expected.count; ++i) {
            unsigned found = 0;
            for (unsigned j = 0; j < expected.count; ++j) {
                if (be(slots[j].data()) != expected.peers[i].id) continue;
                ++found;
                if (slots[j] != expected.peers[i].expanded) { fail(Failure::committed_input); return false; }
            }
            if (found != 1) { fail(Failure::committed_input); return false; }
        }
        return true;
    }
    // Call only at matching genuine MobileConsumeFrame.roster_done entry. Header
    // is the still-live first 13 bytes, which expansion must leave unchanged.
    bool commit(uint64_t ordinal, uint64_t transaction, const uint8_t* live_header,
                unsigned header_length, const CommittedSlots& bank2_slots) {
        if (!event(ordinal)) return false;
        if (!pending || !transaction || transaction != candidate.transaction) {
            fail(Failure::transaction); return false;
        }
        if (!online || lifetime != candidate.lifetime) { fail(Failure::lifetime); return false; }
        if (!live_header || header_length != 13) { fail(Failure::transaction); return false; }
        for (unsigned i = 0; i < 13; ++i) if (live_header[i] != candidate.frame[i]) {
            fail(Failure::pending_replaced); return false;
        }
        Definition next = current;
        const bool delta = candidate.frame[3] == 24;
        if (delta && !candidate.count) { fail(Failure::empty_delta); return false; }
        if (!delta) { next.peers = candidate.peers; next.count = candidate.count; }
        for (unsigned i = 0; i < candidate.count; ++i) {
            const Peer& p = candidate.peers[i];
            unsigned old = 0;
            while (old < current.count && current.peers[old].id != p.id) ++old;
            if (old == current.count && delta) { retire_membership(Failure::unknown_delta); return false; }
            if (old != current.count) {
                if (current.peers[old].generation != p.generation) {
                    retire_membership(Failure::identity_transition); return false;
                }
                // Unavailable uploads are still exact accepted membership/input.
                // They cannot establish a visible static pose. Once status 1
                // establishes it, retain every existing static transition guard.
                if (current.peers[old].status == 1 && !static_equal(current.peers[old], p)) {
                    fail(Failure::static_transition); return false;
                }
                if (delta) next.peers[old] = p;
            }
        }
        if (!compare(next, bank2_slots)) return false;
        if (next_definition - released >= definition_capacity) { fail(Failure::definition_pressure); return false; }
        if (!increment(next_definition)) return false;
        next.id = next_definition; next.lifetime = lifetime; next.transaction = transaction;
        next.captured = candidate.captured; next.committed = instruction;
        next.receiver_id = receiver_id; next.receiver_generation = receiver_generation;
        definitions[(next.id - 1) % definition_capacity] = next;
        current = next; latest = next.id; pending = false;
        return true;
    }
    // Adapter for a bank-aware emulator read callback. Only the first twenty
    // original committed bytes of each 64-byte private record are observed.
    template<class Read> bool commit_bank2(uint64_t ordinal, uint64_t transaction,
            const uint8_t* live_header, unsigned header_length, unsigned base, Read read) {
        if (base < 0xd000 || base > 0xe000 - 2 * 64 - 20) {
            if (event(ordinal)) fail(Failure::malformed);
            return false;
        }
        CommittedSlots slots{};
        for (unsigned slot = 0; slot < max_peers; ++slot)
            for (unsigned byte = 0; byte < 20; ++byte)
                slots[slot][byte] = uint8_t(read(2, base + 64 * slot + byte));
        return commit(ordinal, transaction, live_header, header_length, slots);
    }
    const Definition* get(uint64_t id) {
        if (!id || id <= released || id > next_definition || definitions[(id - 1) % definition_capacity].id != id) {
            fail(Failure::missing_definition); return nullptr;
        }
        return &definitions[(id - 1) % definition_capacity];
    }
    bool release_through(uint64_t id) {
        if (failure != Failure::none) return false;
        if (id < released || id > next_definition || (latest && id >= latest) ||
            (latched.input && id >= latched.input)) { fail(Failure::missing_definition); return false; }
        released = id; return true;
    }
    // This outer opportunity is independent of inner projection/cache readiness.
    // source and source_closed are the genuine completed native source tokens.
    bool opportunity(uint64_t ordinal, uint64_t source, uint64_t source_closed) {
        if (!event(ordinal)) return false;
        if (!source || !source_closed || source_closed >= instruction ||
            source < latched.source || source_closed < latched.source_closed ||
            (source == latched.source && source_closed != latched.source_closed) ||
            (source > latched.source && latched.source && source_closed <= latched.source_closed)) { fail(Failure::missing_source); return false; }
        if (!increment(next_opportunity)) return false;
        latched = {next_opportunity, instruction, source, source_closed, latest};
        return true;
    }
    // Cancellation is explicit evidence, not a carry-clear or incomplete return.
    bool cancel(uint64_t ordinal, uint64_t transaction) {
        if (!event(ordinal)) return false;
        if (!pending || transaction != candidate.transaction) { fail(Failure::transaction); return false; }
        pending = false; return true;
    }
    // Reset/restore/offline/release all retire the current connection. A new
    // observer is required for a new static experiment; counters are never reused.
    void retire(uint64_t ordinal) {
        if (!event(ordinal)) return;
        online = false; pending = false; current = Definition{}; latest = 0;
        if (increment(lifetime)) fail(Failure::retired);
    }
};
static_assert(sizeof(Observer) < 16384, "accepted observer must remain bounded");
} // namespace accepted_ordinary_input
