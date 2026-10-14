/* Experimental source-aware throughput slice. Never a faithful training environment.
 * Profile: synthetic teams, default PSS, no items/abilities/status/affection/weather,
 * neutral stages, normal 100%-accuracy non-high-critical zero-priority moves only.
 * Native independent RNG does not reproduce ROM random streams or draw order.
 */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <assert.h>

typedef struct { const char *name; uint8_t power, type, pp, special, supported; } Move;
#include "simulator_data.h"
#define TEAM 6
#define SLOTS 4
#define BATCH 256
/* Compact, pointer-free state. Batch dimension can be transposed to SoA on GPU. */
typedef struct {
    uint16_t hp[2][TEAM], atk[2][TEAM], def[2][TEAM], sat[2][TEAM], sdf[2][TEAM], speed[2][TEAM];
    uint8_t types[2][TEAM][2], level[2][TEAM], move[2][TEAM][SLOTS], pp[2][TEAM][SLOTS], active[2];
    uint32_t rng, turns, attacks;
} Battle;
static uint32_t random32(Battle *b) {
    uint32_t x = b->rng; x ^= x << 13; x ^= x >> 17; x ^= x << 5; return b->rng = x;
}
static uint32_t range(Battle *b, uint32_t n) {
    uint32_t x, threshold = (0u - n) % n;
    do { x = random32(b); } while (x < threshold);
    return x % n;
}
/* ASM TruncateHL_BC, DamagePass1..4: truncate jointly, floor at each division.
 * Crit modifies numerator BEFORE /defense,/50,+2, unlike many modern calculators.
 * Accepted profile: <=999 stats, level 1..100, supported power <=90. Base
 * DamagePass4 overflow is unreachable, but STAB stage can overflow 16 bits.
 */
static unsigned damage(unsigned level, unsigned power, unsigned attack, unsigned defense,
                       unsigned crit, unsigned modifier, unsigned spread) {
    while (attack > 255 || defense > 255) {
        attack >>= 1; defense >>= 1;
        if (!attack) attack = 1;
        if (!defense) defense = 1;
    }
    if (!defense) defense = 1;
    unsigned v = (2 * level / 5 + 2) * power * attack;
    if (crit) v = v * 3 / 2;
    v = v / defense / 50 + 2;
    if (!modifier) return 0;
    v = v * modifier / 16;
    /* BattleCommand_stab saturates to $ffff BEFORE damagevariation. */
    if (v > 65535) v = 65535;
    if (!v) v = 1;
    return v < 2 ? v : v * spread / 100;
}
static int validate(const Battle *b) {
    for (int s = 0; s < 2; ++s) {
        if (b->active[s] >= TEAM) return 0;
        for (int m = 0; m < TEAM; ++m) {
            if (!b->level[s][m] || b->level[s][m] > 100) return 0;
            if (b->types[s][m][0] >= TYPE_COUNT || b->types[s][m][1] >= TYPE_COUNT) return 0;
            if (!b->atk[s][m] || !b->def[s][m] || !b->sat[s][m] || !b->sdf[s][m]) return 0;
            if (b->atk[s][m] > 999 || b->def[s][m] > 999 || b->sat[s][m] > 999 || b->sdf[s][m] > 999) return 0;
            for (int k = 0; k < SLOTS; ++k) {
                unsigned id = b->move[s][m][k];
                if (!id || id >= MOVE_COUNT || !moves[id].supported || b->pp[s][m][k] > moves[id].pp) return 0;
            }
        }
    }
    return b->rng != 0;
}
static int choose(Battle *b, int s) {
    int a = b->active[s], count = 0, legal[SLOTS];
    for (int k = 0; k < SLOTS; ++k) if (b->pp[s][a][k]) legal[count++] = k;
    return count ? legal[range(b, (unsigned)count)] : -1;
}
static void hit(Battle *b, int s, int slot) {
    int a = b->active[s], d = b->active[1-s];
    const Move *m = &moves[b->move[s][a][slot]];
    --b->pp[s][a][slot]; ++b->attacks;
    /* The ROM consumes its accuracy RNG even for ordinary 100% moves; this spike
       does not match the stream. It preserves the marginal probability only. */
    unsigned crit = range(b, 24) == 0;
    unsigned t0 = b->types[1-s][d][0], t1 = b->types[1-s][d][1];
    unsigned mod = type_chart[m->type][t0];
    if (t1 != t0) mod = mod * type_chart[m->type][t1] / 16;
    if (m->type == b->types[s][a][0] || m->type == b->types[s][a][1]) mod += mod / 2;
    unsigned amount = damage(b->level[s][a], m->power,
        m->special ? b->sat[s][a] : b->atk[s][a],
        m->special ? b->sdf[1-s][d] : b->def[1-s][d], crit, mod, 85 + range(b,16));
    b->hp[1-s][d] = amount >= b->hp[1-s][d] ? 0 : b->hp[1-s][d] - amount;
}
/* Returns winner 0/1, -1 truncated, -2 unsupported. Forced replacements occur
 * between turns, never allow the replacement to inherit a fainted action.
 */
static int rollout(Battle *b) {
    if (!validate(b)) return -2;
    while (b->turns < 512) {
        for (int s = 0; s < 2; ++s) {
            while (b->active[s] < TEAM && !b->hp[s][b->active[s]]) ++b->active[s];
            if (b->active[s] == TEAM) return 1-s;
        }
        int choice[2] = {choose(b,0), choose(b,1)};
        if (choice[0] < 0 || choice[1] < 0) return -2; /* Struggle unsupported. */
        unsigned x = b->speed[0][b->active[0]], y = b->speed[1][b->active[1]];
        int first = x == y ? (int)range(b,2) : x < y;
        ++b->turns;
        hit(b,first,choice[first]);
        if (b->hp[1-first][b->active[1-first]]) hit(b,1-first,choice[1-first]);
    }
    /* A final-turn KO has precedence over the rollout truncation limit. */
    for (int s = 0; s < 2; ++s) {
        int alive = 0;
        for (int m = 0; m < TEAM; ++m) alive |= b->hp[s][m] != 0;
        if (!alive) return 1-s;
    }
    return -1;
}
static Battle fixture(uint32_t seed) {
    Battle b = {0}; b.rng = seed ? seed : 1;
    unsigned supported[MOVE_COUNT], n = 0;
    for (unsigned i = 1; i < MOVE_COUNT; ++i) if (moves[i].supported) supported[n++] = i;
    for (int s = 0; s < 2; ++s) for (int m = 0; m < TEAM; ++m) {
        b.hp[s][m] = 150 + range(&b,100); b.level[s][m] = 50;
        b.atk[s][m] = 80 + range(&b,240); b.def[s][m] = 80 + range(&b,240);
        b.sat[s][m] = 80 + range(&b,240); b.sdf[s][m] = 80 + range(&b,240);
        b.speed[s][m] = 80 + range(&b,100);
        b.types[s][m][0] = range(&b,18); b.types[s][m][1] = b.types[s][m][0];
        for (int k = 0; k < SLOTS; ++k) {
            unsigned id = supported[range(&b,n)]; b.move[s][m][k] = id; b.pp[s][m][k] = moves[id].pp;
        }
    }
    return b;
}
static double seconds(void) { struct timespec ts; clock_gettime(CLOCK_MONOTONIC,&ts); return ts.tv_sec + ts.tv_nsec * 1e-9; }
int main(int argc, char **argv) {
    /* Hand-worked stage-order boundary: crit numerator 264000, /100 /50 +2 =54. */
    assert(damage(50,80,100,100,0,16,100) == 37);
    assert(damage(50,80,100,100,1,16,100) == 54);
    assert(damage(50,80,100,100,0,24,85) == 46);
    assert(damage(50,80,100,100,0,0,100) == 0);
    assert(damage(50,80,300,301,0,16,100) == 37);
    /* 42*80*255 /1 /50 +2 =17138; x6=102828 ->65535,
     * then spread85: floor(65535*85/100)=55704. */
    assert(damage(100,80,255,1,0,96,100) == 65535);
    assert(damage(100,80,255,1,0,96,85) == 55704);
    Battle terminal = fixture(1); terminal.turns = 511;
    for (int s = 0; s < 2; ++s) for (int m = 0; m < TEAM; ++m) {
        terminal.hp[s][m] = 0;
        terminal.types[s][m][0] = terminal.types[s][m][1] = 0;
        for (int k = 0; k < SLOTS; ++k) {
            terminal.move[s][m][k] = 10; /* Extracted SCRATCH ID. */
            terminal.pp[s][m][k] = moves[10].pp;
        }
    }
    terminal.hp[0][0] = 65535; terminal.hp[1][0] = 1;
    terminal.speed[0][0] = 1000; terminal.speed[1][0] = 1;
    assert(rollout(&terminal) == 0 && terminal.turns == 512);
    Battle bad = fixture(1); bad.move[0][0][0] = 1; assert(rollout(&bad) == -2);
    if (argc > 1 && !strcmp(argv[1],"--self-test")) { puts("stage-order and unsupported-move checks passed; no ROM parity claim"); return 0; }
    unsigned count = argc > 1 ? (unsigned)strtoul(argv[1],0,10) : 10000;
    if (!count) return 2;
    Battle batch[BATCH]; uint64_t turns=0, attacks=0, wins[2]={0}, rejected=0, truncated=0;
    double start = seconds();
    for (unsigned base = 0; base < count; base += BATCH) {
        unsigned n = count-base < BATCH ? count-base : BATCH;
        for (unsigned i = 0; i < n; ++i) batch[i] = fixture(base+i+1);
        for (unsigned i = 0; i < n; ++i) {
            int result = rollout(&batch[i]);
            if (result >= 0) ++wins[result]; else if (result == -2) ++rejected; else ++truncated;
            turns += batch[i].turns; attacks += batch[i].attacks;
        }
    }
    double elapsed = seconds()-start;
    printf("experimental native slice, one CPU thread, synthetic random-policy 6v6\n");
    printf("fights=%u seconds=%.6f fights/s=%.0f turns/s=%.0f attacks/s=%.0f state_bytes=%zu batch_bytes=%zu\n",count,elapsed,count/elapsed,turns/elapsed,attacks/elapsed,sizeof(Battle),sizeof(batch));
    printf("wins=%llu,%llu unsupported=%llu truncated=%llu turns=%llu attacks=%llu\n", (unsigned long long)wins[0],(unsigned long long)wins[1],(unsigned long long)rejected,(unsigned long long)truncated,(unsigned long long)turns,(unsigned long long)attacks);
    return 0;
}
