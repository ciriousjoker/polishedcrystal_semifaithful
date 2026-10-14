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
static unsigned damage_base(unsigned level, unsigned power, unsigned attack, unsigned defense,
                            unsigned crit, unsigned modifier) {
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
    return v;
}
static unsigned damage_spread(unsigned base, unsigned spread) {
    return base < 2 ? base : base * spread / 100;
}
static unsigned damage(unsigned level, unsigned power, unsigned attack, unsigned defense,
                       unsigned crit, unsigned modifier, unsigned spread) {
    return damage_spread(damage_base(level,power,attack,defense,crit,modifier),spread);
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
static unsigned matchup_modifier(const Battle *b, int s, int a, int d, const Move *m) {
    unsigned t0 = b->types[1-s][d][0], t1 = b->types[1-s][d][1];
    unsigned mod = type_chart[m->type][t0];
    if (t1 != t0) mod = mod * type_chart[m->type][t1] / 16;
    if (m->type == b->types[s][a][0] || m->type == b->types[s][a][1]) mod += mod / 2;
    return mod;
}
static void hit(Battle *b, int s, int slot) {
    int a = b->active[s], d = b->active[1-s];
    const Move *m = &moves[b->move[s][a][slot]];
    --b->pp[s][a][slot]; ++b->attacks;
    /* The ROM consumes its accuracy RNG even for ordinary 100% moves; this spike
       does not match the stream. It preserves the marginal probability only. */
    unsigned crit = range(b, 24) == 0;
    unsigned mod = matchup_modifier(b,s,a,d,m);
    unsigned amount = damage(b->level[s][a], m->power,
        m->special ? b->sat[s][a] : b->atk[s][a],
        m->special ? b->sdf[1-s][d] : b->def[1-s][d], crit, mod, 85 + range(b,16));
    b->hp[1-s][d] = amount >= b->hp[1-s][d] ? 0 : b->hp[1-s][d] - amount;
}
/* Candidate features only. Numerators use denominator 24*16=384:
 * 23 equiprobable noncritical copies plus one critical copy of each spread.
 * Two source damage-base evaluations and 32 cheap spread evaluations per slot.
 * No sampling, no RNG mutation, no hidden opposing action or future RNG input.
 */
typedef struct {
    uint32_t raw_numerator, useful_numerator;
    uint16_t ko_numerator, minimum, maximum;
} DamageFeatures;
static DamageFeatures aggregate_features(unsigned normal, unsigned critical, unsigned hp) {
    DamageFeatures f = {0}; f.minimum = 65535;
    for (unsigned crit = 0; crit < 2; ++crit) {
        unsigned base = crit ? critical : normal, weight = crit ? 1 : 23;
        for (unsigned spread = 85; spread <= 100; ++spread) {
            unsigned v = damage_spread(base,spread);
            f.raw_numerator += weight * v;
            f.useful_numerator += weight * (v < hp ? v : hp);
            f.ko_numerator += weight * (hp != 0 && v >= hp);
            if (v < f.minimum) f.minimum = v;
            if (v > f.maximum) f.maximum = v;
        }
    }
    return f;
}
/* 1 legal feature, 0 depleted/fainted action, -1 unsupported input/profile. */
static int move_features(const Battle *b, int s, int slot, DamageFeatures *out) {
    if (s < 0 || s > 1 || slot < 0 || slot >= SLOTS || !validate(b)) return -1;
    int a = b->active[s], d = b->active[1-s];
    if (!b->hp[s][a] || !b->hp[1-s][d] || !b->pp[s][a][slot]) return 0;
    const Move *m = &moves[b->move[s][a][slot]];
    unsigned mod = matchup_modifier(b,s,a,d,m);
    unsigned attack = m->special ? b->sat[s][a] : b->atk[s][a];
    unsigned defense = m->special ? b->sdf[1-s][d] : b->def[1-s][d];
    unsigned normal = damage_base(b->level[s][a],m->power,attack,defense,0,mod);
    unsigned critical = damage_base(b->level[s][a],m->power,attack,defense,1,mod);
    *out = aggregate_features(normal,critical,b->hp[1-s][d]);
    return 1;
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
    /* Independent hand-worked 80BP neutral example, lvl50, atk=def=100:
     * noncrit rolls sum540; crit rolls sum791; HP40 caps all crit rolls to40.
     * Thus raw=(23*540+791)/384, useful=(23*540+640)/384, KO=16/384.
     */
    DamageFeatures known = aggregate_features(37,54,40);
    assert(known.raw_numerator == 13211 && known.useful_numerator == 13060);
    assert(known.ko_numerator == 16 && known.minimum == 31 && known.maximum == 54);
    DamageFeatures immune = aggregate_features(0,0,40);
    assert(!immune.raw_numerator && !immune.useful_numerator && !immune.ko_numerator);
    assert(aggregate_features(37,54,1).ko_numerator == 384);
    Battle matchup = fixture(1);
    matchup.move[0][0][0] = 5; matchup.pp[0][0][0] = moves[5].pp; /* DRAGON_CLAW */
    matchup.level[0][0] = 50; matchup.atk[0][0] = matchup.def[1][0] = 100;
    matchup.types[0][0][0] = matchup.types[0][0][1] = 0; /* No Dragon STAB. */
    matchup.types[1][0][0] = matchup.types[1][0][1] = 0;
    matchup.hp[1][0] = 40;
    DamageFeatures matched;
    assert(move_features(&matchup,0,0,&matched) == 1);
    assert(matched.raw_numerator == 13211 && matched.useful_numerator == 13060);
    assert(matched.ko_numerator == 16 && matched.minimum == 31 && matched.maximum == 54);
    matchup.move[0][0][0] = 10; matchup.pp[0][0][0] = moves[10].pp; /* SCRATCH */
    matchup.types[1][0][0] = matchup.types[1][0][1] = 7; /* Ghost immunity. */
    assert(move_features(&matchup,0,0,&matched) == 1);
    assert(!matched.raw_numerator && !matched.useful_numerator && !matched.ko_numerator);
    Battle feature_state = fixture(1), unchanged = feature_state;
    DamageFeatures demo;
    assert(move_features(&feature_state,0,0,&demo) == 1);
    assert(demo.ko_numerator <= 384 && demo.useful_numerator <= 384u*feature_state.hp[1][0]);
    assert(memcmp(&feature_state,&unchanged,sizeof(feature_state)) == 0);
    feature_state.pp[0][0][0] = 0;
    assert(move_features(&feature_state,0,0,&demo) == 0);
    assert(move_features(&bad,0,0,&demo) == -1);
    if (argc > 1 && !strcmp(argv[1],"--features")) {
        Battle b = fixture(1); b.hp[0][0] = b.hp[1][0] = 40;
        Battle before = b;
        puts("experimental deterministic features, 17-move profile; denominator=384");
        puts("cost/slot: 2 damage-base evaluations +32 spread evaluations; GBC feature latency unmeasured");
        for (int side = 0; side < 2; ++side) for (int slot = 0; slot < SLOTS; ++slot) {
            DamageFeatures f;
            int status = move_features(&b,side,slot,&f);
            if (status != 1) { printf("side=%d slot=%d status=%d\n",side,slot,status); continue; }
            printf("side=%d slot=%d move=%s raw=%u/384 useful=%u/384 ko=%u/384 min=%u max=%u\n",
                side,slot,moves[b.move[side][b.active[side]][slot]].name,
                f.raw_numerator,f.useful_numerator,f.ko_numerator,f.minimum,f.maximum);
        }
        assert(memcmp(&b,&before,sizeof(b)) == 0);
        puts("feature extraction left complete state and RNG unchanged");
        return 0;
    }
    if (argc > 1 && !strcmp(argv[1],"--self-test")) { puts("stage-order, unsupported-move and deterministic-feature checks passed; no ROM parity claim"); return 0; }
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
