/* Provisional int4 dense inference. This fixture is untrained. */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

enum { INPUTS = 64, HIDDEN = 96, ACTIONS = 12,
       WEIGHTS = INPUTS * HIDDEN + HIDDEN * ACTIONS,
       MODEL_BYTES = 16 + WEIGHTS / 2 + 2 * (HIDDEN + ACTIONS) + HIDDEN };
/* Export: 16-byte header, packed low-nibble-first signed weights, little-endian
   int16 biases, then hidden-layer right shifts. No native struct serialization. */
static uint8_t model[MODEL_BYTES];
static int16_t products[256];
static int weight(unsigned i) {
    unsigned n = (model[16 + i / 2] >> (4 * (i & 1))) & 15;
    return n < 8 ? (int)n : (int)n - 16;
}
static int bias(unsigned i) {
    unsigned p = 16 + WEIGHTS / 2 + 2 * i;
    unsigned n = model[p] | ((unsigned)model[p + 1] << 8);
    return n < 32768 ? (int)n : (int)n - 65536;
}
static int floor_shift(int v, unsigned shift) {
    int divisor = 1 << shift;
    return v >= 0 ? v / divisor : -((-v + divisor - 1) / divisor);
}
static void infer(const uint8_t inputs[INPUTS], int16_t logits[ACTIONS], int lookup) {
    uint8_t hidden[HIDDEN];
    for (unsigned j = 0; j < HIDDEN; ++j) {
        int32_t sum = bias(j);
        for (unsigned i = 0; i < INPUTS; ++i)
            sum += lookup ? products[(inputs[i] << 4) | (weight(j * INPUTS + i) & 15)] :
                   (int32_t)inputs[i] * weight(j * INPUTS + i);
        int v = floor_shift(sum, model[16 + WEIGHTS / 2 + 2 * (HIDDEN + ACTIONS) + j]);
        hidden[j] = (uint8_t)(v < 0 ? 0 : v > 15 ? 15 : v);
    }
    for (unsigned j = 0; j < ACTIONS; ++j) {
        int32_t sum = bias(HIDDEN + j);
        for (unsigned i = 0; i < HIDDEN; ++i)
            sum += lookup ? products[(hidden[i] << 4) | (weight(INPUTS * HIDDEN + j * HIDDEN + i) & 15)] :
                   (int32_t)hidden[i] * weight(INPUTS * HIDDEN + j * HIDDEN + i);
        logits[j] = (int16_t)sum;
    }
}
/* Moves 0..3, party destinations 4..9, items 10..11. Lowest legal slot wins ties.
   -1 tells the caller that no legal action exists; no invalid slot is selected. */
static int select_action(const int16_t logits[ACTIONS], uint16_t legal) {
    int best = -1;
    for (unsigned i = 0; i < ACTIONS; ++i)
        if ((legal & (1u << i)) && (best < 0 || logits[i] > logits[best])) best = (int)i;
    return best;
}
static int validate(void) {
    if (memcmp(model, "TAI4", 4) || model[4] != 1 || model[5] != INPUTS ||
        model[6] != HIDDEN || model[7] != ACTIONS) return 0;
    for (unsigned i = 8; i < 16; ++i) if (model[i]) return 0;
    for (unsigned i = 0; i < HIDDEN + ACTIONS; ++i)
        if (bias(i) < -16384 || bias(i) > 16384) return 0;
    for (unsigned i = 0; i < HIDDEN; ++i)
        if (model[16 + WEIGHTS / 2 + 2 * (HIDDEN + ACTIONS) + i] > 15) return 0;
    return 1;
}
int main(int argc, char **argv) {
    int benchmark = argc == 2 && !strcmp(argv[1], "bench");
    for (unsigned x = 0; x < 16; ++x)
        for (unsigned w = 0; w < 16; ++w)
            products[(x << 4) | w] = (int16_t)((int)x * (w < 8 ? (int)w : (int)w - 16));
    memcpy(model, "TAI4", 4); model[4] = 1; model[5] = INPUTS;
    model[6] = HIDDEN; model[7] = ACTIONS;
    /* Deterministic stress fixture, not a learned policy. */
    for (unsigned i = 16; i < 16 + WEIGHTS / 2; ++i) model[i] = (uint8_t)(i * 73 + 19);
    for (unsigned i = 0; i < HIDDEN + ACTIONS; ++i) {
        unsigned offset = 16 + WEIGHTS / 2 + 2 * i;
        unsigned value = i < HIDDEN ? 1024 : 33 * (i - HIDDEN);
        model[offset] = value & 255; model[offset + 1] = value >> 8;
    }
    memset(model + 16 + WEIGHTS / 2 + 2 * (HIDDEN + ACTIONS), 5, HIDDEN);
    if (argc == 3 && !strcmp(argv[1], "export")) {
        FILE *f = fopen(argv[2], "wb"); if (!f) return 2;
        int ok = fwrite(model, 1, sizeof model, f) == sizeof model;
        return fclose(f) || !ok ? 2 : 0;
    }
    if (argc == 3 && !strcmp(argv[1], "load")) {
        FILE *f = fopen(argv[2], "rb"); if (!f) return 2;
        int ok = fread(model, 1, sizeof model, f) == sizeof model && fgetc(f) == EOF;
        fclose(f); if (!ok) return 2;
    } else if (argc != 1 && !benchmark) {
        fprintf(stderr, "usage: %s [export PATH | load PATH | bench]\n", argv[0]); return 2;
    }
    if (!validate()) return 3;
    uint8_t input[INPUTS]; int16_t logits[ACTIONS];
    for (unsigned i = 0; i < INPUTS; ++i) input[i] = i & 15;
    infer(input, logits, 0);
    int16_t reference[ACTIONS]; infer(input, reference, 1);
    if (memcmp(logits, reference, sizeof logits)) return 4;
    int16_t tied[ACTIONS] = {0};
    if (select_action(tied, 0) != -1 || select_action(tied, 0xfff) != 0 ||
        select_action(tied, 0x3f0) != 4 || select_action(tied, 1u << 11) != 11) return 5;
    if (benchmark) {
        const unsigned iterations = 10000;
        for (int lookup = 0; lookup < 2; ++lookup) {
            volatile int64_t checksum = 0;
            clock_t start = clock();
            for (unsigned iteration = 0; iteration < iterations; ++iteration) {
                input[0] = iteration & 15;
                infer(input, logits, lookup);
                checksum += logits[iteration % ACTIONS];
            }
            double seconds = (double)(clock() - start) / CLOCKS_PER_SEC;
            printf("mode=%s iterations=%u cpu_seconds=%.6f us_per_inference=%.3f checksum=%lld\n",
                   lookup ? "lookup" : "multiply", iterations, seconds,
                   seconds * 1000000 / iterations, (long long)checksum);
        }
        return 0;
    }
    printf("masked selection: normal=%d replacement=%d all_invalid=%d; ties choose lowest slot\n",
           select_action(logits, 0xfff), select_action(logits, 0x3f0), select_action(logits, 0));
    printf("untrained fixture; model_bytes=%u; logits=", MODEL_BYTES);
    for (unsigned i = 0; i < ACTIONS; ++i) printf("%s%d", i ? "," : "", logits[i]);
    puts(""); return 0;
}
