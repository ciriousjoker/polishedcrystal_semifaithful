/* Seeded adapter for the existing bounded synthetic slice, never a learner. */
#define main simulator_fixture_main
#include "simulator.c"
#undef main
#include <errno.h>
int main(int argc, char **argv) {
    if (argc != 3) return 2;
    char *end;
    errno = 0;
    unsigned long count = strtoul(argv[1], &end, 10);
    if (errno || *end || !count || count > 1000000) return 2;
    unsigned long seed = strtoul(argv[2], &end, 10);
    if (errno || *end || !seed || seed > UINT32_MAX - count) return 2;
    uint64_t wins[2] = {0}, rejected = 0, truncated = 0;
    for (unsigned long i = 0; i < count; ++i) {
        Battle battle = fixture((uint32_t)(seed + i));
        int result = rollout(&battle);
        if (result >= 0) ++wins[result];
        else if (result == -2) ++rejected;
        else ++truncated;
    }
    printf("{\"wins\":[%llu,%llu],\"unsupported\":%llu,\"truncated\":%llu}\n",
           (unsigned long long)wins[0], (unsigned long long)wins[1],
           (unsigned long long)rejected, (unsigned long long)truncated);
    return 0;
}
