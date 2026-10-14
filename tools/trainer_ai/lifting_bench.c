/* Compile with -DLIFTED_SOURCE='"/absolute/build/lifted.c"'. Single core. */
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include LIFTED_SOURCE
static double now(void) { struct timespec t; clock_gettime(CLOCK_MONOTONIC, &t); return t.tv_sec+t.tv_nsec*1e-9; }
static uint32_t next(uint32_t *x) { *x ^= *x << 13; *x ^= *x >> 17; *x ^= *x << 5; return *x; }
static unsigned manual(unsigned level,unsigned power,unsigned atk,unsigned def) {
    if(!def) def=1;
    unsigned x=(level*2/5+2)*power*atk/def/50;
    /* DamagePass4 preserves previous low byte on saturation, initialized to zero here. */
    if(x>=0xff00)return 0xff00;
    return x+2;
}
int main(int argc,char **argv) {
    unsigned count=argc>1?(unsigned)strtoul(argv[1],0,10):300000;
    uint32_t seed=1881; unsigned char vectors[1024][4];
    for(unsigned i=0;i<1024;i++)for(unsigned j=0;j<4;j++)vectors[i][j]=(unsigned char)next(&seed);
    State s={0};s.sp=0xfffe;unsigned long long check=0,ref=0;
    double start=now();
    for(unsigned i=0;i<count;i++){
        unsigned char *v=vectors[i%1024];s.b=v[2];s.c=v[3];s.d=v[1];s.e=v[0];s.m[0xc000]=s.m[0xc001]=0;
        lifted(&s,0);check+=(s.m[0xc000]<<8)|s.m[0xc001];
    }
    double elapsed=now()-start;start=now();
    for(unsigned i=0;i<count;i++){
        unsigned char *v=vectors[i%1024];ref+=manual(v[0],v[1],v[2],v[3]);
    }
    double hand=now()-start;
    if(check!=ref){fprintf(stderr,"Mismatch: %llu versus %llu\n",check,ref);return 1;}
    printf("{\"calls\":%u,\"lifted_seconds\":%.9f,\"lifted_calls_per_second\":%.3f,\"manual_seconds\":%.9f,\"checksum\":%llu}\n",count,elapsed,count/elapsed,hand,check);
    return 0;
}
