/* Unhooked frontend probe: actual LCD, VBlank, DMA and interrupt execution. */
#include <stdint.h>
#include <stdlib.h>
#include <fcntl.h>
#include <mgba/core/core.h>
#include <mgba/gb/core.h>
#include <mgba/internal/gb/gb.h>
#include <mgba/internal/sm83/sm83.h>
#include <mgba-util/vfs.h>

typedef struct {
 uint8_t a,f,b,c,d,e,h,l;
 uint16_t sp,pc,bank;
 uint8_t m[65536];
 uint32_t steps,fault,event_count,event_pos;
 uint8_t events[4096];
} State;
static struct mCore *core;
static uint8_t (*original_load)(struct SM83Core*,uint16_t);
static State *active;
static uint8_t record_load(struct SM83Core *cpu,uint16_t address) {
 uint8_t value=original_load(cpu,address);
 if(address==0xff04) {
  if(active->event_count<sizeof(active->events))active->events[active->event_count++]=value;
  else active->fault=4;
 }
 return value;
}
int projection_open(const char *path) {
 core=GBCoreCreate();
 if(!core || !core->init(core))return 1;
 mCoreInitConfig(core,NULL);
 struct VFile *vf=VFileOpen(path,O_RDONLY);
 return (!vf || !core->loadROM(core,vf)) ? 2 : 0;
}
void projection_close(void) {
 if(core){mCoreConfigDeinit(&core->config);core->deinit(core);core=NULL;}
}
int projection_run(State *s,uint32_t budget,uint16_t crash_pc,uint16_t vblank_pc,
                   uint32_t *vblanks) {
 if(!core)return 5;
 core->reset(core);
 core->busWrite8(core,0xff40,0);
 core->busWrite8(core,0xffff,0);
 core->busWrite8(core,0xff0f,0);
 core->busWrite8(core,0xff70,1);
 for(unsigned a=0xc000;a<0xe000;a++)core->busWrite8(core,a,s->m[a]);
 for(unsigned a=0xff80;a<0xffff;a++)core->busWrite8(core,a,s->m[a]);
 core->busWrite8(core,0x2000,s->bank);
 struct SM83Core *cpu=core->cpu;
 cpu->a=s->a;cpu->f.packed=s->f;cpu->b=s->b;cpu->c=s->c;
 cpu->d=s->d;cpu->e=s->e;cpu->h=s->h;cpu->l=s->l;
 cpu->sp=s->sp;cpu->pc=s->pc;cpu->halted=false;cpu->irqPending=false;
 cpu->memory.setActiveRegion(cpu,cpu->pc);
 core->busWrite8(core,0xff40,0x91);
 core->busWrite8(core,0xffff,1);
 ((struct GB*)cpu->master)->memory.ime=true;
 original_load=cpu->memory.load8;cpu->memory.load8=record_load;active=s;
 s->steps=s->fault=s->event_count=s->event_pos=0;*vblanks=0;
 while(cpu->pc!=0xfffe && s->steps<budget && !s->fault) {
  if(cpu->pc==crash_pc){s->fault=8;break;}
  if(cpu->pc==vblank_pc)(*vblanks)++;
  core->step(core);s->steps++;
 }
 if(cpu->pc!=0xfffe && !s->fault)s->fault=3;
 cpu->memory.load8=original_load;active=NULL;
 s->a=cpu->a;s->f=cpu->f.packed;s->b=cpu->b;s->c=cpu->c;
 s->d=cpu->d;s->e=cpu->e;s->h=cpu->h;s->l=cpu->l;
 s->sp=cpu->sp;s->pc=cpu->pc;
 for(unsigned a=0xc000;a<0xe000;a++)s->m[a]=core->busRead8(core,a);
 for(unsigned a=0xff80;a<0xffff;a++)s->m[a]=core->busRead8(core,a);
 s->bank=cpu->memory.currentSegment(cpu,0x4000);
 return s->fault;
}
