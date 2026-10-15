/* Mixed HP/HUD frontend projection with source-recorded combat Random events.
 * Remaining presentation hooks are explicit. This is not full frontend fidelity. */
#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <mgba/core/core.h>
#include <mgba/gb/core.h>
#include <mgba/internal/gb/gb.h>
#include <mgba/internal/sm83/sm83.h>
#include <mgba-util/vfs.h>
typedef struct {
 uint8_t a,f,b,c,d,e,h,l; uint16_t sp,pc,bank; uint8_t m[65536];
 uint32_t steps,fault,event_count,event_pos; uint8_t events[4096];
} State;
typedef struct {uint16_t bank,pc;} Location;
typedef struct {
 uint16_t bank,caller,origin_bank,origin_pc; uint8_t a,f,add,sub;
} Draw;
typedef struct {
 struct mCore *core; uint32_t frames,ui_entries,ui_draws,used;
 uint32_t reads[65536],writes[65536];
 uint8_t dirty[65536];uint16_t consumer_pc[65536],consumer_bank[65536];
 uint16_t source_pc,source_bank;int in_ui;
 uint32_t source_tick,consumer_step[65536];
 uint8_t (*load)(struct SM83Core*,uint16_t);
 void (*store)(struct SM83Core*,uint16_t,int8_t);
} Context;
static Context contexts[2],*active;
static Draw draws[1024];static uint32_t count;
static uint8_t traced_load(struct SM83Core *cpu,uint16_t address){
 active->reads[address]++;
 if(!active->in_ui && active->dirty[address] && !active->consumer_pc[address]){
  active->consumer_pc[address]=active->source_pc;active->consumer_bank[address]=active->source_bank;
  active->consumer_step[address]=active->source_tick;
 }
 return active->load(cpu,address);
}
static void traced_store(struct SM83Core *cpu,uint16_t address,int8_t value){
 active->writes[address]++;active->dirty[address]=active->in_ui!=0;
 active->store(cpu,address,value);
}
static int match(struct SM83Core *cpu,Location location){
 return cpu->pc==location.pc && (location.pc<0x4000 ||
  cpu->memory.currentSegment(cpu,0x4000)==location.bank);
}
static void save(Context *ctx,State *s){
 struct SM83Core *cpu=ctx->core->cpu;
 s->a=cpu->a;s->f=cpu->f.packed;s->b=cpu->b;s->c=cpu->c;
 s->d=cpu->d;s->e=cpu->e;s->h=cpu->h;s->l=cpu->l;
 s->sp=cpu->sp;s->pc=cpu->pc;s->bank=cpu->memory.currentSegment(cpu,0x4000);
 for(unsigned a=0xc000;a<0xe000;a++)s->m[a]=ctx->core->busRead8(ctx->core,a);
 for(unsigned a=0xff80;a<0xffff;a++)s->m[a]=ctx->core->busRead8(ctx->core,a);
}
int turn_open(const char *path){
 count=0;
 for(int i=0;i<2;i++){
  Context *ctx=&contexts[i];memset(ctx,0,sizeof(*ctx));ctx->core=GBCoreCreate();
  if(!ctx->core || !ctx->core->init(ctx->core))return 1;
  mCoreInitConfig(ctx->core,NULL);struct VFile *vf=VFileOpen(path,O_RDONLY);
  if(!vf || !ctx->core->loadROM(ctx->core,vf))return 2;
  ctx->core->reset(ctx->core);ctx->core->busWrite8(ctx->core,0xff40,0);
 }
 return 0;
}
void turn_close(void){for(int i=0;i<2;i++)if(contexts[i].core){
 struct mCore *core=contexts[i].core;mCoreConfigDeinit(&core->config);core->deinit(core);contexts[i].core=NULL;
}}
/* One complete source caller. Core clocks, VRAM and peripherals persist between
 * successive caller phases. Host state is copied only at these explicit phases. */
int turn_run(State *s,unsigned mode,uint32_t budget,const Location *hooks,unsigned nh,
 const Location *real_ui,unsigned nu,uint16_t random_pc,uint16_t add_pc,
 uint16_t sub_pc,uint16_t crash_pc,uint16_t vblank_pc,
 uint16_t farcall_return,uint16_t donothing_pc,uint16_t range_return,uint32_t *stats){
 if(mode>1 || !contexts[mode].core)return 9;
 Context *ctx=&contexts[mode];struct mCore *core=ctx->core;struct SM83Core *cpu=core->cpu;
 core->busWrite8(core,0xff70,1);core->busWrite8(core,0xffff,0);core->busWrite8(core,0xff0f,0);
 for(unsigned a=0xc000;a<0xe000;a++)core->busWrite8(core,a,s->m[a]);
 for(unsigned a=0xff80;a<0xffff;a++)core->busWrite8(core,a,s->m[a]);
 core->busWrite8(core,0x2000,s->bank);
 cpu->a=s->a;cpu->f.packed=s->f;cpu->b=s->b;cpu->c=s->c;
 cpu->d=s->d;cpu->e=s->e;cpu->h=s->h;cpu->l=s->l;
 cpu->sp=s->sp;cpu->pc=s->pc;cpu->halted=false;cpu->irqPending=false;
 cpu->memory.setActiveRegion(cpu,cpu->pc);
 if(mode){core->busWrite8(core,0xff40,0x91);core->busWrite8(core,0xffff,1);}
 ((struct GB*)cpu->master)->memory.ime=mode!=0;
 ctx->load=cpu->memory.load8;ctx->store=cpu->memory.store8;
 cpu->memory.load8=traced_load;cpu->memory.store8=traced_store;active=ctx;
 uint16_t ui_sp=0,ui_ret=0,random_sp=0,random_ret=0,random_bank=0;
 int in_ui=0,in_random=0;uint8_t preserved[6];
 uint16_t random_origin_bank=0,random_origin_pc=0;
 s->steps=s->fault=0;
 while(cpu->pc!=0xfffe && s->steps<budget && !s->fault){
  ctx->source_tick++;ctx->source_pc=cpu->pc;ctx->source_bank=cpu->memory.currentSegment(cpu,0x4000);
  if(cpu->pc==crash_pc){s->fault=8;break;}
  if(cpu->pc==vblank_pc)ctx->frames++;
  if(in_ui && cpu->sp==ui_sp+2 && cpu->pc==ui_ret){in_ui=0;ctx->in_ui=0;}
  if(in_random && cpu->sp==random_sp+2 && cpu->pc==random_ret){
   if(memcmp(preserved,(uint8_t[]){cpu->b,cpu->c,cpu->d,cpu->e,cpu->h,cpu->l},6)){s->fault=10;break;}
   if(count>=1024){s->fault=11;break;}
   draws[count++]=(Draw){random_bank,random_ret,random_origin_bank,random_origin_pc,cpu->a,cpu->f.packed,
    core->busRead8(core,add_pc),core->busRead8(core,sub_pc)};
   in_random=0;
  }
  if(mode && !in_ui)for(unsigned i=0;i<nu;i++)if(match(cpu,real_ui[i])){
   ui_sp=cpu->sp;ui_ret=core->busRead8(core,ui_sp)|(uint16_t)core->busRead8(core,ui_sp+1)<<8;
   in_ui=1;ctx->in_ui=1;ctx->ui_entries++;break;
  }
  if(cpu->pc==random_pc){
   uint16_t ret=core->busRead8(core,cpu->sp)|(uint16_t)core->busRead8(core,cpu->sp+1)<<8;
   uint16_t bank=cpu->memory.currentSegment(cpu,0x4000);
   uint16_t origin_bank=bank,origin_pc=ret;
   if(ret==farcall_return){
    origin_bank=core->busRead8(core,cpu->sp+2);
    origin_pc=core->busRead8(core,cpu->sp+3)|(uint16_t)core->busRead8(core,cpu->sp+4)<<8;
    /* Actual RST farjp substitutes DoNothing, then the outer CALL's return is
     * immediately above that source wrapper frame. Never guess a host caller. */
    if(origin_pc==donothing_pc){
     origin_pc=core->busRead8(core,cpu->sp+5)|(uint16_t)core->busRead8(core,cpu->sp+6)<<8;
     /* BattleRandomRange preserves BC below its outer return. This exact
      * source loop caller is shared by many logically distinct move checks. */
     if(origin_pc==range_return)
      origin_pc=core->busRead8(core,cpu->sp+9)|(uint16_t)core->busRead8(core,cpu->sp+10)<<8;
    }
   }
   if(in_ui)ctx->ui_draws++;
   else if(!mode){
    if(in_random){s->fault=12;break;}
    in_random=1;random_sp=cpu->sp;random_ret=ret;random_bank=bank;
    random_origin_bank=origin_bank;random_origin_pc=origin_pc;
    memcpy(preserved,(uint8_t[]){cpu->b,cpu->c,cpu->d,cpu->e,cpu->h,cpu->l},6);
   }else{
    if(ctx->used>=count){s->fault=13;break;}
    Draw draw=draws[ctx->used++];
    if(draw.bank!=bank || draw.caller!=ret || draw.origin_bank!=origin_bank || draw.origin_pc!=origin_pc){s->fault=14;break;}
    cpu->a=draw.a;cpu->f.packed=draw.f;core->busWrite8(core,add_pc,draw.add);core->busWrite8(core,sub_pc,draw.sub);
    cpu->sp+=2;cpu->pc=ret;cpu->memory.setActiveRegion(cpu,ret);continue;
   }
  }
  int returned=0;
  if(!in_ui)for(unsigned i=0;i<nh;i++)if(match(cpu,hooks[i])){
   uint16_t ret=core->busRead8(core,cpu->sp)|(uint16_t)core->busRead8(core,cpu->sp+1)<<8;
   if(cpu->sp<0xc000 || cpu->sp>=0xdffe){s->fault=15;break;}
   cpu->sp+=2;cpu->pc=ret;cpu->memory.setActiveRegion(cpu,ret);returned=1;break;
  }
  if(!returned && !s->fault)core->step(core);s->steps++;
 }
 if(cpu->pc!=0xfffe && !s->fault)s->fault=3;
 if(in_random && !s->fault)s->fault=16;
 cpu->memory.load8=ctx->load;cpu->memory.store8=ctx->store;active=NULL;
 save(ctx,s);stats[0]=count;stats[1]=ctx->used;stats[2]=ctx->frames;stats[3]=ctx->ui_entries;stats[4]=ctx->ui_draws;
 return s->fault;
}
unsigned turn_draw_count(void){return count;}
const Draw *turn_draws(void){return draws;}
/* Read/write counters are diagnostics for locating the first scratch consumer,
 * not permission to mask an address from the gameplay projection. */
const uint32_t *turn_reads(unsigned mode){return contexts[mode].reads;}
const uint32_t *turn_writes(unsigned mode){return contexts[mode].writes;}

const uint16_t *turn_consumer_pc(unsigned mode){return contexts[mode].consumer_pc;}
const uint16_t *turn_consumer_bank(unsigned mode){return contexts[mode].consumer_bank;}
const uint32_t *turn_consumer_step(unsigned mode){return contexts[mode].consumer_step;}
