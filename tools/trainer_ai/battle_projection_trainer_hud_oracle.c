/* Actual trainer HUD against its one declared graphics leaf omission.
 * Both modes execute all StageBallTilesData/OAM source instructions. */
#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <mgba/core/core.h>
#include <mgba/gb/core.h>
#include <mgba/internal/gb/gb.h>
#include <mgba/internal/sm83/sm83.h>
#include <mgba-util/vfs.h>
typedef struct {
 uint8_t a,f,b,c,d,e,h,l;uint16_t sp,pc,bank;uint8_t m[65536];
 uint32_t steps,fault,event_count,event_pos;uint8_t events[4096];
} State;
typedef struct {uint16_t bank,pc;} Location;
typedef struct {uint16_t random,add,sub,farcall_return,donothing,range_return;} RandomABI;
typedef struct {uint16_t bank,ret,origin_bank,origin_pc;uint8_t a,f,add,sub;} Draw;
static Draw draws[128];static unsigned draw_count;
static struct mCore *core;
int hud_open(const char *path){
 core=GBCoreCreate();if(!core||!core->init(core))return 1;
 mCoreInitConfig(core,NULL);struct VFile *vf=VFileOpen(path,O_RDONLY);
 return !vf||!core->loadROM(core,vf)?2:0;
}
void hud_close(void){if(core){mCoreConfigDeinit(&core->config);core->deinit(core);core=NULL;}}
static int run_mode(State*s,unsigned actual,uint32_t budget,uint16_t leaf_bank,uint16_t leaf_pc,
 uint16_t crash_pc,uint16_t vblank_pc,uint32_t*stats,const Location*hooks,unsigned nh,const RandomABI*abi){
 if(!core||actual>1)return 9;
 core->reset(core);core->busWrite8(core,0xff40,0);core->busWrite8(core,0xffff,0);core->busWrite8(core,0xff0f,0);
 core->busWrite8(core,0xff70,1);
 for(unsigned a=0xc000;a<0xe000;a++)core->busWrite8(core,a,s->m[a]);
 for(unsigned b=2;b<8;b++){
  core->busWrite8(core,0xff70,b);
  for(unsigned i=0;i<0x1000;i++)core->busWrite8(core,0xd000+i,s->m[(b-2)*0x1000+i]);
 }
 core->busWrite8(core,0xff70,s->m[0xff70]&7);
 for(unsigned a=0xff80;a<0xffff;a++)core->busWrite8(core,a,s->m[a]);
 core->busWrite8(core,0x2000,s->bank);
 struct SM83Core*cpu=core->cpu;
 cpu->a=s->a;cpu->f.packed=s->f;cpu->b=s->b;cpu->c=s->c;
 cpu->d=s->d;cpu->e=s->e;cpu->h=s->h;cpu->l=s->l;
 cpu->sp=s->sp;cpu->pc=s->pc;cpu->halted=false;cpu->irqPending=false;
 cpu->memory.setActiveRegion(cpu,cpu->pc);
 if(actual){core->busWrite8(core,0xff40,0x91);core->busWrite8(core,0xffff,1);}
 ((struct GB*)cpu->master)->memory.ime=actual!=0;
 s->steps=s->fault=0;stats[0]=stats[1]=0;
 const uint16_t hud_bank=s->bank,hud_pc=s->pc;
 uint16_t ui_sp=s->sp,ui_ret=core->busRead8(core,s->sp)|(uint16_t)core->busRead8(core,s->sp+1)<<8;
 int in_hud=1,in_random=0;unsigned used=0,ui_draws=0;
 uint16_t random_sp=0,random_ret=0;Draw pending;uint8_t preserved[6];
 if(abi&&!actual)draw_count=0;
 while(cpu->pc!=0xfffe && s->steps<budget && !s->fault){
  if(cpu->pc==crash_pc){s->fault=8;break;}
  if(cpu->pc==vblank_pc)stats[0]++;
  unsigned bank=cpu->memory.currentSegment(cpu,0x4000);
  if(in_hud && cpu->sp==ui_sp+2 && cpu->pc==ui_ret)in_hud=0;
  if(!in_hud && cpu->pc==hud_pc && bank==hud_bank){
   in_hud=1;ui_sp=cpu->sp;ui_ret=core->busRead8(core,ui_sp)|(uint16_t)core->busRead8(core,ui_sp+1)<<8;
  }
  if(in_random && cpu->sp==random_sp+2 && cpu->pc==random_ret){
   if(draw_count>=128 || memcmp(preserved,(uint8_t[]){cpu->b,cpu->c,cpu->d,cpu->e,cpu->h,cpu->l},6)){s->fault=10;break;}
   pending.a=cpu->a;pending.f=cpu->f.packed;pending.add=core->busRead8(core,abi->add);pending.sub=core->busRead8(core,abi->sub);
   draws[draw_count++]=pending;in_random=0;
  }
  if(abi && cpu->pc==abi->random){
   if(in_hud)ui_draws++;
   else{
    uint16_t ret=core->busRead8(core,cpu->sp)|(uint16_t)core->busRead8(core,cpu->sp+1)<<8;
    uint16_t origin_bank=bank,origin_pc=ret;
    if(ret==abi->farcall_return){
     origin_bank=core->busRead8(core,cpu->sp+2);
     origin_pc=core->busRead8(core,cpu->sp+3)|(uint16_t)core->busRead8(core,cpu->sp+4)<<8;
     if(origin_pc==abi->donothing){
      origin_pc=core->busRead8(core,cpu->sp+5)|(uint16_t)core->busRead8(core,cpu->sp+6)<<8;
      if(origin_pc==abi->range_return)origin_pc=core->busRead8(core,cpu->sp+9)|(uint16_t)core->busRead8(core,cpu->sp+10)<<8;
     }
    }
    if(!actual){
     if(in_random){s->fault=11;break;}
     pending=(Draw){bank,ret,origin_bank,origin_pc,0,0,0,0};in_random=1;random_sp=cpu->sp;random_ret=ret;
     memcpy(preserved,(uint8_t[]){cpu->b,cpu->c,cpu->d,cpu->e,cpu->h,cpu->l},6);
    }else{
     if(used>=draw_count){s->fault=12;break;}Draw draw=draws[used++];
     if(draw.bank!=bank||draw.ret!=ret||draw.origin_bank!=origin_bank||draw.origin_pc!=origin_pc){s->fault=13;break;}
     cpu->a=draw.a;cpu->f.packed=draw.f;core->busWrite8(core,abi->add,draw.add);core->busWrite8(core,abi->sub,draw.sub);
     cpu->sp+=2;cpu->pc=ret;cpu->memory.setActiveRegion(cpu,ret);s->steps++;continue;
    }
   }
  }
  if(abi&&!in_hud){
   int returned=0;
   for(unsigned i=0;i<nh;i++)if(cpu->pc==hooks[i].pc&&(cpu->pc<0x4000||bank==hooks[i].bank)){
    if(cpu->sp<0xc000||cpu->sp>=0xdffe){s->fault=7;break;}
    uint16_t ret=core->busRead8(core,cpu->sp)|(uint16_t)core->busRead8(core,cpu->sp+1)<<8;
    cpu->sp+=2;cpu->pc=ret;cpu->memory.setActiveRegion(cpu,ret);returned=1;break;
   }
   if(returned){s->steps++;continue;}
   if(s->fault)break;
  }
  if(cpu->pc==leaf_pc && (leaf_pc<0x4000 || bank==leaf_bank)){
   stats[1]++;
   if(!actual){
    if(cpu->sp<0xc000||cpu->sp>=0xdffe){s->fault=7;break;}
    uint16_t ret=core->busRead8(core,cpu->sp)|(uint16_t)core->busRead8(core,cpu->sp+1)<<8;
    cpu->sp+=2;cpu->pc=ret;cpu->memory.setActiveRegion(cpu,ret);s->steps++;continue;
   }
  }
  core->step(core);s->steps++;
 }
 if(cpu->pc!=0xfffe && !s->fault)s->fault=3;
 if(abi){stats[2]=draw_count;stats[3]=used;stats[4]=ui_draws;
  if(in_random&&!s->fault)s->fault=14;
  if(actual&&used!=draw_count&&!s->fault)s->fault=15;
 }
 s->a=cpu->a;s->f=cpu->f.packed;s->b=cpu->b;s->c=cpu->c;
 s->d=cpu->d;s->e=cpu->e;s->h=cpu->h;s->l=cpu->l;
 s->sp=cpu->sp;s->pc=cpu->pc;s->bank=cpu->memory.currentSegment(cpu,0x4000);
 unsigned selected=core->busRead8(core,0xff70)&7;s->m[0xff70]=selected;
 for(unsigned b=2;b<8;b++){
  core->busWrite8(core,0xff70,b);
  for(unsigned i=0;i<0x1000;i++)s->m[(b-2)*0x1000+i]=core->busRead8(core,0xd000+i);
 }
 core->busWrite8(core,0xff70,1);
 for(unsigned a=0xc000;a<0xe000;a++)s->m[a]=core->busRead8(core,a);
 core->busWrite8(core,0xff70,selected);
 for(unsigned a=0xff80;a<0xffff;a++)s->m[a]=core->busRead8(core,a);
 return s->fault;
}

int hud_run(State*s,unsigned actual,uint32_t budget,uint16_t leaf_bank,uint16_t leaf_pc,
 uint16_t crash_pc,uint16_t vblank_pc,uint32_t*stats){
 return run_mode(s,actual,budget,leaf_bank,leaf_pc,crash_pc,vblank_pc,stats,NULL,0,NULL);
}
int hud_continue(State*s,unsigned actual,uint32_t budget,uint16_t leaf_bank,uint16_t leaf_pc,
 uint16_t crash_pc,uint16_t vblank_pc,uint32_t*stats,const Location*hooks,unsigned nh,const RandomABI*abi){
 return run_mode(s,actual,budget,leaf_bank,leaf_pc,crash_pc,vblank_pc,stats,hooks,nh,abi);
}
unsigned hud_draw_count(void){return draw_count;}
const Draw* hud_draws(void){return draws;}
