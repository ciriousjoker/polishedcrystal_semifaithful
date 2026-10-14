#include <algorithm>
#include <vector>
#include <utility>
#include <cassert>
#include <cstdio>
#include <cstring>
union Reg { unsigned short w; struct { unsigned char l,h; } b; Reg():w(0){} };
struct cpu_regs { Reg AF,BC,DE,HL; unsigned short SP=0,PC=0; };
struct CPU { unsigned char memory[65536]={}, ram[32768]={}; unsigned read(unsigned p) {return memory[p&65535];} unsigned char* get_ram(){return ram;} unsigned char oam[160]={},vram[0x4000]={}; unsigned char* get_oam(){return oam;} unsigned char* get_vram(){return vram;} };
struct ROM { unsigned char data[4*0x4000]={}; unsigned char* get_rom(){return data;} };
struct MBC { unsigned char* mapping; unsigned char* get_rom(){return mapping;} };
struct LCD { bool get_enable(unsigned){return true;} unsigned short palette[16][4]={},mapped[16][4]={}; unsigned short* get_pal(unsigned i){return palette[i];} unsigned short* get_mapped_pal(unsigned i){return mapped[i];} };
struct gb { LCD lcd; LCD* get_lcd(){return &lcd;} CPU cpu; ROM rom; MBC mbc; gb(){mbc.mapping=rom.data;} CPU* get_cpu(){return &cpu;} ROM* get_rom(){return &rom;} MBC* get_mbc(){return &mbc;} };
static unsigned mobile_observed_frames=17,mobile_native_object_address=0xc000,mobile_native_descriptor_address=0;
static unsigned char mobile_native_descriptors[104]={};
#include "profile_native.hpp"
static void stack(gb& g,unsigned sp,unsigned pc) {g.cpu.memory[sp]=pc&255;g.cpu.memory[sp+1]=pc>>8;}
int main(){
 gb g; cpu_regs r; r.SP=0xc100;r.PC=0x100;r.AF.w=0x1234;r.BC.w=0x5678;r.DE.w=0x86d0;r.HL.w=0x8610;
 acceptance_profile_before(&g,r);acceptance_profile_effect_io_write(&g,0xff55,5);
 assert(acceptance_effect_sequence==0 && acceptance_profile_effect_record(0,32)==0);
 acceptance_profile_effect_io_write(&g,0xff46,0xc0);assert(acceptance_oam_dma_count==0);
 acceptance_profile_effect_span(0,0,0x100);acceptance_profile_effect_span(1,2,0x4100);
 acceptance_profile_effect_watch(0,1,0xd100,2);g.cpu.ram[0x1100]=7;g.cpu.ram[0x1101]=8;
 stack(g,r.SP,0x200);g.mbc.mapping=g.rom.data+0x4000;acceptance_profile_before(&g,r);
 assert(acceptance_profile_effect_active(0)==1);assert(acceptance_profile_effect_record(0,4)==0);
 assert(acceptance_profile_effect_record(0,7)==0x1234);assert(acceptance_profile_effect_sample(0,0,1)==8);
 r.SP=0xc0fe;r.PC=0x4100;stack(g,r.SP,0x300);acceptance_profile_before(&g,r);
 assert(acceptance_profile_effect_active(0)&&acceptance_profile_effect_active(1));
 g.cpu.memory[0xff51]=0x55;g.cpu.memory[0xff52]=0x60;g.cpu.memory[0xff53]=6;g.cpu.memory[0xff54]=0x10;
 acceptance_profile_effect_io_write(&g,0xff55,11);assert(acceptance_profile_effect_record(2,3)==3);
 assert(acceptance_profile_effect_record(2,16)==11);assert(acceptance_profile_effect_record(2,20)==0x10);
 r.SP=0xc100;r.PC=0x300;g.mbc.mapping=g.rom.data+0x8000;acceptance_profile_after(&g,r,0xc9,16);
 assert(!acceptance_profile_effect_active(1)&&acceptance_profile_effect_active(0));
 r.PC=0x150;acceptance_profile_before(&g,r);r.SP=0xc102;r.PC=0x200;acceptance_profile_after(&g,r,0xc9,16);assert(!acceptance_profile_effect_active(0));
 acceptance_profile_effect_io_write(&g,0xff55,12);
 assert(acceptance_profile_effect_record(5,2)==~0u && acceptance_profile_effect_record(5,3)==3);
 // ROMX return must match the caller's actual bank, even with identical PC and SP.
 r.PC=0x4100;r.SP=0xc100;g.mbc.mapping=g.rom.data+0x4000;stack(g,r.SP,0x4200);acceptance_profile_before(&g,r);
 r.PC=0x4200;r.SP=0xc102;g.mbc.mapping=g.rom.data+0x8000;acceptance_profile_after(&g,r,0xc9,16);assert(acceptance_profile_effect_active(1));
 g.mbc.mapping=g.rom.data+0x4000;acceptance_profile_after(&g,r,0xc9,16);assert(!acceptance_profile_effect_active(1));
 // Existing marker-gated completed simulation remains active without native configuration.
 acceptance_profile_marker(0,2,0x4300);r.PC=0x4300;acceptance_profile_before(&g,r);assert(acceptance_profile_marker_count(0)==1);
 acceptance_profile_simulation(2,0x4400,1,0xc000,34,1,10);acceptance_profile_simulation_field(0,0);
 acceptance_profile_save_routine(2,0x4500);acceptance_profile_menu_routine(2,0x4600);
 r.PC=0x4400;r.SP=0xc100;stack(g,r.SP,0x4700);acceptance_profile_before(&g,r);
 r.PC=0x4700;r.SP=0xc102;acceptance_profile_before(&g,r);assert(acceptance_profile_simulation_record(0,2)==1);
 r.PC=0x4500;r.SP=0xc100;acceptance_profile_before(&g,r);assert(acceptance_profile_save_active());
 r.PC=0x4700;r.SP=0xc102;acceptance_profile_before(&g,r);assert(!acceptance_profile_save_active());
 r.PC=0x4600;r.SP=0xc100;acceptance_profile_before(&g,r);assert(acceptance_profile_menu_active());
 r.PC=0x4700;r.SP=0xc102;acceptance_profile_before(&g,r);assert(!acceptance_profile_menu_active());
 acceptance_profile_effect_reset();assert(!acceptance_profile_effect_record(0,32));assert(!acceptance_profile_simulation_record(0,2));assert(!acceptance_profile_marker_count(0));
 // Native ROM0 menu readiness must not use the ROMX bank-minus-one mapping.
 acceptance_profile_menu_routine(0,0x350);r.PC=0x350;r.SP=0xc100;stack(g,r.SP,0x360);
 g.mbc.mapping=g.rom.data+0x8000;acceptance_profile_before(&g,r);assert(acceptance_profile_menu_active());
 r.PC=0x360;r.SP=0xc102;acceptance_profile_before(&g,r);assert(!acceptance_profile_menu_active());
 g.cpu.memory[0xff02]=0x80;g.cpu.memory[0xffff]=8;
 assert(acceptance_profile_serial_status(0)==0x80 && acceptance_profile_serial_status(1)==8);
 acceptance_profile_effect_reset();assert(acceptance_profile_serial_status(0)==~0u);
 // Native readiness requires a completed configured world update, independently
 // of effect spans. Initial memory matches alone must never enable readiness.
 for(unsigned i=0;i<8;i++) acceptance_profile_native_scene_field(i,0,0xc200+i,i==4||i==6||i==7?~0u:i,255);
 for(unsigned i=0;i<8;i++)g.cpu.memory[0xc200+i]=i;g.cpu.memory[0xc204]=1;
 acceptance_profile_native_scene_field(5,0,0xc205,0,255);g.cpu.memory[0xc205]=0;acceptance_profile_native_scene_enable(8);
 acceptance_profile_before(&g,r);assert(!acceptance_profile_native_scene_ready());
 auto complete_world = [&](){
   g.mbc.mapping=g.rom.data+0x4000;r.PC=0x4400;r.SP=0xc100;
   stack(g,r.SP,0x4700);acceptance_profile_before(&g,r);
   r.PC=0x4410;acceptance_profile_before(&g,r);
   r.PC=0x4700;r.SP=0xc102;acceptance_profile_after(&g,r,0xc9,16);
 };
 // Incomplete local and party prerequisites remain unready after real returns.
 g.cpu.memory[0xc204]=0;complete_world();assert(!acceptance_profile_native_scene_ready());
 assert(!acceptance_native_world_completed);
 g.cpu.memory[0xc204]=1;acceptance_profile_before(&g,r);
 assert(!acceptance_profile_native_scene_ready()); // No new returned world update.
 complete_world();assert(acceptance_profile_native_scene_ready());
 acceptance_profile_effect_reset();g.cpu.memory[0xc206]=0;
 complete_world();assert(!acceptance_profile_native_scene_ready());
 assert(!acceptance_native_world_completed);
 g.cpu.memory[0xc206]=6;acceptance_profile_before(&g,r);
 assert(!acceptance_profile_native_scene_ready()); // Repairing party RAM cannot relatch.

 r.PC=0x4400;r.SP=0xc100;stack(g,r.SP,0x4700);acceptance_profile_before(&g,r);
 r.PC=0x4700;r.SP=0xc102;acceptance_profile_after(&g,r,0x00,16);
 assert(!acceptance_profile_native_scene_ready());
 complete_world();assert(acceptance_profile_native_scene_ready());
 g.cpu.memory[0xc203]=77;g.cpu.memory[0xc204]=0;
 acceptance_profile_before(&g,r);assert(acceptance_profile_native_scene_ready());g.cpu.memory[0xc201]=77;acceptance_profile_before(&g,r);assert(!acceptance_profile_native_scene_ready());
 g.cpu.memory[0xc201]=1;g.cpu.memory[0xc203]=3;g.cpu.memory[0xc204]=1;
 acceptance_profile_before(&g,r);assert(!acceptance_profile_native_scene_ready());
 complete_world();assert(acceptance_profile_native_scene_ready());
 acceptance_profile_effect_reset();acceptance_profile_before(&g,r);
 assert(!acceptance_profile_native_scene_ready());
 acceptance_profile_native_scene_enable(0);
 r.PC=0x100;r.SP=0xc100;stack(g,r.SP,0x200);
 for(unsigned i=0;i<2050;i++){acceptance_profile_before(&g,r);r.PC=0x200;r.SP=0xc102;acceptance_profile_after(&g,r,0xc9,16);r.PC=0x100;r.SP=0xc100;}
 assert(acceptance_profile_effect_record(0,32)==4096);assert(acceptance_profile_effect_record(0,33)>0);
 acceptance_profile_effect_reset();acceptance_effect_sequence=~0u;acceptance_profile_before(&g,r);assert(acceptance_profile_effect_record(0,33)==1);
 acceptance_profile_effect_reset();assert(acceptance_profile_effect_record(0,33)==0);

 // Actual dispatcher chronology: RET completes, then pending IRQ pushes PC/SP,
 // handler writes FF55, and RETI returns. The completed effect must stay closed.
 acceptance_profile_effect_watch(1,0,0xc210,1);
 r.PC=0x100;r.SP=0xc100;r.AF.w=0x1234;stack(g,r.SP,0x200);
 acceptance_profile_before(&g,r);
 r.PC=0x150;acceptance_profile_before(&g,r);
 g.cpu.memory[0xc210]=42;r.PC=0x200;r.SP=0xc102;
 acceptance_profile_after(&g,r,0xc9,16);
 assert(!acceptance_profile_effect_active(0));
 assert(acceptance_profile_effect_record(1,3)==2);
 assert(acceptance_profile_effect_record(1,7)==0x1234);
 assert(acceptance_profile_effect_sample(1,1,0)==42);
 // irq_process runs before the next before-instruction hook.
 r.PC=0x40;r.SP=0xc100;r.AF.w=0xabcd;g.cpu.memory[0xc210]=99;
 acceptance_profile_before(&g,r);acceptance_profile_effect_io_write(&g,0xff55,3);
 assert(acceptance_profile_effect_record(2,3)==3 && acceptance_profile_effect_record(2,2)==~0u);
 r.PC=0x200;r.SP=0xc102;acceptance_profile_after(&g,r,0xd9,16);
 assert(acceptance_profile_effect_record(0,32)==3);
 // Unsupported restores preserve the recorded history but invalidate its continuity.
 acceptance_profile_restore_boundary();assert(acceptance_profile_effect_record(0,33)==1);
 assert(acceptance_profile_effect_record(0,32)==3);
 r.PC=0x100;r.SP=0xc100;acceptance_profile_before(&g,r);
 assert(!acceptance_profile_effect_active(0));
 acceptance_profile_effect_reset();acceptance_profile_before(&g,r);
 assert(acceptance_profile_effect_active(0));acceptance_profile_restore_boundary();
 assert(!acceptance_profile_effect_active(0) && acceptance_profile_effect_record(0,33)==1);
 acceptance_profile_effect_reset();
 // A native-only configuration must also invalidate, and never relatch after restore.
 acceptance_profile_effect_span(0,0,0);acceptance_profile_effect_span(1,0,0);
 acceptance_profile_native_scene_enable(8);g.cpu.memory[0xc201]=1;g.cpu.memory[0xc203]=3;g.cpu.memory[0xc204]=1;
 acceptance_profile_before(&g,r);assert(!acceptance_profile_native_scene_ready());
 complete_world();assert(acceptance_profile_native_scene_ready());
 acceptance_profile_restore_boundary();acceptance_profile_before(&g,r);
 assert(!acceptance_profile_native_scene_ready() && acceptance_profile_effect_record(0,33));
 acceptance_profile_effect_reset();acceptance_profile_native_scene_enable(0);
 acceptance_profile_restore_boundary();assert(!acceptance_profile_effect_record(0,33));
 // Conditional returns must be taken. Matching PC/SP alone is never a return.
 acceptance_profile_effect_span(0,0,0x100);r.PC=0x100;r.SP=0xc100;stack(g,r.SP,0x200);
 acceptance_profile_before(&g,r);r.PC=0x200;r.SP=0xc102;
 acceptance_profile_after(&g,r,0xc0,8);assert(acceptance_profile_effect_active(0));
 acceptance_profile_after(&g,r,0x00,20);assert(acceptance_profile_effect_active(0));
 acceptance_profile_after(&g,r,0xc0,20);assert(!acceptance_profile_effect_active(0));
 // Recursive entries close only the innermost matching frame.
 acceptance_profile_effect_reset();r.PC=0x100;r.SP=0xc100;stack(g,r.SP,0x200);acceptance_profile_before(&g,r);
 r.SP=0xc0fe;stack(g,r.SP,0x150);acceptance_profile_before(&g,r);
 assert(acceptance_profile_effect_active(0)==2);
 r.PC=0x150;r.SP=0xc100;acceptance_profile_after(&g,r,0xd9,16);
 assert(acceptance_profile_effect_active(0)==1);
 acceptance_profile_before(&g,r);r.PC=0x200;r.SP=0xc102;acceptance_profile_after(&g,r,0xc8,20);
 assert(!acceptance_profile_effect_active(0));
 // Rendering captures hardware before VBlank, checks every visible line and
 // rejects both changed state and missing lines. No callback-time substitution.
 acceptance_profile_effect_reset();acceptance_profile_video_enable(1);
 g.cpu.memory[0xff70]=7;g.cpu.ram[0x7100]=32;acceptance_profile_effect_io_write(&g,0xff46,0xd1);
 assert(acceptance_oam_dma_bank==7 && acceptance_oam_dma_source[0]==32);
 r.PC=0x100;r.SP=0xc100;stack(g,r.SP,0x200);acceptance_profile_before(&g,r);
 g.cpu.oam[0]=32;g.cpu.vram[0x2000]=77;g.lcd.palette[0][1]=0x1234;g.lcd.mapped[8][2]=0xabcd;
 for(unsigned line=0;line<144;line++) acceptance_profile_render_line(&g,line);
 assert(acceptance_profile_video_field(0)==1 && acceptance_profile_video_field(5)==144 && acceptance_profile_video_field(6)==0);
 assert(acceptance_profile_video_byte(0)==32 && acceptance_profile_video_byte(160+0x2000)==77);
 assert(acceptance_profile_video_byte(160+0x4000+2)==0x34);
 assert(acceptance_profile_video_byte(160+0x4000+128+8*8+4)==0xcd);
 g.cpu.oam[0]=99;assert(acceptance_profile_video_byte(0)==32);
 acceptance_profile_render_line(&g,0);g.cpu.vram[3]=42;acceptance_profile_render_line(&g,2);
 assert(acceptance_profile_video_field(6)==1);
 assert(acceptance_profile_video_change(0,2)==1);
 assert(acceptance_profile_video_change(0,0)==163 && acceptance_profile_video_change(0,1)==42);
 assert(acceptance_profile_video_row(2,9)==1);
 // Ordered real render calls preserve a hardware mutation rather than dropping
 // the frame. Alias return binds the initial non-OW animation before FF46.
 acceptance_profile_effect_reset();acceptance_profile_effect_span(5,0,0x500);
 acceptance_profile_effect_alias(5,0,0x510);
 r.PC=0x100;r.SP=0xc100;stack(g,r.SP,0x200);acceptance_profile_before(&g,r);
 r.PC=0x510;r.SP=0xc0fe;stack(g,r.SP,0x150);acceptance_profile_before(&g,r);
 r.PC=0x150;r.SP=0xc100;acceptance_profile_after(&g,r,0xc9,16);
 acceptance_profile_effect_io_write(&g,0xff46,0xd1);
 for(unsigned line=0;line<144;line++) {
   if(line==17) {g.cpu.vram[3]=55;g.lcd.mapped[8][2]=0x4321;}
   acceptance_profile_render_line(&g,line,9);
 }
 assert(acceptance_profile_video_field(6)==0 && acceptance_profile_video_field(5)==144);
 assert(acceptance_profile_video_row(0,1)==3 && acceptance_profile_video_row(17,8)==9);
 assert(acceptance_profile_video_row(16,9)==0 && acceptance_profile_video_row(17,9)==3);
 assert(acceptance_profile_video_byte(163)==42 && acceptance_profile_video_change(0,1)==55);
 acceptance_profile_render_line(&g,0); // complete prior frame, valid new frame
 acceptance_profile_render_line(&g,0); // unfinished previous capture is fatal
 assert(acceptance_profile_effect_record(0,33)>0);
 acceptance_profile_effect_reset();
 r.PC=0x100;r.SP=0xc100;stack(g,r.SP,0x200);acceptance_profile_before(&g,r);
 acceptance_profile_render_line(&g,0,1);
 for(unsigned line=1;line<6;line++) {
   std::memset(g.cpu.vram,line,sizeof(g.cpu.vram));acceptance_profile_render_line(&g,line,1);
 }
 assert(acceptance_profile_video_field(6)&8);
 assert(acceptance_profile_effect_record(0,33)>0 && acceptance_profile_video_change(0,2)==65536);
 acceptance_profile_effect_reset();assert(acceptance_profile_video_field(0)==0);
 acceptance_profile_video_enable(0);acceptance_profile_render_line(&g,0);
 assert(acceptance_profile_video_field(0)==0);
 std::puts("PASS immediate RET/IRQ/GDMA chronology, return registers/fence, RETI, conditional RET, recursive/nested calls, ROM0/ROMX, arguments, native readiness, legacy hooks, restores, reset and overflow");
}
