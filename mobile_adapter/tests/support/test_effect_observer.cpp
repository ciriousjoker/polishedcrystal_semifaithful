#include <algorithm>
#include <vector>
#include <utility>
#include <cassert>
#include <cstdio>
#include <cstring>
union Reg { unsigned short w; struct { unsigned char l,h; } b; Reg():w(0){} };
struct cpu_regs { Reg AF,BC,DE,HL; unsigned short SP=0,PC=0; };
struct CPU { cpu_regs regs; unsigned clock=0; cpu_regs* get_regs(){return &regs;} unsigned get_clock(){return clock;} bool get_speed(){return true;} unsigned char memory[65536]={}, ram[32768]={}; unsigned read(unsigned p) {return memory[p&65535];} unsigned char* get_ram(){return ram;} unsigned char oam[160]={},vram[0x4000]={}; unsigned char* get_oam(){return oam;} unsigned char* get_vram(){return vram;} };
struct ROM { unsigned char data[4*0x4000]={}; unsigned char* get_rom(){return data;} };
struct MBC { unsigned char* mapping; unsigned char* get_rom(){return mapping;} };
struct LCD { bool get_enable(unsigned){return true;} unsigned short palette[16][4]={},mapped[16][4]={}; unsigned short* get_pal(unsigned i){return palette[i];} unsigned short* get_mapped_pal(unsigned i){return mapped[i];} };
struct gb { LCD lcd; LCD* get_lcd(){return &lcd;} CPU cpu; ROM rom; MBC mbc; gb(){mbc.mapping=rom.data;} CPU* get_cpu(){return &cpu;} ROM* get_rom(){return &rom;} MBC* get_mbc(){return &mbc;} };
static unsigned mobile_observed_frames=17,mobile_native_object_address=0xc000,mobile_native_descriptor_address=0;
static unsigned char mobile_native_descriptors[104]={};
#include "profile_native.hpp"
#ifndef ACCEPTANCE_REAL_PALETTE_GETTER
extern "C" void acceptance_profile_palette_current() { acceptance_profile_palette_config(11,2,0.0f); }
#endif
static void stack(gb& g,unsigned sp,unsigned pc) {g.cpu.memory[sp]=pc&255;g.cpu.memory[sp+1]=pc>>8;}
int main(){
 gb g; cpu_regs r; r.SP=0xc100;r.PC=0x100;r.AF.w=0x1234;r.BC.w=0x5678;r.DE.w=0x86d0;r.HL.w=0x8610;

 // Genuine taken dispatch, CB first-byte provenance and bounded stop.
 assert(acceptance_profile_timing_point(0,0x1000,1)==1);
 for(unsigned i=0;i<8;++i)assert(acceptance_profile_timing_field(i,0xff90+i));
 assert(acceptance_profile_timing_config(0xff98,0xff99,0xc800,0xffa0,0,0x200,0x200,0,0));
 acceptance_game_context=&g;assert(acceptance_profile_timing_arm());
 g.cpu.clock=10;
 for(unsigned n=0;n<16;++n){
   r.PC=0x200;r.SP=0xc100;
   auto pre=acceptance_timing_sample(&g,r);
   r.PC=0x40;r.SP=0xc0fe;stack(g,r.SP,0x200);
   acceptance_timing_irq(&g,pre,r,0x200);
   acceptance_timing_before(&g,r);r.PC=0x42;g.cpu.clock+=8;
   acceptance_timing_after(&g,r,0xcb,8); // CB D9 cannot close an IRQ.
   assert(acceptance_profile_timing_status(5)==n);
   acceptance_timing_before(&g,r);r.PC=0x200;r.SP=0xc100;g.cpu.clock+=16;
   acceptance_timing_after(&g,r,0xd9,16);
 }
 assert(acceptance_profile_timing_status(2)&&!acceptance_profile_timing_status(1));
 assert(acceptance_profile_timing_status(5)==16&&acceptance_profile_timing_status(6)==32);
 auto frozen=acceptance_profile_timing_status(6);
 acceptance_timing_before(&g,r);acceptance_timing_after(&g,r,0x00,4);
 assert(acceptance_profile_timing_status(6)==frozen);
 acceptance_profile_restore_boundary();assert(acceptance_profile_timing_status(4)==unsigned(native_vblank_timing::Failure::restored));
 acceptance_profile_effect_reset();
 // Exact HRAM wait RET closes the owner atomically, including disabled tail-JP.
 assert(acceptance_profile_timing_status(0)); // Reset preserves immutable bindings.
 assert(!acceptance_profile_timing_status(3)&&!acceptance_profile_timing_status(4));
 acceptance_game_context=&g;assert(acceptance_profile_timing_arm());
 r.PC=0x200;r.SP=0xc100;auto dma_pre=acceptance_timing_sample(&g,r);
 r.PC=0x40;r.SP=0xc0fe;stack(g,r.SP,0x200);acceptance_timing_irq(&g,dma_pre,r,0x200);
 r.PC=0x1000;r.SP=0xc0fc;stack(g,r.SP,0x200);
 acceptance_timing_before(&g,r);r.PC=0xffa0;g.cpu.clock+=16;acceptance_timing_after(&g,r,0xc3,16);
 const unsigned char dma_bytes[]={0xe2,0x05,0x20,0xfd,0xc9};
 for(unsigned i=0;i<5;++i)g.cpu.memory[0xffa0+i]=dma_bytes[i];
 acceptance_timing_before(&g,r);g.cpu.regs=r;
 acceptance_profile_effect_io_write(&g,0xff46,0xc0);
 r.PC=0xffa1;g.cpu.clock+=8;acceptance_timing_after(&g,r,0xe2,8);
 r.PC=0xffa4;acceptance_timing_before(&g,r);
 r.PC=0x200;r.SP=0xc0fe;g.cpu.clock+=16;acceptance_timing_after(&g,r,0xc9,16);
 assert(!acceptance_profile_timing_status(3)&&!acceptance_profile_timing_status(4));
 assert(acceptance_timing.event(3)->kind==native_vblank_timing::Kind::dma_wait_complete);
 assert(acceptance_timing.event(4)->kind==native_vblank_timing::Kind::job_return);
 acceptance_timing_before(&g,r);r.SP=0xc100;g.cpu.clock+=16;acceptance_timing_after(&g,r,0xd9,16);
 assert(acceptance_profile_timing_status(5)==1);
 acceptance_profile_effect_reset();
 r.PC=0x100;r.SP=0xc100;
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
 // Startup maps DMG initialization before cartridge detection. Only observed
 // pre-render transitions are allowed; every selected raw/mapped color is
 // independently checked by the oracle after render starts.
 acceptance_profile_palette_config(8,2,0.0f);
 acceptance_profile_palette_config(11,2,0.0f);
 assert(acceptance_palette_config[4]==0);
 for(unsigned line=0;line<144;line++) acceptance_profile_render_line(&g,line);
 assert(acceptance_profile_video_field(0)==1 && acceptance_profile_video_field(5)==144 && acceptance_profile_video_field(6)==0);
 assert(acceptance_profile_video_byte(0)==32 && acceptance_profile_video_byte(160+0x2000)==77);
 assert(acceptance_profile_video_byte(160+0x4000+2)==0x34);
 assert(acceptance_profile_video_byte(160+0x4000+128+8*8+4)==0xcd);

 // Actual invocation snapshots pre-toggle parity and binds the exact buffer.
 unsigned short output[160*144]={};
 acceptance_profile_palette_config(11,2,0.0f);
 const unsigned render_id=acceptance_profile_video_field(1);
 acceptance_profile_display_begin(output,160,144,16,320,1,1,1.02f,0,1,0,1,0);
 assert(acceptance_profile_display_field(0)==1 && acceptance_profile_display_field(23)==24);
 assert(acceptance_profile_display_field(2)==render_id && acceptance_profile_display_field(4)==144);
 assert(acceptance_profile_display_field(11)==0x3f828f5c && acceptance_profile_display_field(12)==0);
 assert(acceptance_profile_display_matches(output) && !acceptance_profile_display_matches(output+1));
 ++mobile_observed_frames; // Callback follows VBlank, preserving prior hardware ID.
 assert(acceptance_profile_display_field(3)==17 && acceptance_profile_display_field(12)==0);
 acceptance_profile_render_line(&g,0);
 assert(acceptance_profile_display_field(2)==render_id); // Next render cannot rewrite snapshot.
 acceptance_profile_display_begin(output,160,144,16,320,1,1,1.02f,1,1,0,1,0);
 assert(acceptance_profile_display_field(1)==2 && acceptance_profile_display_field(4)==1);
 assert(acceptance_profile_video_field(5)==1); // Invocation supplies no synthetic row credit.
 acceptance_profile_palette_config(11,1,0.0f);
 acceptance_profile_display_begin(output,160,144,16,320,1,1,1.02f,1,1,0,1,0);
 assert(acceptance_profile_display_field(21)==1); // Changed cache config is rejected.
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
 r.PC=0x160;acceptance_profile_before(&g,r);
 acceptance_profile_effect_io_write(&g,0xff46,0xd1);
 for(unsigned line=0;line<144;line++) {
   if(line==17) {g.cpu.vram[3]=55;g.lcd.mapped[8][2]=0x4321;}
   acceptance_profile_render_line(&g,line,9);
 }
 assert(acceptance_profile_video_field(6)==0 && acceptance_profile_video_field(5)==144);
 assert(acceptance_profile_video_row(0,1)==3 && acceptance_profile_video_row(17,8)==9);
 assert(acceptance_profile_video_row(16,9)==0 && acceptance_profile_video_row(17,9)==3);
 assert(acceptance_profile_video_byte(163)==42 && acceptance_profile_video_change(0,1)==55);
 // Ninth clear span captures the complete source. Later returns cannot rebind old DMA.
 acceptance_profile_effect_span(8,0,0x600);
 acceptance_profile_effect_teardown(0,0x180);
 acceptance_profile_effect_watch(0,0,0xc100,160);
 acceptance_profile_effect_watch(4,0,0xc200,160);
 r.PC=0x600;r.SP=0xc0fe;stack(g,r.SP,0x180);acceptance_profile_before(&g,r);
 g.cpu.memory[0xc190]=32;g.cpu.memory[0xc191]=24;g.cpu.memory[0xc192]=1;
 std::memset(g.cpu.memory+0xc200,0,160);
 r.PC=0x180;r.SP=0xc100;acceptance_profile_after(&g,r,0xc9,16);
 const unsigned teardown_index=acceptance_effect_records.size();
 assert(acceptance_profile_effect_returns(8)==1);
 assert(acceptance_profile_effect_record(teardown_index-1,16)==1);
 assert(acceptance_profile_effect_sample(teardown_index-1,0,144)==32);
 acceptance_profile_render_line(&g,0);
 assert(acceptance_profile_video_row(0,1)==3);
 r.PC=0x190;acceptance_profile_before(&g,r);
 acceptance_profile_effect_io_write(&g,0xff46,0xc1);
 acceptance_profile_render_line(&g,1);
 assert(acceptance_profile_video_row(1,1)==teardown_index);
 for(unsigned line=2;line<144;line++)acceptance_profile_render_line(&g,line);
 const auto overflow=acceptance_effect_overflow;
 acceptance_profile_effect_span(9,0,0x600);
 assert(acceptance_effect_overflow==overflow+1);
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
 assert(acceptance_profile_display_field(0)==0 && !acceptance_profile_display_matches(output));
 acceptance_profile_video_enable(0);acceptance_profile_render_line(&g,0);
 assert(acceptance_profile_video_field(0)==0);
 // Unrelated Clear does not publish ordinary OAM before an effect. Later
 // normal native writes are captured as DMA bytes with source_index0.
 acceptance_profile_effect_reset();acceptance_profile_video_enable(1);
 acceptance_profile_effect_span(0,0,0);acceptance_profile_effect_span(5,0,0x500);
 acceptance_profile_effect_span(8,0,0x600);
 r.PC=0x600;r.SP=0xc100;stack(g,r.SP,0x700);acceptance_profile_before(&g,r);
 r.PC=0x700;r.SP=0xc102;acceptance_profile_after(&g,r,0xc9,16);
 assert(acceptance_profile_effect_record(1,16)==0);
 g.cpu.memory[0xc190]=33;r.PC=0x710;acceptance_profile_before(&g,r);
 acceptance_profile_effect_io_write(&g,0xff46,0xc1);
 assert(acceptance_oam_dma_producer==0 && acceptance_oam_dma_source[144]==33);
 // Once an animation source exists, unrelated Clear retains that source,
 // even when it clears animation state. The Rust gate checks source equality.
 r.PC=0x500;r.SP=0xc100;stack(g,r.SP,0x720);acceptance_profile_before(&g,r);
 r.PC=0x720;r.SP=0xc102;acceptance_profile_after(&g,r,0xc9,16);
 const auto live_index=acceptance_effect_records.size();
 r.PC=0x600;r.SP=0xc100;stack(g,r.SP,0x730);acceptance_profile_before(&g,r);
 std::memset(g.cpu.memory+0xc200,0,160);
 r.PC=0x730;r.SP=0xc102;acceptance_profile_after(&g,r,0xc9,16);
 r.PC=0x740;acceptance_profile_before(&g,r);
 acceptance_profile_effect_io_write(&g,0xff46,0xc1);
 assert(acceptance_oam_dma_producer==live_index);
 // Static EMPTY diagnostics require genuine unwindowed returns with live provenance.
 acceptance_profile_effect_reset();acceptance_profile_native_scene_enable(0);
 acceptance_profile_simulation(2,0x4400,1,0xc000,34,1,10);
 for(unsigned j=0;j<4;++j)acceptance_profile_static_world_field(j,0,0xc300+j);
 g.cpu.memory[0xc300]=7;g.cpu.memory[0xc301]=9;
 g.cpu.memory[0xc302]=2;g.cpu.memory[0xc303]=0;
 auto static_entry=[&](){g.mbc.mapping=g.rom.data+0x4000;r.PC=0x4400;r.SP=0xc100;
   stack(g,r.SP,0x4700);acceptance_profile_before(&g,r);};
 auto static_ret=[&](unsigned opcode=0xc9,unsigned cycles=16){
   r.PC=0x4410;r.SP=0xc100;acceptance_profile_before(&g,r);
   r.PC=0x4700;r.SP=0xc102;acceptance_profile_after(&g,r,opcode,cycles);};
 acceptance_profile_before(&g,r);assert(!acceptance_profile_static_status(6));
 static_entry();static_ret(0,16);assert(!acceptance_profile_static_status(6));
 // Exact pre-SP, post-SP, return PC, bank and taken-return opcode are required.
 for(unsigned bad=0;bad<5;++bad){
   acceptance_profile_effect_reset();static_entry();
   r.PC=0x4410;r.SP=bad==0?0xc0fe:0xc100;acceptance_profile_before(&g,r);
   r.PC=bad==1?0x4701:0x4700;r.SP=bad==2?0xc104:0xc102;
   if(bad==3)g.mbc.mapping=g.rom.data+0x8000;
   acceptance_profile_after(&g,r,bad==4?0xc0:0xc9,bad==4?8:16);
   assert(!acceptance_profile_static_status(6)&&!acceptance_profile_static_status(7));
 }
 acceptance_profile_effect_reset();static_entry();static_ret(0xc0,20);
 assert(acceptance_profile_static_status(6)&&acceptance_profile_static_status(7)==1);
 assert(!acceptance_profile_native_scene_ready()); // EMPTY never changes Head readiness.
 for(unsigned changed=0;changed<4;++changed){
   acceptance_profile_effect_reset();static_entry();static_ret();
   const unsigned previous=g.cpu.memory[0xc300+changed];
   g.cpu.memory[0xc300+changed]=previous+1;acceptance_profile_before(&g,r);
   assert(!acceptance_profile_static_status(6));
   g.cpu.memory[0xc300+changed]=previous;acceptance_profile_before(&g,r);
   assert(!acceptance_profile_static_status(6)); // Repairing RAM cannot relatch.
   static_entry();static_ret();assert(acceptance_profile_static_status(6));
 }
 // A map transition inside the call cannot seed a completed return.
 acceptance_profile_effect_reset();static_entry();g.cpu.memory[0xc301]++;
 static_ret();assert(!acceptance_profile_static_status(6));g.cpu.memory[0xc301]--;
 acceptance_profile_before(&g,r);assert(!acceptance_profile_static_status(6));
 static_entry();static_ret(0xd9,16);assert(acceptance_profile_static_status(6));
 // Enabling is tied to completion; an invalidated live scene freezes first failure.
 acceptance_profile_static_inputs(1,0xd000,0xd200,0xd201);
 for(unsigned j=0;j<4;++j)acceptance_profile_static_point(j,2,0x5000+j);
 acceptance_profile_effect_reset();assert(!acceptance_profile_static_enable());
 static_entry();static_ret();assert(acceptance_profile_static_enable());
 g.cpu.memory[0xc303]=1;acceptance_profile_before(&g,r);
 assert(acceptance_profile_static_status(1)==11&&!acceptance_profile_static_status(6));
 g.cpu.memory[0xc303]=0;static_entry();static_ret();
 assert(acceptance_profile_static_status(1)==11);
 acceptance_profile_restore_boundary();assert(acceptance_profile_static_status(1)==11);
 acceptance_profile_effect_reset();static_entry();static_ret();
 // Diagnostics are pure even for an invalid token or unsupported status index.
 const auto before_status=acceptance_profile_static_status(1);
 assert(acceptance_profile_static_source(999,0,0)==~uint64_t(0));
 assert(acceptance_profile_static_status(999)==~uint64_t(0));
 assert(acceptance_profile_static_status(1)==before_status);
 acceptance_profile_restore_boundary();assert(!acceptance_profile_static_status(6));
 assert(acceptance_profile_static_status(1)==12);
 acceptance_static_intent.fail(99);assert(acceptance_profile_static_status(1)==12);
 acceptance_profile_effect_reset();assert(!acceptance_profile_static_status(6)&&!acceptance_profile_static_status(7));
 assert(!acceptance_profile_static_status(1));
 std::puts("PASS immediate RET/IRQ/GDMA chronology, return registers/fence, RETI, conditional RET, recursive/nested calls, ROM0/ROMX, arguments, native readiness, legacy hooks, restores, reset, overflow and actual display binding");
 return 0;
}
