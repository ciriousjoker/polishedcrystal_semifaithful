// Functional observer tests; no native source intent or physical-pixel acceptance.
#define main original_head_observer_main
#include "test_effect_observer.cpp"
#undef main
static unsigned short ordinary_pixels[160*144];
static void fresh(unsigned count=2) {
 acceptance_profile_effect_reset();
 assert(acceptance_profile_ordinary_arm(count));
 assert(!acceptance_profile_effect_active(0));
}
static void rows(gb& game, unsigned count=144) {
 for(unsigned row=0;row<count;++row) {
  acceptance_profile_render_buffer(ordinary_pixels,row);
  acceptance_profile_render_line(&game,row,1);
 }
}
static void display() {
 acceptance_profile_palette_config(11,2,0.0f);
 acceptance_profile_display_begin(ordinary_pixels,160,144,16,320,0,1,1.0f,0,1,0,1,0);
}
int main() {
 gb game;
 assert(!acceptance_profile_ordinary_arm(0)); assert(!acceptance_profile_ordinary_arm(801));
 fresh(); rows(game); display(); assert(acceptance_profile_ordinary_callback(ordinary_pixels)==1);
 assert(acceptance_profile_ordinary_callback(ordinary_pixels)==0);
 assert(acceptance_profile_ordinary_status(3)==1);
 // Extra renderer invocation without a new LCD raster cannot earn cadence.
 display(); assert(acceptance_profile_ordinary_status(1)==3);
 fresh(); rows(game); display(); assert(acceptance_profile_ordinary_callback(ordinary_pixels));
 rows(game); display(); assert(acceptance_profile_ordinary_callback(ordinary_pixels));
 assert(acceptance_profile_ordinary_status(1)==2 && acceptance_profile_ordinary_status(3)==2);
 fresh(); rows(game,143); display(); assert(acceptance_profile_ordinary_status(1)==3);
 fresh(); rows(game,1); acceptance_profile_render_buffer(ordinary_pixels,0); assert(acceptance_profile_ordinary_status(1)==3);
 fresh(); rows(game,1); acceptance_profile_render_buffer(ordinary_pixels,2); assert(acceptance_profile_ordinary_status(1)==3);
 fresh(); rows(game); display(); display(); assert(acceptance_profile_ordinary_status(1)==3);
 fresh(); acceptance_ordinary_requested=false; acceptance_ordinary_state=1; assert(!acceptance_profile_ordinary_callback(ordinary_pixels)); assert(acceptance_profile_ordinary_status(1)==3);
 fresh(); rows(game); display(); assert(!acceptance_profile_ordinary_callback(ordinary_pixels+1)); assert(acceptance_profile_ordinary_status(1)==3);
 fresh(); rows(game,1); acceptance_profile_render_buffer(ordinary_pixels+1,1); assert(acceptance_profile_ordinary_status(1)==3);
 fresh(); rows(game); acceptance_profile_render_buffer(ordinary_pixels,0); assert(acceptance_profile_ordinary_status(1)==3);
 fresh(); display(); assert(acceptance_profile_ordinary_status(1)==0); // Prior retained output has no capture credit.
 fresh(); rows(game); acceptance_profile_display_begin(ordinary_pixels+1,160,144,16,320,0,1,1.0f,0,1,0,1,0); assert(acceptance_profile_ordinary_status(1)==3);
 fresh(); rows(game,70); acceptance_profile_ordinary_cancel(); assert(acceptance_profile_ordinary_status(1)==4);
 // Exact unsupported native chronology: a complete unconsumed raster, then
 // the injected hook, native LCD-off clear, same-buffer display and callback.
 fresh(); rows(game); acceptance_profile_ordinary_unsupported_output();
 std::memset(ordinary_pixels,0xff,sizeof(ordinary_pixels)); display();
 assert(!acceptance_profile_ordinary_callback(ordinary_pixels));
 assert(acceptance_profile_ordinary_status(1)==3 && !acceptance_profile_ordinary_status(3));
 // Supported interlace/brightness can change pixels after display entry.
 fresh(1); rows(game);
 acceptance_profile_palette_config(11,2,0.0f);
 acceptance_profile_display_begin(ordinary_pixels,160,144,16,320,1,1,1.5f,1,1,0,1,0);
 std::memset(ordinary_pixels,0x77,sizeof(ordinary_pixels));
 assert(acceptance_profile_ordinary_callback(ordinary_pixels));
 assert(acceptance_profile_ordinary_status(1)==2);
 fresh(800);
 for(unsigned i=0;i<800;++i) { rows(game); display(); assert(acceptance_profile_ordinary_callback(ordinary_pixels)); }
 assert(acceptance_profile_ordinary_status(3)==800 && acceptance_profile_ordinary_status(1)==2);
 assert(!acceptance_profile_ordinary_callback(ordinary_pixels));
 fresh(); rows(game); display(); assert(acceptance_profile_ordinary_callback(ordinary_pixels));
 rows(game); display(); assert(acceptance_profile_ordinary_callback(ordinary_pixels));
 fresh(); acceptance_profile_render_buffer(ordinary_pixels,0); acceptance_profile_render_line(&game,0,-1); assert(acceptance_profile_ordinary_status(1)==3);
 for(unsigned request_ly : {153u,0u}) {
  acceptance_profile_effect_reset(); acceptance_game_context=&game;
  game.cpu.memory[0xff44]=request_ly;
  assert(acceptance_profile_ordinary_arm(1));
  assert(acceptance_profile_ordinary_status(1)==0 && acceptance_profile_ordinary_status(5));
  assert(!acceptance_profile_ordinary_arm(1));
  display(); assert(!acceptance_profile_ordinary_callback(ordinary_pixels));
  if(request_ly==0) for(unsigned row=1;row<144;++row) {
   acceptance_profile_render_buffer(ordinary_pixels,row); acceptance_profile_render_line(&game,row,1);
  }
  game.cpu.memory[0xff44]=0;
  display(); assert(!acceptance_profile_ordinary_callback(ordinary_pixels));
  rows(game); assert(acceptance_profile_ordinary_status(1)==1);
  assert(!acceptance_profile_ordinary_status(5));
  assert(acceptance_profile_ordinary_status(8)==request_ly && acceptance_profile_ordinary_status(9)==0);
  display(); assert(acceptance_profile_ordinary_callback(ordinary_pixels));
  assert(acceptance_profile_ordinary_status(1)==2 && acceptance_profile_ordinary_status(3)==1);
 }
 for(unsigned reason=1;reason<=20;++reason) {
  fresh(); rows(game);
  switch(reason) {
   case 1: acceptance_profile_ordinary_unsupported_output(); break;
   case 2: acceptance_profile_render_buffer(nullptr,1); break;
   case 3: display(); acceptance_profile_render_buffer(ordinary_pixels,0); break;
   case 4: acceptance_profile_render_buffer(ordinary_pixels,144); break;
   case 5: fresh(); rows(game,1); acceptance_profile_render_buffer(ordinary_pixels,0); break;
   case 6: acceptance_profile_render_buffer(ordinary_pixels,0); break;
   case 7: acceptance_ordinary_rows_started=false; acceptance_profile_render_buffer(ordinary_pixels,1); break;
   case 8: acceptance_profile_render_buffer(ordinary_pixels,2); break;
   case 9: acceptance_video_meta[5]=1; acceptance_profile_render_buffer(ordinary_pixels+1,1); break;
   case 10: display(); display(); break;
   case 11: acceptance_ordinary_rows_started=false; display(); break;
   case 12: acceptance_profile_display_begin(ordinary_pixels+1,160,144,16,320,0,1,1.0f,0,1,0,1,0); break;
   case 13: acceptance_video_meta[0]=0; display(); break;
   case 14: display(); assert(acceptance_profile_ordinary_callback(ordinary_pixels)); display(); break;
   case 15: acceptance_video_meta[5]=143; display(); break;
   case 16: acceptance_video_meta[6]=1; display(); break;
   case 17: assert(!acceptance_profile_ordinary_callback(ordinary_pixels)); break;
   case 18: display(); assert(!acceptance_profile_ordinary_callback(ordinary_pixels+1)); break;
   case 19: acceptance_profile_render_line(&game,143,-1); break;
   case 20: acceptance_profile_restore_boundary(); break;
  }
  assert(acceptance_profile_ordinary_status(1)==3);
  assert(acceptance_profile_ordinary_failure(1)==reason);
  unsigned snapshot[27]; for(unsigned i=0;i<27;++i)snapshot[i]=acceptance_profile_ordinary_failure(i);
  acceptance_profile_ordinary_unsupported_output(); display();
  acceptance_profile_ordinary_callback(ordinary_pixels); acceptance_profile_restore_boundary();
  for(unsigned i=0;i<27;++i)assert(snapshot[i]==acceptance_profile_ordinary_failure(i));
 }
 fresh(); acceptance_ordinary_requested=false; acceptance_ordinary_state=1;
 acceptance_profile_render_buffer(ordinary_pixels,1); assert(acceptance_profile_ordinary_failure(1)==7);
 fresh(); acceptance_profile_restore_boundary();
 assert(acceptance_profile_ordinary_failure(1)==20 && acceptance_profile_ordinary_failure(5)==0);
 assert(acceptance_profile_ordinary_status(1)==3 && !acceptance_profile_ordinary_status(5));
 puts("ordinary observer focused PASS");
}
