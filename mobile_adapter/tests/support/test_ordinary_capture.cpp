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
 fresh(); assert(!acceptance_profile_ordinary_callback(ordinary_pixels)); assert(acceptance_profile_ordinary_status(1)==3);
 fresh(); rows(game); display(); assert(!acceptance_profile_ordinary_callback(ordinary_pixels+1)); assert(acceptance_profile_ordinary_status(1)==3);
 fresh(); rows(game,1); acceptance_profile_render_buffer(ordinary_pixels+1,1); assert(acceptance_profile_ordinary_status(1)==3);
 fresh(); rows(game); acceptance_profile_render_buffer(ordinary_pixels,0); assert(acceptance_profile_ordinary_status(1)==3);
 fresh(); display(); assert(acceptance_profile_ordinary_status(1)==3); // Unsupported leading retained output.
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
 puts("ordinary observer focused PASS");
}
