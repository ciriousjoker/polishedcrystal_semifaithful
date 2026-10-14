// Independently consumable bounded OUTF v1 reader. CRC is integrity, not pixel proof.
#include <iostream>
#include <vector>
#include <cstdint>
#include <stdexcept>
#include <zlib.h>
static uint32_t word(const std::vector<unsigned char>& data, size_t at) {
 if(at+4>data.size()) throw std::runtime_error("truncated word");
 return uint32_t(data[at])|(uint32_t(data[at+1])<<8)|(uint32_t(data[at+2])<<16)|(uint32_t(data[at+3])<<24);
}
static std::vector<unsigned char> read(size_t count) {
 std::vector<unsigned char> data(count);
 if(!std::cin.read(reinterpret_cast<char*>(data.data()),count)) throw std::runtime_error("truncated record/footer");
 return data;
}
int main(int argc,char** argv) {
 try {
  const unsigned expected=argc==2?std::stoul(argv[1]):800;
  if(!expected || expected>800) throw std::runtime_error("count overflow");
  unsigned prior_display=0,prior_render=0;
  constexpr size_t hardware=160+0x4000+128+128+6+160, rows=144*16*4, fixed=40*4+hardware+rows+160*144*2;
  for(unsigned index=1;index<=expected;++index) {
   auto h=read(20);
   if(word(h,0)!=0x4654554f || word(h,4)!=1 || word(h,8)!=index || word(h,12)<fixed || word(h,12)>fixed+65536*8)
    throw std::runtime_error("record format/order/size");
   auto p=read(word(h,12));
   if(crc32(0,p.data(),p.size())!=word(h,16)) throw std::runtime_error("digest mismatch");
   unsigned display=word(p,8),render=word(p,12),changes=word(p,156);
   if(changes>65536 || p.size()!=fixed+changes*8 || (!display || !render) ||
      (prior_display && display!=prior_display+1) || (prior_render && render!=prior_render+1) ||
      word(p,4)!=1 || word(p,20)!=144 || word(p,96)!=24 || word(p,100)!=1 ||
      word(p,104)!=render || word(p,120)!=144 || word(p,124)!=0 || word(p,128)!=143)
    throw std::runtime_error("display/raster association");
   unsigned previous=0;
   for(unsigned row=0;row<144;++row) {
    size_t at=160+hardware+row*64;
    unsigned end=word(p,at+36);
    if(end<previous || end>changes || word(p,at+32)>0x7fffffff) throw std::runtime_error("row boundary/windowline");
    previous=end;
   }
   if(previous!=changes) throw std::runtime_error("missing row changes");
   for(unsigned change=0;change<changes;++change) {
    size_t at=160+hardware+rows+change*8;
    if(word(p,at)>=hardware || word(p,at+4)>255) throw std::runtime_error("invalid hardware delta");
   }
   std::cout<<index<<' '<<display<<' '<<render<<' '<<word(h,16)<<'\n';
   prior_display=display; prior_render=render;
  }
  auto footer=read(8);
  if(word(footer,0)!=0x31444e45 || word(footer,4)!=expected || std::cin.peek()!=EOF) throw std::runtime_error("missing/wrong footer or count overflow");
  return 0;
 } catch(const std::exception& error) { std::cerr<<error.what()<<'\n';return 1; }
}
