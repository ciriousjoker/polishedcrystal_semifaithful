// Finite startup diagnostic. All authority values are owned, never observer references.
#pragma once
#include <vector>
#include "static_ordinary_intent.hpp"
#include "accepted_ordinary_input.hpp"
namespace native_initializer {
constexpr uint64_t absent = ~uint64_t(0);
enum Provenance : unsigned { UnprovenSetup, Initializer, Ordinary };
struct Sample {
 std::array<unsigned char,444> original{};
 std::array<unsigned char,6> operands{};
 std::array<unsigned char,73> map{};
 std::array<unsigned,13> scene{};
 std::array<unsigned,4> world{};
 std::array<unsigned,27> lighting{};
};
struct Authority {
 accepted_ordinary_input::Latch latch{};
 static_ordinary_intent::Source source{};
 accepted_ordinary_input::Definition definition{};
 uint64_t order=0, lifetime=0, receiver=0, generation=0;
 std::array<unsigned,13> scene{};
};
struct Raster {
 unsigned provenance=UnprovenSetup;
 uint64_t order=0, host=0, ly=0, phase=0, raster=absent, display=absent, callback=0;
 uint64_t source_at_row0=0, input_at_row0=0, receiver=0, generation=0, lifetime=0;
 Sample sample{};
 Authority ordinary{};
 const void* buffer=nullptr;
};
struct Probe {
 bool configured=false, anchored=false, initialized=false;
 unsigned bank=0, pc=0, group=0, number=0, count=0, acknowledged=0, failure=0;
 uint64_t order=0, anchor=0, entry=0, returned=0, activation=0, first_source=0, first_outer=0;
 uint64_t entry_host=0, entry_ly=0, return_host=0, return_ly=0;
 unsigned expected_return_bank=0, expected_return_pc=0;
 unsigned lighting_fields[24][2]{};
 unsigned entry_bank=0, entry_pc=0, entry_sp=0, return_bank=0, return_pc=0, return_sp=0;
 Sample initializer{};
 Authority pending{};
 unsigned limit=64;
 std::vector<Raster> rows=std::vector<Raster>(64);
 bool set_limit(unsigned value) {
  if(configured || value<64 || value>800)return false;
  limit=value; rows.resize(value); return true;
 }
 void fail(unsigned why) { if(!failure) failure=why; }
 void reset() {
  const auto b=bank,p=pc,g=group,n=number,capacity=limit; const bool c=configured;
  unsigned fields[24][2]; std::copy(&lighting_fields[0][0],&lighting_fields[0][0]+48,&fields[0][0]);
  *this=Probe{}; limit=capacity; rows.resize(capacity); configured=c; bank=b;pc=p;group=g;number=n;
  std::copy(&fields[0][0],&fields[0][0]+48,&lighting_fields[0][0]);
 }
 static uint64_t sample_value(const Sample& s,unsigned f) {
  if(f<444) return s.original[f]; f-=444;
  if(f<6) return s.operands[f]; f-=6;
  if(f<73) return s.map[f]; f-=73;
  if(f<13) return s.scene[f]; f-=13;
  if(f<4)return s.world[f];f-=4;
  return f<27?s.lighting[f]:absent;
 }
 static uint64_t source_value(const static_ordinary_intent::Source& s,unsigned f) {
  const uint64_t meta[]={s.id,s.entry,s.returned,s.closed,s.emitted,s.operands_captured,s.map_captured};
  if(f<7)return meta[f];f-=7;
  if(f<444)return s.original[f];f-=444;
  if(f<444)return s.at_return[f];f-=444;
  if(f<444)return s.at_close[f];f-=444;
  if(f<6)return s.operands_entry[f];f-=6;
  if(f<6)return s.operands_return[f];f-=6;
  if(f<6)return s.operands_close[f];f-=6;
  if(f<73)return s.map_entry[f];f-=73;
  if(f<73)return s.map_return[f];f-=73;
  return f<73?s.map_close[f]:absent;
 }
 static uint64_t definition_value(const accepted_ordinary_input::Definition& d,unsigned f) {
  const uint64_t meta[]={d.id,d.lifetime,d.transaction,d.captured,d.committed,d.receiver_id,d.receiver_generation,d.count};
  if(f<8)return meta[f];f-=8;
  if(f>=3*35)return absent;
  const auto& p=d.peers[f/35]; f%=35;
  const uint64_t values[]={p.id,p.generation,p.revision,p.instance,p.epoch,p.x,p.y,p.map_group,p.map_number,p.facing,p.movement,p.status,p.avatar,0,0};
  return f<15?values[f]:p.expanded[f-15];
 }
 uint64_t value(unsigned section,unsigned index,unsigned f) const {
  if(section==0 && index==0) {
   const uint64_t v[]={1,configured,anchored,initialized,count,acknowledged,failure,order,anchor,entry,returned,activation,first_source,first_outer,entry_host,entry_ly,entry_bank,entry_pc,entry_sp,return_host,return_ly,return_bank,return_pc,return_sp,1,expected_return_bank,expected_return_pc};
   return f<27?v[f]:absent;
  }
  if(section==1)return initialized&&index==0?sample_value(initializer,f):absent;
  if(index>=count)return absent;
  const auto& r=rows[index]; const auto& a=r.ordinary; const auto& l=a.latch;
  if(section==2) {
   const uint64_t v[]={r.provenance,r.order,r.host,r.ly,r.phase,r.raster,r.display,r.callback,r.source_at_row0,r.input_at_row0,r.receiver,r.generation,r.lifetime,a.order,l.opportunity,l.instruction,l.source,l.source_closed,l.input,a.lifetime,a.receiver,a.generation};
   return f<22?v[f]:absent;
  }
  if(section==3)return sample_value(r.sample,f);
  if(section==4)return r.provenance==Ordinary?source_value(a.source,f):absent;
  if(section==5)return r.provenance==Ordinary?definition_value(a.definition,f):absent;
  if(section==6)return r.provenance==Ordinary&&f<13?a.scene[f]:absent;
  return absent;
 }
};
}
