"""Focused transport checks using actual writer functions and an independent reader."""
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
import zlib
import struct

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]

class OrdinaryStream(unittest.TestCase):
    def test_absolute_deadline_positive_progress_and_retries(self):
        source = (ROOT / 'mobile_adapter/tools/smoke_libretro.cpp').read_text()
        writer = source[source.index('static void ordinary_write('):source.index('static void ordinary_u32(')]
        # A deterministic monotonic clock isolates policy from host scheduling.
        # The actual production writer and its shared record deadline are used.
        self.assertIn('ordinary_write(header, deadline); ordinary_write(payload, deadline);', source)
        harness = r'''
#include <vector>
#include <chrono>
#include <poll.h>
#include <cerrno>
#include <cassert>
#include <stdexcept>
#include <string>
static int ordinary_fd=1;
static unsigned calls=0, mode=0;
struct TestClock {
 using time_point=std::chrono::steady_clock::time_point;
 static time_point current;
 static time_point now(){return current;}
};
TestClock::time_point TestClock::current{};
static void fail(const std::string& s){throw std::runtime_error(s);}
static long write(int,const unsigned char*,size_t) {
 ++calls;
 unsigned ms=mode==1?1001:mode==2?500:mode==3?100:125;
 TestClock::current+=std::chrono::milliseconds(ms);
 if(mode==2 && calls<=2){errno=calls==1?EINTR:EAGAIN;return -1;}
 if(mode==4){errno=EPIPE;return -1;}
 if(mode==5)return 0;
 return 1;
}
static int test_poll(pollfd*,unsigned,int) {
 TestClock::current+=std::chrono::milliseconds(10);return 0;
}
#define poll test_poll
WRITER
static void reset(unsigned m){mode=m;calls=0;TestClock::current={};}
static bool reject(unsigned n) {
 try {ordinary_write(std::vector<unsigned char>(n));return false;}
 catch(const std::runtime_error& e){return std::string(e.what()).find("Ordinary capture")!=std::string::npos;}
}
int main() {
 reset(0);assert(reject(12));assert(calls==8); // positive one-byte125ms progress
 reset(0);
 const auto deadline=TestClock::now()+std::chrono::seconds(1);
 ordinary_write(std::vector<unsigned char>(4),deadline);
 bool rejected=false;
 try{ordinary_write(std::vector<unsigned char>(8),deadline);}catch(const std::runtime_error&){rejected=true;}
 assert(rejected && calls==8); // header cannot give payload a new second
 reset(1);assert(reject(1));assert(calls==1); // final positive write is too late
 reset(2);assert(reject(1));assert(calls==2); // EINTR/EAGAIN share the budget
 reset(3);ordinary_write(std::vector<unsigned char>(8));assert(calls==8);
 reset(4);assert(reject(1));assert(calls==1);
 reset(5);assert(reject(1));assert(TestClock::now().time_since_epoch()>=std::chrono::seconds(1));
}
'''.replace('WRITER',writer.replace('std::chrono::steady_clock','TestClock'))
        with tempfile.TemporaryDirectory() as directory:
            cpp, binary = Path(directory)/'deadline.cpp', Path(directory)/'deadline'
            cpp.write_text(harness)
            subprocess.run(['c++','-std=c++17','-O0',str(cpp),'-o',str(binary)],check=True)
            subprocess.run([str(binary)],check=True,timeout=5)

    def test_writer_reader_and_failures(self):
        source = (ROOT / 'mobile_adapter/tools/smoke_libretro.cpp').read_text()
        functions = source[source.index('// OUTF v1:'):source.index('static const unsigned char* descriptor')]
        getters = '\n'.join(line for line in source.splitlines() if line.startswith('static unsigned (*ordinary_'))
        harness = r'''
#include <fcntl.h>
#include <unistd.h>
#include <poll.h>
#include <cerrno>
#include <chrono>
#include <cmath>
#include <iostream>
#include <cstdlib>
#include <zlib.h>
#define main head_observer_main
#include "test_effect_observer.cpp"
#undef main
static int ordinary_fd=-1;
static unsigned ordinary_count=0,input_frame=5;
static unsigned pixel_format=2;
static constexpr unsigned RETRO_PIXEL_FORMAT_RGB565=2;
static void fail(const std::string& message) {std::cerr<<message<<'\n';std::exit(1);}
GETTERS
FUNCTIONS
int main(int argc,char** argv) {
 if(argc!=4)return 3;
 ordinary_fd=open(argv[1],O_WRONLY|O_NONBLOCK); if(ordinary_fd<0)return 4;
 std::cout<<"ready"<<std::endl; std::cin.get();
 ordinary_status=acceptance_profile_ordinary_status;
 ordinary_callback=acceptance_profile_ordinary_callback;
 ordinary_effect_video_byte=acceptance_profile_video_byte;
 ordinary_effect_video_field=acceptance_profile_video_field;
 ordinary_effect_display_field=acceptance_profile_display_field;
 ordinary_effect_display_matches=acceptance_profile_display_matches;
 ordinary_effect_video_row=acceptance_profile_video_row;
 ordinary_effect_video_change=acceptance_profile_video_change;
 const unsigned count=std::stoul(argv[2]),mode=std::stoul(argv[3]);
 acceptance_profile_effect_reset();assert(acceptance_profile_ordinary_arm(count));
 gb game;unsigned short pixels[160*144]={};
 for(unsigned frame=0;frame<count;++frame) {
  for(unsigned row=0;row<144;++row) {
   acceptance_profile_render_buffer(pixels,row);
   acceptance_profile_render_line(&game,row,1);
  }
  if(mode==3) {
   acceptance_profile_ordinary_unsupported_output();
   std::memset(pixels,0xff,sizeof(pixels));
  }
  acceptance_profile_display_begin(pixels,160,144,16,320,0,1,1.0f,0,1,0,1,0);
  if(mode==1)acceptance_display_metadata[15]=3;
  if(mode==2)acceptance_video_rows[70][8]=~0u;
  ordinary_capture(pixels,160,144,320);
  ordinary_capture(pixels,160,144,320); // duplicate frontend callback
 }
 return 0;
}
'''.replace('GETTERS', getters).replace('FUNCTIONS', functions)
        with tempfile.TemporaryDirectory() as directory:
            tmp = Path(directory)
            cpp, producer, reader = tmp/'producer.cpp', tmp/'producer', tmp/'reader'
            cpp.write_text(harness)
            subprocess.run(['c++','-std=c++17','-O0','-I',str(HERE),str(cpp),'-lz','-o',str(producer)],check=True)
            subprocess.run(['c++','-std=c++17','-O0',str(HERE/'ordinary_stream_reader.cpp'),'-lz','-o',str(reader)],check=True)
            fifo = tmp/'pipe'; os.mkfifo(fifo)
            def produce(count, mode=0, drain=True):
                fd=os.open(fifo,os.O_RDONLY|os.O_NONBLOCK); os.set_blocking(fd,True)
                process=subprocess.Popen([str(producer),str(fifo),str(count),str(mode)],stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
                self.assertEqual(process.stdout.readline(),b'ready\n')
                consumer=subprocess.Popen([str(reader),str(count)],stdin=fd,stdout=subprocess.PIPE,stderr=subprocess.PIPE) if drain else None
                process.stdin.write(b'\n'); process.stdin.flush()
                result=process.communicate(timeout=20)
                os.close(fd)
                output=consumer.communicate(timeout=5) if consumer else None
                return process.returncode, consumer.returncode if consumer else None, result, output
            rc,rr,_,out=produce(800)
            self.assertEqual((rc,rr),(0,0)); self.assertEqual(len(out[0].splitlines()),800)
            rc,rr,_,out=produce(1,3) # actual causal LCD-off hook before overwrite
            self.assertNotEqual(rc,0); self.assertNotEqual(rr,0)
            self.assertEqual(out[0],b'') # no output record or complete footer
            self.assertNotEqual(produce(1,1)[0],0) # actual unsupported scale metadata
            self.assertNotEqual(produce(1,drain=False)[0],0) # bounded partial-write timeout
            # Independent malformed frame tests, with valid CRC for semantic negatives.
            hardware=160+0x4000+128+128+6+160
            payload=bytearray(160+hardware+144*64+160*144*2)
            def put(at,value):struct.pack_into('<I',payload,at,value)
            for at,value in [(4,1),(8,1),(12,1),(20,144),(96,24),(100,1),(104,1),(120,144),(128,143)]:put(at,value)
            footer=struct.pack('<II',0x31444e45,1)
            def record(ordinal=1):return struct.pack('<IIIII',0x4654554f,1,ordinal,len(payload),zlib.crc32(payload))+payload
            def read(data):return subprocess.run([str(reader),'1'],input=data,capture_output=True).returncode
            self.assertEqual(read(record()+footer),0)
            self.assertNotEqual(read(record()[:-1]),0)
            self.assertNotEqual(read(record()+footer+b'x'),0)
            self.assertNotEqual(read(record(2)+footer),0)
            first=record()
            put(8,2)
            duplicate=struct.pack('<IIIII',0x4654554f,1,2,len(payload),zlib.crc32(payload))+payload
            self.assertNotEqual(subprocess.run([str(reader),'2'],input=first+duplicate+struct.pack('<II',0x31444e45,2),capture_output=True).returncode,0)
            put(8,1)
            put(160+hardware+70*64+36,1);self.assertNotEqual(read(record()+footer),0);put(160+hardware+70*64+36,0)
            put(160+hardware+70*64+32,0xffffffff);self.assertNotEqual(read(record()+footer),0)
            self.assertNotEqual(produce(1,2)[0],0)

if __name__ == '__main__':unittest.main()
