"""Focused bounded protocol/physical decoder tests, no emulator or game build."""
import copy,struct,subprocess,unittest,zlib,os,tempfile
from pathlib import Path
HERE=Path(__file__).parent
class Pictures(unittest.TestCase):
 @classmethod
 def setUpClass(cls):
  if 'ORDINARY_PICTURE_READER' in os.environ:
   cls.binary=Path(os.environ['ORDINARY_PICTURE_READER'])
  else:
   cls.temp=tempfile.TemporaryDirectory(prefix='ordinary-picture-')
   cls.binary=Path(cls.temp.name)/'ordinary_picture_reader'
   subprocess.run(['rustc','--edition=2021',str(HERE/'ordinary_picture_reader.rs'),'-o',str(cls.binary)],check=True,capture_output=True)
 @classmethod
 def tearDownClass(cls):
  if hasattr(cls,'temp'):cls.temp.cleanup()
 def base(self):
  d=[1,1,1,1,144,160,144,16,320,0,1,1065520988,0,1,0,1,0,1,11,1,0,0,0,24]
  m=[1,1,1,1,0,144,0,143,1,1,1,1,0,7]
  h=bytearray(16966);h[16800]=0x93;h[16805]=0xc1
  for p in range(16):
   h[16544+p*8+2:16544+p*8+4]=struct.pack('<H',32767)
   h[16672+p*8+2:16672+p*8+4]=struct.pack('<H',65503)
  rows=sum(([1,0,1,1,1,1,0,7,9,0,1,1,0,0,0,0] for _ in range(144)),[])
  return [d,m,h,rows,[],bytearray(46080)]
 def stream(self,f):
  d,m,h,rows,changes,pixels=f
  pack=lambda a:struct.pack('<'+'I'*len(a),*a)
  p=pack([1]+d+m+[len(changes)//2])+h+pack(rows)+pack(changes)+pixels
  return pack([0x4654554f,1,1,len(p),zlib.crc32(p)])+p+pack([0x31444e45,1])
 def run_bytes(self,b,count=1):return subprocess.run([str(self.binary),str(count)],input=b,stdout=subprocess.PIPE,stderr=subprocess.PIPE).returncode
 def test_valid_and_self_consistent_different_intent(self):
  f=self.base();self.assertEqual(self.run_bytes(self.stream(f)),0)
  # Physical valid full white BG is different intent, explicitly outside this gate.
  for y in range(8):f[2][160+y*2]=255
  f[5][:]=struct.pack('<H',65503)*23040
  self.assertEqual(self.run_bytes(self.stream(f)),0)
 def test_one_pixel_and_selected_tile_raw_mapped_corruption(self):
  mutations=[lambda f:f[5].__setitem__(0,1),lambda f:f[2].__setitem__(160,128),lambda f:f[2].__setitem__(16544,31),lambda f:f[2].__setitem__(16672,1)]
  for mutate in mutations:
   f=self.base();mutate(f);self.assertNotEqual(self.run_bytes(self.stream(f)),0)
 def test_sparse_and_row_dma_strict(self):
  for changes in ([160,1,160,2],[161,1,160,2],[160,0],[16966,1]):
   f=self.base();f[4]=changes
   for y in range(144):f[3][y*16+9]=len(changes)//2
   self.assertNotEqual(self.run_bytes(self.stream(f)),0)
  for index,value in [(4,0),(6,1),(7,3),(8,0xffffffff),(9,1)]:
   f=self.base();f[3][index]=value;self.assertNotEqual(self.run_bytes(self.stream(f)),0)
  f=self.base();f[2][0]=1;self.assertNotEqual(self.run_bytes(self.stream(f)),0)
 def test_framing_crc_truncation_footer_bounds(self):
  b=self.stream(self.base())
  for cut in (0,4,19,20,len(b)-9,len(b)-1):self.assertNotEqual(self.run_bytes(b[:cut]),0)
  for at in (0,4,8,12,16,len(b)-8,len(b)-4):
   x=bytearray(b);x[at]^=1;self.assertNotEqual(self.run_bytes(x),0)
  self.assertNotEqual(self.run_bytes(b+b'x'),0)
  self.assertNotEqual(self.run_bytes(b[:-8]+b),0)
 def test_multiple_output_progression_and_limits(self):
  first=self.stream(self.base())[:-8]
  second=self.base(); second[0][1]=2;second[0][2]=2;second[1][1]=2
  b=bytearray(self.stream(second)[:-8]);struct.pack_into('<I',b,8,2)
  footer=struct.pack('<II',0x31444e45,2)
  self.assertEqual(self.run_bytes(first+b+footer,2),0)
  for array,index in [(0,1),(0,2),(1,1)]:
   bad=copy.deepcopy(second);bad[array][index]=1
   data=bytearray(self.stream(bad)[:-8]);struct.pack_into('<I',data,8,2)
   self.assertNotEqual(self.run_bytes(first+data+footer,2),0)
  self.assertNotEqual(self.run_bytes(first,0),0);self.assertNotEqual(self.run_bytes(first,801),0)
 def test_display_and_render_strict(self):
  for array,index,value in [(0,2,2),(0,3,2),(0,13,2),(0,19,0),(1,5,143),(1,6,1),(1,7,142)]:
   f=self.base();f[array][index]=value;self.assertNotEqual(self.run_bytes(self.stream(f)),0)
if __name__=='__main__':unittest.main()
