#!/usr/bin/env python3
"""Read one bounded immutable Head profile and stream unchanged pictures as OUTF.
This is physical component evidence only. It adds no owner/source intent.
"""
import hashlib,json,struct,sys,zlib
from pathlib import Path
p=Path(sys.argv[1]); assert p.stat().st_size <= 64*1024*1024
raw=p.read_bytes(); print(f'{p} sha256={hashlib.sha256(raw).hexdigest()}',file=sys.stderr)
d=json.loads(raw); del raw
frames=d['native_effect_video_frames']; assert 1<=len(frames)<=800
pack=lambda values: struct.pack('<'+'I'*len(values),*values)
for ordinal,f in enumerate(frames,1):
 assert (f['width'],f['height'],f['format'])==(160,144,2)
 assert len(f['hardware'])==16966 and len(f['pixels'])==46080 and len(f['rows'])==144*16
 assert len(f['changes'])%2==0 and len(f['changes'])<=65536*2
 payload=pack([f['host_frame']]+f['display_metadata']+f['render_metadata']+[len(f['changes'])//2])+bytes(f['hardware'])+pack(f['rows'])+pack(f['changes'])+bytes(f['pixels'])
 assert len(payload)<=596710
 sys.stdout.buffer.write(pack([0x4654554f,1,ordinal,len(payload),zlib.crc32(payload)]));sys.stdout.buffer.write(payload)
sys.stdout.buffer.write(pack([0x31444e45,len(frames)]))
