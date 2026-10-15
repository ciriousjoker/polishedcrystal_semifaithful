#!/usr/bin/env python3
"""Compile bounded reachable SM83 ROM instructions into native C, fail closed.

Instructions are decoded at build time only. Calls, returns and RST execute through
actual emulated RAM stack bytes, including the game's bank-wrapper manipulation.
"""
import argparse
import hashlib
import json
import pathlib
import subprocess

ROOT = pathlib.Path(__file__).resolve().parents[2]
MODULE_DIGEST = hashlib.sha256(pathlib.Path(__file__).read_bytes()).hexdigest()
ABI = '''#include <stdint.h>
#include <string.h>
typedef struct {
 uint8_t a,f,b,c,d,e,h,l;
 uint16_t sp,pc,bank;
 uint8_t m[65536];
 uint32_t steps,fault,event_count,event_pos;
 uint8_t events[4096];
} State;
'''
RUNTIME = r'''
#define HL ((uint16_t)((s->h<<8)|s->l))
#define BC ((uint16_t)((s->b<<8)|s->c))
#define DE ((uint16_t)((s->d<<8)|s->e))
#define Z 128
#define N 64
#define H 32
#define C 16
static uint8_t rd(State*s,uint16_t p){
 if(p<0x8000){uint32_t o=p<0x4000?p:s->bank*0x4000u+p-0x4000;if(o>=sizeof(rom)){s->fault=2;return 0;}return rom[o];}
 if((p>=0xc000&&p<0xe000)||(p>=0xff80&&p<0xffff))return s->m[p];
 if(p==0xff04){if(s->event_pos>=s->event_count||s->event_pos>=4096){s->fault=2;return 0;}return s->events[s->event_pos++];}
 s->fault=2;return 0;
}
static void wr(State*s,uint16_t p,uint8_t v){
 if(p>=0x2000&&p<0x4000){s->bank=v&127;if(!s->bank)s->bank=1;return;}
 if((p>=0xc000&&p<0xe000)||(p>=0xff80&&p<0xffff)){s->m[p]=v;return;}
 s->fault=2;
}
static uint16_t pair(State*s,int r){switch(r){case 0:return BC;case 1:return DE;case 2:return HL;default:return s->sp;}}
static void putpair(State*s,int r,uint16_t v){switch(r){case 0:s->b=v>>8;s->c=v;break;case 1:s->d=v>>8;s->e=v;break;case 2:s->h=v>>8;s->l=v;break;default:s->sp=v;}}
static uint8_t reg(State*s,int r){switch(r){case 0:return s->b;case 1:return s->c;case 2:return s->d;case 3:return s->e;case 4:return s->h;case 5:return s->l;case 6:return rd(s,HL);default:return s->a;}}
static void putreg(State*s,int r,uint8_t v){switch(r){case 0:s->b=v;break;case 1:s->c=v;break;case 2:s->d=v;break;case 3:s->e=v;break;case 4:s->h=v;break;case 5:s->l=v;break;case 6:wr(s,HL,v);break;default:s->a=v;}}
static void push(State*s,uint16_t v){wr(s,--s->sp,v>>8);wr(s,--s->sp,v);}
static uint16_t pop(State*s){uint16_t v=rd(s,s->sp++);return v|((uint16_t)rd(s,s->sp++)<<8);}
static void alu(State*s,int op,uint8_t v){unsigned a=s->a,c=(s->f&C)!=0,r=0;switch(op){
 case 0:case 1:c=op==1?c:0;r=a+v+c;s->f=(r>255?C:0)|(((a&15)+(v&15)+c)>15?H:0);s->a=r;break;
 case 2:case 3:case 7:c=op==3?c:0;r=(a-v-c)&255;s->f=N|(a<v+c?C:0)|((a&15)<(v&15)+c?H:0);if(op!=7)s->a=r;break;
 case 4:r=a&v;s->a=r;s->f=H;break;case 5:r=a^v;s->a=r;s->f=0;break;case 6:r=a|v;s->a=r;s->f=0;break;
 }if(!(r&255))s->f|=Z;}
static void incdec(State*s,int r,int dec){unsigned a=reg(s,r),v=(a+(dec?-1:1))&255;putreg(s,r,v);s->f=(s->f&C)|(dec?N:0)|((dec?(a&15)==0:(a&15)==15)?H:0)|(v?0:Z);}
static void rotate(State*s,int kind,int r,int accumulator){unsigned a=reg(s,r),cy=(s->f&C)!=0,v=0,c=0;switch(kind){
 case 0:v=(a<<1)|(a>>7);c=a>>7;break;case 1:v=(a>>1)|(a<<7);c=a&1;break;
 case 2:v=(a<<1)|cy;c=a>>7;break;case 3:v=(a>>1)|(cy<<7);c=a&1;break;
 case 4:v=a<<1;c=a>>7;break;case 5:v=(a>>1)|(a&128);c=a&1;break;
 case 6:v=(a<<4)|(a>>4);break;case 7:v=a>>1;c=a&1;break;}
 putreg(s,r,v);s->f=(c?C:0)|((v&255)||accumulator?0:Z);}
static void addsp(State*s,int8_t v,int hl){uint16_t a=s->sp;unsigned u=(uint8_t)v;s->f=(((a&15)+(u&15))>15?H:0)|(((a&255)+u)>255?C:0);if(hl)putpair(s,2,a+v);else s->sp=a+v;}
'''

def parse_symbols(text):
    out={}
    for line in text.splitlines():
        if not line or line.startswith(';') or ':' not in line.split()[0]:continue
        loc,name=line.split(maxsplit=1)
        bank,pc=loc.split(':');out[name]=(int(bank,16),int(pc,16))
    return out

def symbols(path):
    return parse_symbols(pathlib.Path(path).read_text())

def compile_rom(rom, roots, traps=()):
    pending=list(roots);seen=set();blocks={};unsupported=[]
    def byte(bank,pc):
        off=pc if pc<0x4000 else bank*0x4000+pc-0x4000
        if not 0<=off<len(rom) or pc>=0x8000:raise ValueError(f'Non-ROM instruction {bank:02x}:{pc:04x}')
        return rom[off]
    while pending:
        bank,pc=pending.pop()
        if (bank,pc) in seen:continue
        seen.add((bank,pc))
        key=pc if pc<0x4000 else (bank<<16)|pc
        if (bank,pc) in traps or (0,pc) in traps and pc<0x4000:
            blocks[key]='s->fault=4;';continue
        op=byte(bank,pc);n=1;code=None;targets=[];fall=True
        def imm():return byte(bank,pc+1)
        def word():return imm()|(byte(bank,pc+2)<<8)
        cond=['!(s->f&Z)','s->f&Z','!(s->f&C)','s->f&C']
        if op==0:code=''
        elif op&0xcf==0x01:n=3;code=f'putpair(s,{op>>4},{word()});'
        elif op&0xcf in (0x03,0x0b):code=f'putpair(s,{op>>4},pair(s,{op>>4}){ "+1" if op&15==3 else "-1"});'
        elif op&0xcf==0x09:
            r=op>>4;code=f'{{unsigned a=HL,v=pair(s,{r}),z=a+v;s->f=(s->f&Z)|(z>65535?C:0)|(((a&4095)+(v&4095))>4095?H:0);putpair(s,2,z);}}'
        elif op&0xc7==4:code=f'incdec(s,{(op>>3)&7},0);'
        elif op&0xc7==5:code=f'incdec(s,{(op>>3)&7},1);'
        elif op&0xc7==6:n=2;code=f'putreg(s,{(op>>3)&7},{imm()});'
        elif 0x40<=op<0x80 and op!=0x76:code=f'putreg(s,{(op>>3)&7},reg(s,{op&7}));'
        elif 0x80<=op<0xc0:code=f'alu(s,{(op>>3)&7},reg(s,{op&7}));'
        elif op in (2,0x12,0x22,0x32,0x0a,0x1a,0x2a,0x3a):
            addr='BC' if op<0x10 else 'DE' if op<0x20 else 'HL';code=f'wr(s,{addr},s->a);' if not op&8 else f's->a=rd(s,{addr});'
            if op>=0x20:code+=f'putpair(s,2,HL{ "+1" if op<0x30 else "-1"});'
        elif op in (7,15,23,31):code=f'rotate(s,{(op>>3)&3},7,1);'
        elif op in (0xe0,0xf0):n=2;code=f'wr(s,{0xff00+imm()},s->a);' if op==0xe0 else f's->a=rd(s,{0xff00+imm()});'
        elif op in (0xe2,0xf2):code='wr(s,0xff00+s->c,s->a);' if op==0xe2 else 's->a=rd(s,0xff00+s->c);'
        elif op in (0xea,0xfa):n=3;code=f'wr(s,{word()},s->a);' if op==0xea else f's->a=rd(s,{word()});'
        elif op==8:n=3;code=f'wr(s,{word()},s->sp);wr(s,{(word()+1)&65535},s->sp>>8);'
        elif op&0xc7==0xc6:n=2;code=f'alu(s,{(op>>3)&7},{imm()});'
        elif op in (0xc5,0xd5,0xe5,0xf5):
            r=(op>>4)&3;code='push(s,(s->a<<8)|s->f);' if r==3 else f'push(s,pair(s,{r}));'
        elif op in (0xc1,0xd1,0xe1,0xf1):
            r=(op>>4)&3;code='{uint16_t v=pop(s);s->a=v>>8;s->f=v&240;}' if r==3 else f'putpair(s,{r},pop(s));'
        elif op in (0xe8,0xf8):n=2;code=f'addsp(s,(int8_t){imm()},{int(op==0xf8)});'
        elif op==0xf9:code='s->sp=HL;'
        elif op in (0x18,0x20,0x28,0x30,0x38,0xc3,0xc2,0xca,0xd2,0xda,0xcd,0xc4,0xcc,0xd4,0xdc):
            rel=op<0x40;n=2 if rel else 3;target=(pc+2+(imm() if imm()<128 else imm()-256))&65535 if rel else word()
            call=op in (0xcd,0xc4,0xcc,0xd4,0xdc);uncond=op in (0x18,0xc3,0xcd)
            code=(f'push(s,{pc+n});' if call else '')+f's->pc={target};'
            if not uncond:code=f'if({cond[((op>>3)&3)]}){{{code}}}'
            targets=[(bank,target)];fall=call or not uncond
        elif op in (0xc9,0xc0,0xc8,0xd0,0xd8):
            code='s->pc=pop(s);';fall=op!=0xc9
            if fall:code=f'if({cond[(op>>3)&3]}){{{code}}}'
        elif op==0xe9:code='s->pc=HL;';fall=False
        elif op&0xc7==0xc7:
            target=op&0x38;code=f'push(s,{pc+1});s->pc={target};';targets=[(bank,target)]
            if target==0x10:
                dst=word()&0x7fff;db=byte(bank,pc+3);targets.append((db,dst));n=4
            # Discovery skips farcall pointer data. Runtime pushes pc+1 exactly.
        elif op==0xcb:
            n=2;x=imm();r=x&7;k=(x>>3)&7
            if x<0x40:code=f'rotate(s,{k},{r},0);'
            elif x<0x80:code=f's->f=(s->f&C)|H|((reg(s,{r})&{1<<k})?0:Z);'
            else:code=f'putreg(s,{r},reg(s,{r}){ "&" if x<0xc0 else "|"}{255-(1<<k) if x<0xc0 else 1<<k});'
        elif op==0x2f:code='s->a^=255;s->f|=N|H;'
        elif op==0x37:code='s->f=(s->f&Z)|C;'
        elif op==0x3f:code='s->f=(s->f&Z)|((s->f&C)?0:C);'
        if code is None:
            unsupported.append({'bank':bank,'pc':pc,'opcode':op});continue
        if fall:targets.append((bank,(pc+n)&65535))
        pending.extend(targets)
        key=pc if pc<0x4000 else (bank<<16)|pc
        body=f's->pc={(pc+(1 if op&0xc7==0xc7 else n))&65535};{code}'
        if key in blocks and blocks[key]!=body:raise ValueError('Conflicting ROM0 block')
        blocks[key]=body
    if unsupported:raise ValueError('Unsupported reachable instructions: '+json.dumps(unsupported))
    return blocks,seen

def source_paths():
    """Full tracked tree except trainer-AI tooling, which does not build the game."""
    paths=subprocess.check_output(['git','ls-files','-z'],cwd=ROOT).split(b'\0')
    return sorted(p.decode() for p in paths if p and not p.startswith(b'tools/trainer_ai/'))

def build(rom_path,sym_path,entries,build_dir,source_root=None,boundaries=(),excluded=()):
    build_dir=pathlib.Path(build_dir);build_dir.mkdir(parents=True,exist_ok=True)
    manifest_path=build_dir/'manifest.json';lib=build_dir/'stateful.pending.dylib'
    # Failed regeneration cannot leave an apparently current old binary or manifest.
    for p in (manifest_path,lib):p.unlink(missing_ok=True)
    sha=lambda p:hashlib.sha256(pathlib.Path(p).read_bytes()).hexdigest()
    rom=pathlib.Path(rom_path).read_bytes();rom_digest=hashlib.sha256(rom).hexdigest()
    sym_bytes=pathlib.Path(sym_path).read_bytes();sym_digest=hashlib.sha256(sym_bytes).hexdigest();syms=parse_symbols(sym_bytes.decode())
    source_digests={}
    if sha(__file__)!=MODULE_DIGEST:raise ValueError('Generator changed since module import')
    if len(rom)<0x150 or rom[0x147] not in (0x0f,0x10,0x11,0x12,0x13) or len(rom)>128*0x4000:
        raise ValueError('Unsupported cartridge profile: requires MBC3, at most 128 ROM banks')
    if source_root:
        missing=[p for p in source_paths() if not (pathlib.Path(source_root)/p).is_file()]
        if missing:raise ValueError('Incomplete game source snapshot: '+', '.join(missing[:10]))
        source_digests={p:sha(pathlib.Path(source_root)/p) for p in source_paths()}
    roots=[syms[e] if isinstance(e,str) else tuple(e) for e in entries]
    traps=[syms['Crash']] if 'Crash' in syms else []
    boundary_roots=[syms[e] if isinstance(e,str) else tuple(e) for e in boundaries]
    excluded_roots=[syms[e] if isinstance(e,str) else tuple(e) for e in excluded]
    machine_key=lambda root:root[1] if root[1]<0x4000 else (root[0]<<16)|root[1]
    if {machine_key(r) for r in boundary_roots}&{machine_key(r) for r in traps+excluded_roots}:raise ValueError('Ambiguous native boundary')
    blocks,seen=compile_rom(rom,roots,traps+boundary_roots+excluded_roots)
    # Boundary events stop BEFORE the callee executes. Runtime never resumes or
    # fabricates a callee return; the external profile owns any audited handling.
    for bank,pc in boundary_roots:
        key=pc if pc<0x4000 else (bank<<16)|pc
        if key in blocks:blocks[key]='s->fault=5;'

    c=ABI+'static const uint8_t rom[]={'+','.join(map(str,rom))+'};\n'+RUNTIME
    c+='\nvoid stateful_run(State*s,uint32_t budget,uint16_t stop_pc){uint32_t used=0;while(!s->fault&&s->pc!=stop_pc){if(used++>=budget){s->fault=3;break;}s->steps++;switch(s->pc<0x4000?s->pc:((uint32_t)s->bank<<16)|s->pc){\n'
    c+='\n'.join(f'case {key}:{{{body}}}break;' for key,body in sorted(blocks.items()))
    c+='\ndefault:s->fault=1;break;}}}\n'
    cpath=build_dir/'stateful.c';cpath.write_text(c)
    compiler=subprocess.check_output(['cc','--version'],text=True)
    identity=hashlib.sha256((c+compiler).encode()).hexdigest()
    lib=build_dir/('stateful-'+identity+'.dylib')
    subprocess.run(['cc','-std=c11','-O3','-shared','-fPIC',str(cpath),'-o',str(lib)],check=True)
    sha=lambda p:hashlib.sha256(pathlib.Path(p).read_bytes()).hexdigest()
    manifest={'schema':1,'library_path':str(lib.resolve()),'abi_sha256':hashlib.sha256(ABI.encode()).hexdigest(),'profile':'bounded-mbc3-flat-wram-div-event-tape','rom_sha256':rom_digest,'symbols_sha256':sym_digest,'generator_sha256':MODULE_DIGEST,'generated_c_sha256':sha(cpath),'library_sha256':sha(lib),'entries':dict(zip(map(str,entries),roots)),'explicit_crash_traps':traps,'boundaries':dict(zip(map(str,boundaries),boundary_roots)),'excluded':dict(zip(map(str,excluded),excluded_roots)),'blocks':len(blocks),'reachable_contexts':len(seen),'normalization':'none; real stack, return addresses and bank writes','limits':'WRAM0/WRAMX flat C000-DFFF, HRAM; only DIV MMIO reads; no interrupts, DMA, RTC, external RAM, WRAM banking or arbitrary unknown PC'}
    if source_root:
        paths=source_paths()
        missing=[p for p in paths if not (pathlib.Path(source_root)/p).is_file()]
        if missing:raise ValueError('Incomplete game source snapshot: '+', '.join(missing[:10]))
        if set(paths)!=set(source_digests):raise ValueError('Source input set changed during generation')
        for path,digest in source_digests.items():
            if sha(pathlib.Path(source_root)/path)!=digest:
                raise ValueError('Source changed during generation: '+path)
        manifest['sources']=source_digests
        manifest['source_profile']='tracked-tree-except-tools/trainer_ai/'
    else:manifest['source_profile']='ROM-only-no-source-proof'
    manifest['compiler']=subprocess.check_output(['cc','--version'],text=True).splitlines()[0]
    if sha(__file__)!=MODULE_DIGEST:raise ValueError('Generator changed during generation')
    if sha(rom_path)!=rom_digest or sha(sym_path)!=sym_digest:
        raise ValueError('ROM or symbols changed during generation')
    manifest_path.write_text(json.dumps(manifest,indent=2)+'\n')
    return manifest

def load_verified(build_dir, rom_path, sym_path, source_root=None, entries=None, boundaries=(), excluded=()):
    """Only load an artifact matching present inputs and generator, never stale output."""
    import ctypes
    build_dir=pathlib.Path(build_dir)
    manifest=json.loads((build_dir/'manifest.json').read_text())
    if hashlib.sha256(pathlib.Path(__file__).read_bytes()).hexdigest()!=MODULE_DIGEST:
        raise ValueError('Generator changed since module import')
    sha=lambda p:hashlib.sha256(pathlib.Path(p).read_bytes()).hexdigest()
    checks={'rom_sha256':sha(rom_path),'symbols_sha256':sha(sym_path),
            'generator_sha256':sha(__file__),'abi_sha256':hashlib.sha256(ABI.encode()).hexdigest(),
            'generated_c_sha256':sha(build_dir/'stateful.c'),
            'library_sha256':sha(manifest['library_path']),
            'compiler':subprocess.check_output(['cc','--version'],text=True).splitlines()[0],
            'profile':'bounded-mbc3-flat-wram-div-event-tape'}
    for key,value in checks.items():
        if manifest.get(key)!=value:raise ValueError('Stale native artifact: '+key)
    if entries is not None and set(manifest['entries'])!=set(entries):
        raise ValueError('Native entry configuration mismatch')
    if set(manifest.get('boundaries',{}))!=set(map(str,boundaries)) or set(manifest.get('excluded',{}))!=set(map(str,excluded)):
        raise ValueError('Native boundary configuration mismatch')
    if manifest.get('sources') and source_root is None:
        raise ValueError('Source root required for source-backed artifact')
    if source_root is not None:
        if set(manifest.get('sources',{}))!=set(source_paths()):
            raise ValueError('Native source input path set mismatch')
        if not manifest.get('sources'):raise ValueError('Missing source provenance')
        for path,digest in manifest['sources'].items():
            if sha(pathlib.Path(source_root)/path)!=digest:
                raise ValueError('Stale native source: '+path)
    return ctypes.CDLL(manifest['library_path'])

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('--rom',required=True,type=pathlib.Path);p.add_argument('--symbols',required=True,type=pathlib.Path);p.add_argument('--entry',action='append',required=True);p.add_argument('--build-dir',required=True,type=pathlib.Path);p.add_argument('--source-root',type=pathlib.Path);a=p.parse_args()
    print(json.dumps(build(a.rom,a.symbols,a.entry,a.build_dir,a.source_root),indent=2))
