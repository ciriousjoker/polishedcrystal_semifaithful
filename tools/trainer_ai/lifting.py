#!/usr/bin/env python3
"""Bounded source-driven SM83 arithmetic lifter, deliberately fail closed."""
import argparse
import hashlib
import json
import pathlib
import re
import subprocess

ROOT = pathlib.Path(__file__).resolve().parents[2]
SOURCES = ['engine/battle/effect_commands.asm', 'engine/math/math.asm', 'home/header.asm', 'ram/hram.asm', 'macros/rst.asm', 'home/farcall.asm']

def extract(root):
    battle = (root / SOURCES[0]).read_text()
    math = (root / SOURCES[1]).read_text()
    header = (root / SOURCES[2]).read_text()
    hram = (root / SOURCES[3]).read_text()
    return (battle[battle.index('DamagePass1:'):battle.index('BattleCommand_constantdamage:')]
            + math[:math.index('Divide16::')]
            + header[header.index('PopBCDEHL::'):header.index('ContChar:')],
            hram[hram.index('UNION', hram.index('; math-related values')):hram.index('UNION', hram.index('hMathBuffer::') + 1)])

def lines(source):
    scope = ''
    for raw in source.splitlines():
        s = raw.split(';')[0].strip()
        if not s: continue
        if re.fullmatch(r'[\w.]+:?', s) and (s.endswith(':') or s.startswith('.')):
            label = s.rstrip(':')
            if not label.startswith('.'): scope = label
            yield ('label', scope + label if label.startswith('.') else label)
        elif re.fullmatch(r'\w+::', s):
            scope = s[:-2]; yield ('label', scope)
        else:
            s = re.sub(r'(?<!\w)\.([A-Za-z_]\w*)', lambda m: scope + '.' + m[1], s)
            s = re.sub(r'^farcall\s+', 'call ', s)
            s = re.sub(r'^farjp\s+|^jmp\s+', 'jp ', s)
            yield ('ins', s)

def ident(s): return s.replace('.', '_')

def generate(source, symbols):
    items = list(lines(source))
    # Native C functions mirror global assembly entry points; local labels stay gotos.
    globals_ = [x for k,x in items if k == 'label' and '.' not in x]
    globals_ += [x.split(' ',1)[1] for k,x in items if k == 'ins' and x.startswith('call ') and '.' in x]
    def value(s):
        s = s.strip()
        s = re.sub(r'\$([0-9a-fA-F]+)', r'0x\1', s)
        s = re.sub(r'LOW\(([^)]+)\)', r'((\1)&255)', s)
        for name in sorted(symbols, key=len, reverse=True):
            s = re.sub(r'\b' + name + r'\b', str(symbols[name]), s)
        if re.search(r'[^\d\sxa-fA-F()+&\-]', s): raise ValueError('Unsupported value: '+s)
        return s
    def immediate(s, bits):
        try:
            expression = value(s)
            # value() admits only numeric arithmetic after symbol substitution.
            result = eval(expression, {"__builtins__": {}}, {})
            return isinstance(result, int) and 0 <= result < (1 << bits)
        except (ValueError, SyntaxError, NameError, TypeError):
            return False
    byte_regs = set('abcdehl')
    pairs = {'bc', 'de', 'hl'}
    conditions = {'z', 'nz', 'c', 'nc'}
    labels = {x for kind, x in items if kind == 'label'}
    def absolute_memory(x, high=False):
        if not (x.startswith('[') and x.endswith(']')): return False
        try:
            address = eval(value(x[1:-1]), {"__builtins__": {}}, {})
        except (ValueError, SyntaxError, NameError, TypeError):
            return False
        # Isolated fixture has RAM only here; hardware/ROM writes are unsupported.
        return (0xff80 <= address < 0xffff) or (not high and 0xc000 <= address < 0xe000)
    def byte_source(x):
        return x in byte_regs or x == '[hl]' or immediate(x, 8)
    def validate(op, args, line):
        valid = False
        if op in ('ld', 'ldh') and len(args) == 2:
            d, v = args
            if op == 'ldh':
                valid = ((d == 'a' and (v == '[c]' or absolute_memory(v, True))) or
                         (v == 'a' and (d == '[c]' or absolute_memory(d, True))))
            else:
                valid = ((d in byte_regs and (v in byte_regs or v == '[hl]' or immediate(v, 8))) or
                         (d == '[hl]' and (v in byte_regs or immediate(v, 8))) or
                         (d in pairs and immediate(v, 16)) or
                         (d == 'a' and (v in ('[hli]', '[hld]', '[c]') or absolute_memory(v))) or
                         (v == 'a' and (d in ('[hli]', '[hld]', '[c]') or absolute_memory(d))))
        elif op in ('push', 'pop'):
            valid = len(args) == 1 and args[0] in pairs
        elif op in ('call', 'jp', 'jr'):
            valid = (len(args) in (1, 2) and args[-1] in labels | {'Crash'} and
                     (len(args) == 1 or args[0] in conditions))
            # Calls to local functions are supported only unconditionally.
            if op == 'call' and len(args) == 2: valid = False
        elif op == 'ret':
            valid = args == [''] or (len(args) == 1 and args[0] in conditions)
        elif op in ('inc', 'dec'):
            valid = len(args) == 1 and args[0] in byte_regs | pairs | {'[hl]'}
        elif op in ('and', 'xor', 'or', 'add', 'adc', 'sub', 'cp'):
            valid = ((len(args) == 1 and byte_source(args[0])) or
                     (op in ('add', 'adc', 'sub', 'cp') and len(args) == 2 and
                      args[0] == 'a' and byte_source(args[1])))
            if op == 'add' and args == ['hl', 'hl']: valid = True
        elif op == 'rra':
            valid = args == ['']
        elif op in ('rl', 'sla'):
            valid = len(args) == 1 and args[0] in byte_regs | {'[hl]'}
        if not valid: raise ValueError('Unsupported instruction or operand form: ' + line)
    def read(s):
        if s in 'abcdehl' and len(s)==1: return 's->'+s
        if s in ('bc','de','hl'): return 'PAIR('+s[0]+','+s[1]+')'
        if s.startswith('['):
            x=s[1:-1]
            if x=='c': return 's->m[0xff00+s->c]'
            if x in ('hl','hli','hld'): return 's->m[PAIR(h,l)]'
            return 's->m['+value(x)+']'
        return value(s)
    def write(d,v):
        if d in ('bc','de','hl'): return 'SET('+d[0]+','+d[1]+','+v+');'
        return read(d)+' = (uint8_t)('+v+');'
    def cond(c): return {'z':'s->z','nz':'!s->z','c':'s->cy','nc':'!s->cy'}[c]
    out=['''#include <stdint.h>
#include <string.h>
typedef struct { uint8_t a,b,c,d,e,h,l,z,cy,n,hf; uint16_t sp; uint8_t m[65536]; int fault; } State;
#define PAIR(h,l) ((uint16_t)((s->h<<8)|s->l))
#define SET(h,l,v) do { uint16_t vv=(uint16_t)(v); s->h=vv>>8; s->l=vv; } while(0)
static void push(State*s,uint16_t v){s->m[--s->sp]=v>>8;s->m[--s->sp]=v;}
static uint16_t pop(State*s){uint16_t v=s->m[s->sp++];return v|(s->m[s->sp++]<<8);}
''']
    out += ['static void '+ident(g)+'(State*s);' for g in globals_]
    opened=False
    for kind,line in items:
        if kind=='label':
            if line in globals_:
                if opened: out.append('}')
                out.append('static void '+ident(line)+'(State*s){'); opened=True
            else: out.append(ident(line)+':;')
            continue
        op,_,arg=line.partition(' '); args=[a.strip() for a in arg.split(',')]
        validate(op, args, line)
        code=''
        if op in ('ld','ldh'):
            d,v=args; code=write(d,read(v))
            if '[hli]' in args: code+=' SET(h,l,PAIR(h,l)+1);'
            if '[hld]' in args: code+=' SET(h,l,PAIR(h,l)-1);'
        elif op in ('push','pop'):
            p=args[0]
            if p not in ('hl','de','bc'): raise ValueError(line)
            code='push(s,PAIR('+p[0]+','+p[1]+'));' if op=='push' else 'SET('+p[0]+','+p[1]+',pop(s));'
        elif op in ('call','jp','jr'):
            target=args[-1]
            if target=='Crash': code='s->fault=1; return;'
            elif op=='call': code=ident(target)+'(s); if(s->fault) return;'
            elif '.' not in target: code=ident(target)+'(s); return;'
            else: code='goto '+ident(target)+';'
            if len(args)==2: code='if('+cond(args[0])+'){'+code+'}'
        elif op=='ret': code=('if('+cond(arg)+') ' if arg else '')+'return;'
        elif op in ('inc','dec'):
            x=args[0]
            if x in ('hl','de','bc'): code=write(x,read(x)+(' + 1' if op=='inc' else ' - 1'))
            else:
                sign='+' if op=='inc' else '-'
                code='{uint8_t old='+read(x)+'; '+write(x,'old '+sign+' 1')+'s->z=('+read(x)+'==0); s->n='+str(int(op=='dec'))+'; s->hf=((old&15)=='+('15' if op=='inc' else '0')+');}'
        elif op in ('and','xor','or'):
            v=read(args[-1]); operator={'and':'&','xor':'^','or':'|'}[op]
            code='s->a '+operator+'= '+v+';s->z=s->a==0;s->cy=0;s->n=0;s->hf='+str(int(op=='and'))+';'
        elif op=='add' and args==['hl','hl']:
            code='{uint32_t v=PAIR(h,l);s->cy=(v+v)>65535;s->hf=((v&4095)*2)>4095;s->n=0;SET(h,l,v+v);}'
        elif op in ('add','adc','sub','cp'):
            v=read(args[-1]); carry='s->cy' if op=='adc' else '0'; minus=op in ('sub','cp')
            code='{uint16_t operand=('+v+');uint16_t carry='+carry+';uint16_t rhs=operand+carry;uint16_t old=s->a;uint16_t v=old'+('-' if minus else '+')+'rhs;s->cy='+('old<rhs' if minus else 'v>255')+';s->hf='+('(old&15)<(rhs&15)' if minus else '((old&15)+(operand&15)+carry)>15')+';s->n='+str(int(minus))+';s->z=(uint8_t)v==0;'+('' if op=='cp' else 's->a=v;')+'}'
        elif op in ('rra','rl','sla'):
            x='a' if op=='rra' else arg; v=read(x)
            code='{uint8_t old='+v+';uint8_t cy=s->cy;'+write(x,('(old>>1)|(cy<<7)' if op=='rra' else '(old<<1)|'+('cy' if op=='rl' else '0')))+'s->cy='+('old&1' if op=='rra' else 'old>>7')+';s->z='+('0' if op=='rra' else read(x)+'==0')+';s->n=s->hf=0;}'
        else: raise ValueError('Unsupported instruction: '+line)
        out.append('  '+code+' /* '+line+' */')
    out += ['}', '''void lifted(State*s,int entry){switch(entry){case 0:DamagePass1(s);if(s->fault)return;DamagePass2(s);if(s->fault)return;DamagePass3(s);if(s->fault)return;DamagePass4(s);break;case 1:Multiply(s);break;case 2:Divide(s);break;case 3:DamagePass4(s);break;}}
''']
    return '\n'.join(out)

def build(root,build):
    build.mkdir(parents=True,exist_ok=True)
    source,hram=extract(root)
    # Exact UNION source controls actual aliases; isolated addresses are fixture-only.
    asm='DEF ERR_DIV_ZERO EQU 1\nSECTION "math ram", HRAM[$ffa0]\n'+hram+'\nSECTION "damage", WRAM0[$c000]\nwCurDamage: ds 2\nSECTION "code", ROM0[$100]\nEntry: call DamagePass1\ncall DamagePass2\ncall DamagePass3\ncall DamagePass4\nret\n'
    asm+='\n'.join(line.replace('farcall ', 'call ').replace('farjp ', 'jp ').replace('jmp ', 'jp ') for line in source.splitlines())+'\nCrash: halt\n'
    (build/'fixture.asm').write_text(asm)
    subprocess.run(['rgbasm','-o',str(build/'fixture.o'),str(build/'fixture.asm')],check=True)
    subprocess.run(['rgblink','-o',str(build/'fixture.gb'),'-n',str(build/'fixture.sym'),str(build/'fixture.o')],check=True)
    symbols={}
    for line in (build/'fixture.sym').read_text().splitlines():
        if re.match(r'\w\w:\w{4} ',line):
            addr,name=line.split(); symbols[name]=int(addr.split(':')[1],16)
    symbols['ERR_DIV_ZERO']=1
    (build/'lifted.c').write_text(generate(source,symbols))
    subprocess.run(['cc','-std=c11','-O3','-shared','-fPIC','-o',str(build/'lifted.dylib'),str(build/'lifted.c')],check=True)
    manifest={'sources':{p:hashlib.sha256((root/p).read_bytes()).hexdigest() for p in SOURCES},'symbols':symbols,'normalization':'farcall/farjp/jmp become direct call/jp; bank effects excluded; C call stack abstracts return stack; register pushes exact'}
    (build/'manifest.json').write_text(json.dumps(manifest,indent=2)+'\n')
    return source,symbols

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('--source-root',type=pathlib.Path,default=ROOT);p.add_argument('--build-dir',type=pathlib.Path,required=True);a=p.parse_args();build(a.source_root,a.build_dir)
