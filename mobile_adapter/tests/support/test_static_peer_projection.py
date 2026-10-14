"""Finite pure projection checks. No enabled, cadence or native acceptance claim."""
from pathlib import Path
import os
import subprocess
import tempfile
import unittest

HERE = Path(__file__).resolve().parent
SEALED = Path(os.environ.get("NATIVE_APPEARANCE_114", "/Volumes/Storage/Cache/mobileadapter/m2-disabled-native-reference-114-20261014/disabled-native-references"))

class ProjectionTests(unittest.TestCase):
    @unittest.skipUnless((SEALED / "sealed-reference-manifest.json").exists(), "sealed114 evidence absent")
    def test_pure_actual_dictionary_projection(self):
        source = r'''
#[path="NATIVE"] mod native_appearance;
#[path="INTENT"] mod static_ordinary_intent;
#[path="PROJECT"] mod static_peer_projection;
use static_peer_projection::*;
fn main() {
 let dir=std::path::PathBuf::from(std::env::args().nth(1).unwrap());
 let manifest=std::fs::read(dir.join("sealed-reference-manifest.json")).unwrap();
 let refs:[Vec<u8>;4]=std::array::from_fn(|r|std::fs::read(dir.join(format!("{r}/reference.nrf"))).unwrap());
 let sealed=native_appearance::SealedReferences::from_114(2,&manifest,refs.each_ref().map(Vec::as_slice)).unwrap();
 let mut raw=[0u8;444]; raw[..34].copy_from_slice(&refs[0][44..78]);
 raw[442..444].copy_from_slice(&refs[0][486..488]);
 raw[16..20].copy_from_slice(&[12,8,12,8]);raw[7]=255;raw[8]=0;
 // Synthetic fixture is an explicit typed-input unit witness, not live authority.
 let scope=IndependentLocalScope { source:1,observed:2,map:[11,23],instance:123,
 evidence:LocalScopeEvidence::FrozenMapPreimage {preimage_sha256:[1;32]} };
 let input=Input { source:Source { id:1,entry:1,returned:3,closed:4,original:[raw;3],operands:[[8,4,0xa5,0,0,0];3]},
 local_identity:(1,1,1),accepted:Some(Accepted {id:1,lifetime:1,transaction:1,captured:5,committed:6,receiver_id:1,receiver_generation:1,peers:vec![]}),
 opportunity:1,opportunity_instruction:7,receiver_role:0,local_avatar:0,local_scope:Some(scope),
 stationary:Some(ValidatedStationaryEvidence {raw_step_flags:0xa5,native_sample_sha256:[3;32]}) };
 let fail=|i:Input,e:Error|assert_eq!(project(&sealed,i).unwrap_err(),e);
 let p=project(&sealed,input.clone()).unwrap(); assert_eq!(p.figures.len(),1);
 assert_eq!((p.figures[0].parts[0].x,p.figures[0].parts[0].y),(72,76));
 for duration in [0,1,255] {let mut i=input.clone();for r in &mut i.source.original {r[10]=duration;}assert!(project(&sealed,i).is_ok());}
 let mut i=input.clone();i.local_scope=None;i.accepted=None;i.local_identity=(1,0,0);assert_eq!(project(&sealed,i).unwrap().figures.len(),1);
 let mut i=input.clone();i.stationary=None;fail(i,Error::MissingStationaryEvidence);
 let mut i=input.clone();i.source.operands[2][2]=0;fail(i,Error::Unstable);
 let mut i=input.clone();for r in &mut i.source.original {r[7]=0;}fail(i,Error::Pose);
 let mut i=input.clone();i.opportunity_instruction=6;fail(i,Error::Chronology);
 let mut peer=[0u8;20];peer[1]=2;peer[3]=1;peer[5]=9;peer[6..8].copy_from_slice(&[11,23]);
 peer[9]=123;peer[11]=1;peer[12..14].copy_from_slice(&144u16.to_be_bytes());peer[14..16].copy_from_slice(&64u16.to_be_bytes());peer[17]=1;peer[18]=1;
 let mut i=input.clone();i.accepted.as_mut().unwrap().peers.push(peer);
 let p=project(&sealed,i.clone()).unwrap();assert_eq!(p.figures.len(),2);assert_eq!(p.figures[1].parts[0].x,88);
 assert_eq!(p.provenance.accepted.as_ref().unwrap().peers[0],peer);
 let appearance=sealed.get(native_appearance::Key::stationary(0,1,native_appearance::ReceiverLighting::GameCornerDay12).unwrap());
 for n in 0..4 {assert_eq!(p.figures[1].parts[n].tile,appearance.parts()[n].tile16);assert_eq!(p.figures[1].parts[n].rgb,appearance.parts()[n].rgb8);assert_eq!(p.figures[1].parts[n].flags,appearance.parts()[n].flags);}
 for at in [6,8,17] {let mut j=i.clone();j.accepted.as_mut().unwrap().peers[0][at]^=1;assert_eq!(project(&sealed,j).unwrap().figures.len(),1);}
 let mut j=i.clone();j.local_scope=None;fail(j,Error::MissingLocalScope);
 let mut j=i.clone();j.accepted.as_mut().unwrap().peers[0][10]=7;assert_eq!(project(&sealed,j).unwrap().figures.len(),2);
 let mut j=i.clone();j.accepted.as_mut().unwrap().peers.push(peer);fail(j,Error::Identity);
 let mut j=i.clone();j.accepted.as_mut().unwrap().peers[0][19]=1;fail(j,Error::Pose);
 for x in [0u16,47,209,4000] {let mut j=i.clone();j.accepted.as_mut().unwrap().peers[0][12..14].copy_from_slice(&x.to_be_bytes());fail(j,Error::Clipped);}
 for x in [64u16,208] {let mut j=i.clone();j.accepted.as_mut().unwrap().peers[0][12..14].copy_from_slice(&x.to_be_bytes());assert!(project(&sealed,j).is_ok());}
 // Negative camera origin uses signed arithmetic, never u8 wrapping.
 let mut j=i.clone();for c in &mut j.source.operands {c[0]=0;}for r in &mut j.source.original {r[16]=4;r[18]=4;}
 assert_eq!(project(&sealed,j).unwrap_err(),Error::Clipped);
 println!("pure sealed114 projection checks PASS; local scope and raw flags remain external prerequisites");
}
'''
        for token, name in [("NATIVE", "native_appearance.rs"), ("INTENT", "static_ordinary_intent.rs"), ("PROJECT", "static_peer_projection.rs")]:
            source = source.replace(token, str(HERE / name))
        with tempfile.TemporaryDirectory() as tmp:
            rust = Path(tmp) / "projection.rs"
            exe = Path(tmp) / "projection"
            rust.write_text(source)
            subprocess.run(["rustc", "--edition=2021", "-Awarnings", str(rust), "-o", str(exe)], check=True)
            subprocess.run([str(exe), str(SEALED)], check=True)

if __name__ == "__main__":
    unittest.main()
