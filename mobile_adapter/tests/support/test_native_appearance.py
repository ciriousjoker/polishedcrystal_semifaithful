"""Focused host normalization checks, never enabled/native acceptance."""
from pathlib import Path
import hashlib
import json
import os
import subprocess
import tempfile
import unittest

SUPPORT = Path(__file__).resolve().parent
SEALED = Path(os.environ.get("NATIVE_APPEARANCE_114", "/Volumes/Storage/Cache/mobileadapter/m2-disabled-native-reference-114-20261014/disabled-native-references"))
MANIFEST = "438605687f76cd63028994542f83909cf7949e00128e061b240841fb058a5229"
REFERENCES = [
    "ecf683747893d2ce4e2b24f77967e032e5746cb6ad2b255ea292ef67afe05c99",
    "a5dca2fab99da3afef7a0f9dbb84dbfb4614812246cf6eb8341ebd877394d589",
    "56dd8245cfb3238e8be84e4823295fb127aefe43c197799d2577b794af0ae968",
    "7839483c6ac9ac90a285131b6ed15301b07eba59e4e9c832ad9cfa2619b62cb0",
]


class NativeAppearanceTests(unittest.TestCase):
    def test_standalone_rust_properties(self):
        with tempfile.TemporaryDirectory() as tmp:
            exe = Path(tmp) / "properties"
            subprocess.run(["rustc", "--edition=2021", "--test", str(SUPPORT / "native_appearance.rs"), "-o", str(exe)], check=True)
            subprocess.run([str(exe)], check=True)

    @unittest.skipUnless((SEALED / "sealed-reference-manifest.json").exists(), "external sealed114 evidence unavailable")
    def test_exact_actual_114_exports_and_authentication(self):
        manifest = (SEALED / "sealed-reference-manifest.json").read_bytes()
        self.assertEqual(hashlib.sha256(manifest).hexdigest(), MANIFEST)
        parsed = json.loads(manifest)
        self.assertEqual(parsed["scope"], "disabled-native-four-standing-count4-hour12")
        for role in range(4):
            data = (SEALED / str(role) / "reference.nrf").read_bytes()
            self.assertEqual(len(data), 1016)
            self.assertEqual(hashlib.sha256(data).hexdigest(), REFERENCES[role])
            self.assertEqual(parsed["roles"][role]["reference_sha256"], REFERENCES[role])
            self.assertEqual(parsed["roles"][role]["actual_outputs"], 16)
        source = r'''
#[path = MODULE] mod native_appearance;
use native_appearance::*;
fn main() {
    let directory=std::path::PathBuf::from(std::env::args().nth(1).unwrap());
    let manifest=std::fs::read(directory.join("sealed-reference-manifest.json")).unwrap();
    let refs: [Vec<u8>;4]=std::array::from_fn(|r|std::fs::read(directory.join(format!("{r}/reference.nrf"))).unwrap());
    let slices=refs.each_ref().map(Vec::as_slice);
    let sealed=SealedReferences::from_114(2,&manifest,slices).unwrap();
    for receiver in 0..4 { for avatar in 0..4 {
        let key=Key::stationary(receiver,avatar,ReceiverLighting::GameCornerDay12).unwrap();
        let a=sealed.get(key);
        assert_eq!(a.key().receiver(),receiver);assert_eq!(a.key().avatar(),avatar);
        assert_eq!(a.key().lighting(),ReceiverLighting::GameCornerDay12);
        assert_eq!(a.sprite(),[1,4,199,209][avatar as usize]);
        assert_eq!(a.canonical_palette(),avatar);
        assert_eq!(a.parts().map(|p|(p.dx,p.dy)),[(0,0),(8,0),(0,8),(8,8)]);
        assert_eq!(a.manifest_sha256(),MANIFEST_SHA256);
        assert_eq!(a.reference_sha256(),REFERENCE_SHA256[receiver as usize]);
        assert_eq!(a.original().map(|p|p.oam_index),std::array::from_fn(|n|39-a.actor()*4-n as u8));
        assert!(a.matches(key,a.parts()));
        let other=Key::stationary((receiver+1)%4,avatar,ReceiverLighting::GameCornerDay12).unwrap();
        assert!(!a.matches(other,a.parts()));
        // Preserve every original byte, including transparent constituents.
        for n in 0..4 {
            let at=488+(a.actor() as usize*4+n)*28;
            assert_eq!(&a.parts()[n].tile16,&refs[receiver as usize][at+4..at+20]);
            assert_eq!(&a.parts()[n].rgb8,&refs[receiver as usize][at+20..at+28]);
            assert_eq!(a.parts()[n].flags,refs[receiver as usize][at+3]&0xf0);
        }
    }}
    // Exhaustive single-byte export corruption cannot acquire authority.
    for role in 0..4 { for at in 0..1016 {
        let mut changed=refs.clone();changed[role][at]^=1;
        assert_eq!(SealedReferences::from_114(2,&manifest,changed.each_ref().map(Vec::as_slice)).unwrap_err(),Error::Reference);
    }}
    let mut wrong_manifest=manifest.clone();wrong_manifest[0]^=1;
    assert_eq!(SealedReferences::from_114(2,&wrong_manifest,slices).unwrap_err(),Error::Manifest);
    let mut swap=slices;swap.swap(0,1);
    assert_eq!(SealedReferences::from_114(2,&manifest,swap).unwrap_err(),Error::Reference);
    let mut short=slices;short[0]=&refs[0][..1015];
    assert_eq!(SealedReferences::from_114(2,&manifest,short).unwrap_err(),Error::Length);
    println!("sealed114 roles=4 appearances=16 original_parts=64 mutations=4064 PASS");
}
'''.replace("MODULE", json.dumps(str(SUPPORT / "native_appearance.rs")))
        with tempfile.TemporaryDirectory() as tmp:
            rust = Path(tmp) / "sealed.rs"
            exe = Path(tmp) / "sealed"
            rust.write_text(source)
            subprocess.run(["rustc", "--edition=2021", str(rust), "-o", str(exe)], check=True)
            subprocess.run([str(exe), str(SEALED)], check=True)


if __name__ == "__main__":
    unittest.main()
