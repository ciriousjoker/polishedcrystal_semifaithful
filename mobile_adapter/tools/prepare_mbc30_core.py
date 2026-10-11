#!/usr/bin/env python3
"""Install the checksum-pinned user core and build read-only test observers.

Uses the supplied archive; does not replace RetroArch's global core installation.
Release runs use the delivered universal library. Tests use separate native builds
with debugger observations only, preserving the fork's mapper/runtime behavior.
"""
import argparse
import hashlib
from pathlib import Path
import shutil
import subprocess
import zipfile
from core_runtime import (CORE_HOME, CORE_SOURCE, CORE_SHA256, SOURCE_SHA256,
                          RELEASE_CORE, OBSERVED_CORE, PROFILE_CORE, CPU_SHA256)
from observe_core_palettes import prepare
from probe_emulator import require


def digest(data):
    return hashlib.sha256(data).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--zip', type=Path, default=Path.home() / 'Downloads/DoubleCherryGB-mbc30-macos-universal.zip')
    parser.add_argument('--profile', action='store_true', help='Also build instruction/cadence observer')
    args = parser.parse_args()
    CORE_HOME.mkdir(parents=True, exist_ok=True)
    archive = RELEASE_CORE.parent / 'source.tar.gz'
    if not RELEASE_CORE.exists() or not archive.exists():
        with zipfile.ZipFile(args.zip) as package:
            for name, target, checksum in (
                ('DoubleCherryGB_libretro.dylib', RELEASE_CORE, CORE_SHA256),
                ('source.tar.gz', archive, SOURCE_SHA256)):
                data = package.read('DoubleCherryGB-mbc30-macos-universal/' + name)
                require(digest(data) == checksum, 'Supplied MBC30 archive checksum differs: ' + name)
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_bytes(data)
            for name in ('LICENSE', 'libmobile-license/COPYING', 'libmobile-license/COPYING.LESSER'):
                target = RELEASE_CORE.parent / name
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_bytes(package.read('DoubleCherryGB-mbc30-macos-universal/' + name))
    require(digest(RELEASE_CORE.read_bytes()) == CORE_SHA256, 'Installed release core checksum differs')
    require(digest(archive.read_bytes()) == SOURCE_SHA256, 'Installed source archive checksum differs')
    if not CORE_SOURCE.exists():
        # Extraction is allowed only for this exact, checksum-pinned source archive.
        subprocess.run(['tar', '-xzf', str(archive), '-C', str(CORE_HOME)], check=True)
    require(digest((CORE_SOURCE / 'gb_core/cpu.cpp').read_bytes()) == CPU_SHA256,
            'Pinned source CPU changed; restore the supplied source archive')
    prepare(CORE_SOURCE)

    def build(source, directory):
        subprocess.run(['cmake', '--fresh', '-S', str(source), '-B', str(directory),
            '-DCMAKE_BUILD_TYPE=Release', '-DLIBMOBILE_BUILD_SHARED=OFF',
            '-DDCGB_BUILD_TESTS=ON'], check=True)
        subprocess.run(['cmake', '--build', str(directory), '--parallel', '4'], check=True)
        subprocess.run(['ctest', '--test-dir', str(directory), '--output-on-failure'], check=True)

    build(CORE_SOURCE, OBSERVED_CORE.parent)
    if args.profile:
        source = CORE_HOME / 'source-profile'
        if not source.exists():
            subprocess.run(['python3', str(Path(__file__).with_name('prepare_profile_core.py')),
                            '--source', str(CORE_SOURCE), '--output', str(source)], check=True)
        shutil.copyfile(Path(__file__).with_name('profile_core.h'), source / 'gb_core/profile_core.h')
        build(source, PROFILE_CORE.parent)
    print('MBC30 release core: ' + str(RELEASE_CORE))
    print('MBC30 observed core: ' + str(OBSERVED_CORE))


if __name__ == '__main__':
    main()
